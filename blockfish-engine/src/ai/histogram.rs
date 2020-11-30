use std::ops::Range;

pub struct Histogram {
    buckets: usize,
    values: Vec<i64>,
}

impl Histogram {
    pub fn new(buckets: usize) -> Self {
        let values = Vec::with_capacity(buckets);
        Histogram { buckets, values }
    }

    pub fn range(&self) -> Range<i64> {
        match (self.values.first(), self.values.last()) {
            (Some(&s), Some(&e)) => s..(e + 1),
            (_, _) => 0..0,
        }
    }

    pub fn bucket_ranges<'a>(&'a self) -> impl Iterator<Item = Range<i64>> + 'a {
        let Range { start, end } = self.range();
        let n = self.buckets as i64;
        (0..n).map(move |i| {
            let s = start + (end - start) * i / n;
            let e = start + (end - start) * (i + 1) / n;
            s..e
        })
    }

    pub fn bucket_counts(&self) -> Vec<(Range<i64>, usize)> {
        let mut buckets = self.bucket_ranges().map(|r| (r, 0)).collect::<Vec<_>>();
        let mut idx = 0;
        for v in self.values.iter() {
            while !buckets[idx].0.contains(v) {
                idx += 1;
            }
            buckets[idx].1 += 1;
        }
        buckets
    }
}

impl Default for Histogram {
    fn default() -> Self {
        Histogram::new(8)
    }
}

impl Extend<i64> for Histogram {
    fn extend<I: IntoIterator<Item = i64>>(&mut self, iter: I) {
        self.values.extend(iter);
        self.values.sort();
    }
}

impl std::iter::FromIterator<i64> for Histogram {
    fn from_iter<T: IntoIterator<Item = i64>>(iter: T) -> Self {
        let mut hi = Self::default();
        hi.extend(iter);
        hi
    }
}

static MAX_STARS: usize = 20;

impl std::fmt::Display for Histogram {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let buckets = self.bucket_counts();
        let max_cnt = buckets.iter().map(|&(_, cnt)| cnt).max().unwrap_or(1);
        let max_cnt = std::cmp::max(max_cnt, MAX_STARS);
        let mut stars = String::with_capacity(MAX_STARS);
        for (r, cnt) in buckets {
            // range notation
            let r = format!("[{},{})", r.start, r.end);
            // stars showing count
            let n_stars = cnt * MAX_STARS / max_cnt;
            stars.clear();
            stars.extend(std::iter::repeat('*').take(n_stars));
            // display it
            writeln!(f, "{:>10}: {} ({})", r, stars, cnt)?;
        }
        Ok(())
    }
}

mod test {
    use super::*;

    #[test]
    fn test_fmt() {
        let mut h = Histogram::new(4);
        h.extend(0..10);
        assert_eq!(
            format!("{}", h).lines().collect::<Vec<_>>(),
            [
                "     [0,2): ** (2)",
                "     [2,5): *** (3)",
                "     [5,7): ** (2)",
                "    [7,10): *** (3)"
            ],
        );
    }

    #[test]
    fn test_fmt_limit_stars() {
        let mut h = Histogram::new(4);
        h.extend(0..100);
        h.extend(std::iter::once(51));
        assert_eq!(
            format!("{}", h).lines().collect::<Vec<_>>(),
            [
                "    [0,25): ******************* (25)",
                "   [25,50): ******************* (25)",
                "   [50,75): ******************** (26)",
                "  [75,100): ******************* (25)"
            ],
        );
    }
}
