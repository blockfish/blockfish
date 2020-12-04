pub mod text_fmt {
    pub fn plural<'a>(n: u32, what: &'a str) -> Plural<'a> {
        Plural(n, what)
    }

    pub struct Plural<'a>(u32, &'a str);
    impl<'a> std::fmt::Display for Plural<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            if self.0 == 1 {
                write!(f, "1 {}", self.1)
            } else {
                write!(f, "{} {}s", self.0, self.1)
            }
        }
    }

    pub fn maybe<'a, T: std::fmt::Display>(v: Option<T>, alt: &'a str) -> Maybe<'a, T> {
        Maybe(v, alt)
    }

    pub struct Maybe<'a, T>(Option<T>, &'a str);
    impl<'a, T: std::fmt::Display> std::fmt::Display for Maybe<'a, T> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self.0.as_ref() {
                Some(x) => write!(f, "{}", x),
                None => write!(f, "{}", self.1),
            }
        }
    }

    pub fn maybe_f32<'a>(v: Option<f32>, alt: &'a str) -> MaybeF32<'a> {
        MaybeF32(v, alt)
    }

    pub struct MaybeF32<'a>(Option<f32>, &'a str);
    impl<'a> std::fmt::Display for MaybeF32<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self.0 {
                Some(x) => write!(f, "{:.3}", x),
                None => write!(f, "{}", self.1),
            }
        }
    }
}
