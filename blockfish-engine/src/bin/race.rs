use block_stacker::{Ruleset, Stacker};
use blockfish::{Config, StackerExt as _};
use std::time::{Duration, Instant};

struct Race {
    ai_config: Config,
    stacker: Stacker,
    ds: usize,
    pc: usize,
}

impl Race {
    fn new(ai_config: Config, rules: std::rc::Rc<Ruleset>) -> Self {
        let stacker = Stacker::new(rules);
        Self {
            ai_config,
            stacker,
            ds: 0,
            pc: 0,
        }
    }

    fn step(&mut self) {
        let snapshot = self.stacker.snapshot().expect("no snapshot");
        let ai = blockfish::ai(self.ai_config.clone(), snapshot);
        let best_suggestion = ai
            .into_iter()
            .min_by_key(|s| s.score)
            .expect("no suggestions");
        self.stacker.run(best_suggestion.inputs);
        let (_, ds) = self.stacker.hard_drop();
        self.ds += ds;
        self.pc += 1;
    }
}

static REFRESH_PERIOD: Duration = Duration::from_millis(500);
static GOAL_DS: usize = 1000;

fn main() {
    // timing
    let start = Instant::now();
    let mut prev_refresh = start;

    // cheese race bot
    let mut race = Race::new(Config::default(), Ruleset::guideline());
    print_stats(&race, None);

    // loop until goal is met
    while race.ds < GOAL_DS {
        race.step();
        let now = Instant::now();
        if prev_refresh + REFRESH_PERIOD <= now {
            prev_refresh = now;
            print!("\r");
            print_stats(&race, Some(now - start));
        }
    }

    let end = Instant::now();
    println!("\r----------------------------------------------------");
    println!("{} pieces", race.pc);
    println!("{} cleared", race.ds);
    println!("total time: {:.2}s", (end - start).as_secs_f64());
}

fn print_stats(race: &Race, time: Option<Duration>) {
    use std::io::Write;
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    write!(stdout, "{} pieces, {}/{}", race.pc, race.ds, GOAL_DS).unwrap();
    if let Some(time) = time {
        let pps = (race.pc as f64) / time.as_secs_f64();
        write!(stdout, ", {:.2} pps", pps).unwrap();
    }

    stdout.flush().unwrap();
}
