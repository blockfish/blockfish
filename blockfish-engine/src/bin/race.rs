use argh::FromArgs;
use block_stacker::{Config as BSConfig, Ruleset, Stacker};
use blockfish::{Config as BFConfig, StackerExt as _};
use std::io::Write;
use std::sync::{atomic, Arc};
use std::time::{Duration, Instant};
use thiserror::Error;

#[derive(FromArgs)]
/// Headless cheese race simulator for Blockfish.
struct Args {
    #[argh(positional)]
    goal: Option<usize>,
    /// don't display stats until race finishes
    #[argh(switch, short = 'q')]
    quiet: bool,
    /// garbage level, defaults to 9
    #[argh(option, short = 'g')]
    garbage: Option<usize>,
    /// minimum garbage level, defaults to 2
    #[argh(option, short = 'G')]
    min_garbage: Option<usize>,
    /// AI tuning parameters, defaults to "50,10,30,25"
    #[argh(option, short = 'A')]
    ai_params: Option<Parameters>,
}

struct Parameters(Vec<i64>);

#[derive(Debug, Error)]
enum ParseParametersError {
    #[error("{0}")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("expected 3 to 4 parameters")]
    Count,
}

impl std::str::FromStr for Parameters {
    type Err = ParseParametersError;
    fn from_str(s: &str) -> Result<Self, ParseParametersError> {
        let parts = s
            .split(',')
            .map(|s| s.parse())
            .collect::<Result<Vec<_>, _>>()?;
        if (3..=4).contains(&parts.len()) {
            Ok(Parameters(parts))
        } else {
            Err(ParseParametersError::Count)
        }
    }
}

impl Args {
    fn to_game_config(&self) -> BSConfig {
        let mut cfg = BSConfig::default();
        if let Some(h) = self.garbage {
            cfg.garbage.max_height = h;
        }
        if let Some(h) = self.min_garbage {
            cfg.garbage.min_height = h;
        }
        cfg.garbage.total_lines = self.goal;
        cfg
    }

    fn to_ai_config(&self) -> BFConfig {
        let mut cfg = BFConfig::default();
        if let Some(Parameters(params)) = self.ai_params.as_ref() {
            cfg.scoring.row_factor = params[0];
            cfg.scoring.piece_estimate_factor = params[1];
            cfg.scoring.piece_penalty = params[2];
            if let Some(&p) = params.get(3) {
                cfg.search_limit = std::cmp::max(p, 1) as usize * 1_000;
            }
        }
        cfg
    }
}

struct Race {
    ai_cfg: BFConfig,
    stacker: Stacker,
    ds_goal: Option<usize>,
    ds: usize,
    pc: usize,
    start_time: Instant,
}

impl Race {
    fn new(ai_cfg: BFConfig, game_cfg: BSConfig, rules: Ruleset) -> Self {
        let ds_goal = game_cfg.garbage.total_lines;
        let stacker = Stacker::new(rules.into(), game_cfg);
        Self {
            ai_cfg,
            stacker,
            ds_goal,
            ds: 0,
            pc: 0,
            start_time: Instant::now(),
        }
    }

    fn is_done(&self) -> bool {
        if let Some(goal) = self.ds_goal {
            self.ds >= goal
        } else {
            false
        }
    }

    fn step(&mut self) {
        let snapshot = self.stacker.snapshot().expect("no snapshot");
        let mut ai = blockfish::AI::new(self.ai_cfg.clone());
        ai.start(snapshot);
        let best_suggestion = ai
            .into_iter()
            .min_by_key(|s| s.score)
            .expect("no suggestions");
        self.stacker.run(best_suggestion.inputs);
        let (_, ds) = self.stacker.hard_drop();
        self.ds += ds;
        self.pc += 1;
    }

    fn print_stats(&self, w: &mut impl Write, now: Instant, short: bool) {
        let Race {
            pc, ds, ds_goal, ..
        } = self;
        let elapsed = (now - self.start_time).as_secs_f64();
        let pps = (*pc as f64) / elapsed;
        if short {
            write!(w, "{} p, {}", pc, ds).unwrap();
            if let Some(goal) = *ds_goal {
                write!(w, "/{}", goal).unwrap();
            }
            write!(w, "L ds, {:.2} pps ", pps).unwrap();
        } else {
            writeln!(w, "{} pieces", pc).unwrap();
            writeln!(w, "{}L downstack", ds).unwrap();
            writeln!(w, "total time: {:.2}s ({:.2}pps)", elapsed, pps).unwrap();
        }
    }
}

static REFRESH_PERIOD: Duration = Duration::from_millis(500);

fn main() {
    let args = argh::from_env::<Args>();

    // cheese race bot
    let mut race = Race::new(
        args.to_ai_config(),
        args.to_game_config(),
        Ruleset::guideline(),
    );

    // periodic stats updates
    let stderr = std::io::stderr();
    let mut stderr = if args.quiet {
        None
    } else {
        Some(stderr.lock())
    };
    let mut prev_refresh = Instant::now();

    // install ctrl-c signal handler
    let sigint = Arc::new(atomic::AtomicBool::new(false));
    let sigint_handler = {
        let sigint = sigint.clone();
        move || sigint.store(true, atomic::Ordering::Relaxed)
    };
    let sigint_handler_id = unsafe { signal_hook::register(signal_hook::SIGINT, sigint_handler) }
        .expect("failed to register interrupt signal handler");

    // loop until goal is met
    while !race.is_done() {
        race.step();

        if let Some(mut out) = stderr.as_mut() {
            let now = Instant::now();
            if now >= prev_refresh + REFRESH_PERIOD {
                write!(out, "\r").unwrap();
                race.print_stats(&mut out, now, true);
                out.flush().unwrap();
                prev_refresh = now;
            }
        }

        if sigint.load(atomic::Ordering::Relaxed) {
            stderr.take();
            signal_hook::unregister(sigint_handler_id);
            eprint!("\r\n--- caught ctrl-C ---\n\n");
            break;
        }
    }

    // finish stats updates
    if let Some(mut out) = stderr {
        write!(out, "\r                                \r").unwrap();
    }

    // print report
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    race.print_stats(&mut stdout, Instant::now(), false);
}
