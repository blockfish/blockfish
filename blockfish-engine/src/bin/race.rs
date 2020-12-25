use argh::FromArgs;
use block_stacker::{Config as BSConfig, Ruleset, Stacker};
use blockfish::{Config as BFConfig, StackerExt as _};
use serde::Serialize;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{atomic, Arc};
use std::time::{Duration, Instant};

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
    /// parameters to AI, defaults to "25/0,3,4"
    #[argh(option, short = 'A')]
    ai_params: Option<BFConfig>,
    /// integer used to seed the random number generator
    #[argh(option, short = 's')]
    seed: Option<u64>,
    /// display ASCII rendering of game state at the end
    #[argh(switch)]
    ascii: bool,
    /// write a trace of the race to the given file
    #[argh(option)]
    trace_file: Option<PathBuf>,
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
        cfg.prng_seed = self.seed;
        cfg
    }

    fn to_ai_config(&self) -> BFConfig {
        self.ai_params.clone().unwrap_or_default()
    }
}

struct Race {
    ai_cfg: BFConfig,
    stacker: Stacker,
    ds_goal: Option<usize>,
    start_time: Instant,
    trace: Vec<usize>,
}

#[derive(Serialize)]
struct Trace<'a> {
    time: f64,
    seed: u64,
    ds: &'a [usize],
}

impl Race {
    fn new(ai_cfg: BFConfig, game_cfg: BSConfig, rules: Ruleset) -> Self {
        let ds_goal = game_cfg.garbage.total_lines;
        let stacker = Stacker::new(rules.into(), game_cfg);
        Self {
            ai_cfg,
            stacker,
            ds_goal,
            start_time: Instant::now(),
            trace: Vec::with_capacity(ds_goal.unwrap_or(100) * 5),
        }
    }

    fn ds(&self) -> usize {
        self.trace.last().cloned().unwrap_or(0)
    }

    fn pc(&self) -> usize {
        self.trace.len()
    }

    fn won(&self) -> bool {
        if let Some(goal) = self.ds_goal {
            self.ds() >= goal
        } else {
            false
        }
    }

    fn lost(&self) -> bool {
        // no current piece indicates lose
        self.stacker.current_piece_type().is_none()
    }

    fn done(&self) -> bool {
        self.won() || self.lost()
    }

    fn step(&mut self) {
        let snapshot = self.stacker.snapshot().expect("no snapshot");
        let best_suggestion = blockfish::AI::new(self.ai_cfg.clone())
            .analyze(snapshot)
            .min_by_key(|s| s.score)
            .expect("no suggestions");
        self.stacker.run(best_suggestion.inputs);
        let (_, garbage_cleared) = self.stacker.hard_drop();
        self.trace.push(self.ds() + garbage_cleared);
    }

    fn print_stats(&self, w: &mut impl Write, now: Instant, short: bool) -> std::io::Result<()> {
        let elapsed = (now - self.start_time).as_secs_f64();
        let ds = self.ds();
        let pc = self.pc();
        let pps = (pc as f64) / elapsed;
        if short {
            write!(w, "{} p, {}", pc, ds)?;
            if let Some(goal) = self.ds_goal {
                write!(w, "/{}", goal)?;
            }
            write!(w, "L ds, {:.2} pps ", pps)?;
        } else {
            if self.lost() {
                writeln!(w, "topped out early")?;
            }
            writeln!(w, "{} pieces", pc)?;
            writeln!(w, "{}L downstack", ds)?;
            writeln!(w, "total time: {:.2}s ({:.2}pps)", elapsed, pps)?;
            writeln!(w, "PRNG seed: {}", self.stacker.prng_seed())?;
        }
        Ok(())
    }

    fn as_trace(&self) -> Trace {
        let time = (Instant::now() - self.start_time).as_secs_f64();
        let seed = self.stacker.prng_seed();
        Trace {
            time,
            seed,
            ds: &self.trace,
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
    while !race.done() {
        race.step();

        if let Some(mut out) = stderr.as_mut() {
            let now = Instant::now();
            if now >= prev_refresh + REFRESH_PERIOD {
                let _ = write!(out, "\r");
                let _ = race.print_stats(&mut out, now, true);
                let _ = out.flush();
                prev_refresh = now;
            }
        }

        if sigint.load(atomic::Ordering::Relaxed) {
            signal_hook::unregister(sigint_handler_id);
            stderr = None;
            eprint!("\r\n--- caught ctrl-C ---\n\n");
            break;
        }
    }

    // finish stats updates
    if let Some(mut out) = stderr {
        let _ = write!(out, "\r                                \r");
    }

    // print report
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    race.print_stats(&mut stdout, Instant::now(), false)
        .unwrap();
    if args.ascii {
        writeln!(stdout, "\n{:?}", race.stacker).unwrap();
    }

    // render JSON if requested
    if let Some(path) = args.trace_file {
        let file = match std::fs::File::create(path) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("error writing trace file:\n{}", e);
                std::process::exit(1);
            }
        };
        serde_json::to_writer(file, &race.as_trace()).unwrap();
    }

    std::process::exit(if race.won() {
        0
    } else if race.lost() {
        1
    } else {
        // ctrl-c
        130
    });
}
