#![windows_subsystem = "windows"]

mod controller;
mod controls;
mod resources;
mod theme;
mod timer;
mod util;
mod view;

use block_stacker::{Config as BSConfig, Ruleset};
use blockfish::Config as BFConfig;

use argh::FromArgs;
use thiserror::Error;

static VERSION: &'static str = "DEVELOPMENT BUILD";

const DEFAULT_CONFIG_DIR: &str = "./config";

// Error handling
#[derive(Debug, Error)]
enum Error {
    #[error("{0}")]
    Sdl(String),
    #[error("failed to load resources")]
    Resources(#[from] resources::ResourceLoadError),
    #[error("failed to parse {0} config")]
    ParseConfig(&'static str, #[source] serde_json::Error),
}

fn sdl_error(e: impl std::fmt::Display) -> Error {
    Error::Sdl(format!("{}", e))
}

// Args

/// Blockfish GUI client.
#[derive(FromArgs)]
struct Args {
    #[argh(positional)]
    goal: Option<Goal>,
    /// configuration directory
    #[argh(option, short = 'c')]
    config_dir: Option<std::path::PathBuf>,
    /// garbage level, defaults to 9
    #[argh(option, short = 'g')]
    garbage: Option<usize>,
    /// minimum garbage level, defaults to 2
    #[argh(option, short = 'G')]
    min_garbage: Option<usize>,
    /// integer used to seed the random number generator
    #[argh(option, short = 's')]
    seed: Option<u64>,
    /// integer used to seed the random number generator
    #[argh(option, short = 'A')]
    ai_params: Option<BFConfig>,
}

impl Args {
    fn game_config(&self) -> BSConfig {
        let mut cfg = BSConfig::default();
        cfg.prng_seed = self.seed;
        cfg.garbage.total_lines = self.goal.unwrap_or_default().lines();
        if let Some(h) = self.garbage {
            cfg.garbage.max_height = h;
        }
        if let Some(h) = self.min_garbage {
            cfg.garbage.min_height = h;
        }
        cfg
    }

    fn ai_config(&self) -> BFConfig {
        self.ai_params.clone().unwrap_or_default()
    }
}

#[derive(Copy, Clone, Debug)]
enum Goal {
    Infinite,
    Lines(usize),
}

impl Goal {
    fn lines(&self) -> Option<usize> {
        match *self {
            Goal::Infinite => None,
            Goal::Lines(n) => Some(n),
        }
    }
}

impl Default for Goal {
    fn default() -> Self {
        Goal::Lines(100)
    }
}

#[derive(Debug, Error)]
#[error("invalid goal, expected number or 'infinite'")]
struct ParseGoalError;

impl std::str::FromStr for Goal {
    type Err = ParseGoalError;
    fn from_str(s: &str) -> std::result::Result<Self, ParseGoalError> {
        if s == "infinite" {
            Ok(Goal::Infinite)
        } else {
            s.parse()
                .map(|n| Goal::Lines(n))
                .map_err(|_| ParseGoalError)
        }
    }
}

// main

type Result<T> = std::result::Result<T, Error>;

pub fn main() {
    pretty_env_logger::init();
    std::process::exit(match entry(argh::from_env()) {
        Ok(_) => 0,
        Err(err) => {
            error_trace_log(&err);
            #[cfg(feature = "msgbox")]
            error_trace_msgbox(&err);
            1
        }
    });
}

fn error_trace_log(err: &(dyn std::error::Error + 'static)) {
    let mut trace = Some(err);
    while let Some(err) = trace {
        log::error!("{}", err);
        trace = err.source();
    }
}

#[cfg(feature = "msgbox")]
fn error_trace_msgbox(err: &(dyn std::error::Error + 'static)) {
    use std::fmt::Write as _;
    let mut contents = "Error: ".to_string();
    let mut trace = Some(err);
    while let Some(err) = trace {
        let _ = write!(&mut contents, "{}\n", err);
        trace = err.source();
    }
    let _ = msgbox::create("Blockfish crashed", &contents, msgbox::IconType::Error);
}

// Entry point

fn entry(mut args: Args) -> Result<()> {
    // init subsystems
    let sdl = sdl2::init().map_err(sdl_error)?;
    let sdl_video = sdl.video().map_err(sdl_error)?;
    let ttf = sdl2::ttf::init().map_err(sdl_error)?;

    // create window
    let mut canvas = sdl_video
        .window("Blockfish", view::DEFAULT_SIZE.0, view::DEFAULT_SIZE.1)
        .position_centered()
        .build()
        .map_err(sdl_error)?
        .into_canvas()
        .present_vsync()
        .build()
        .map_err(sdl_error)?;

    // load resources
    let resources = resources::Resources::load(&ttf)?;

    // more necessary init
    let mut events = sdl.event_pump().map_err(sdl_error)?;
    let texture_creator = canvas.texture_creator();

    // load config files
    let config_dir = args
        .config_dir
        .take()
        .unwrap_or_else(|| DEFAULT_CONFIG_DIR.into());
    let controls = controls::Controls::from_config_file(&config_dir)?;
    let theme = theme::Theme::from_config_file(&config_dir)?;

    // build ai, game state, view and controller
    let rules = std::rc::Rc::new(Ruleset::guideline());
    let ai = blockfish::ai::AI::new(args.ai_config());
    let stacker = block_stacker::Stacker::new(rules.clone(), args.game_config());
    let view = view::View::new(resources, rules, controls, &theme, VERSION);
    let mut ctl = controller::Controller::new(ai, view, stacker);
    let mut tmr = ctl.view().make_timer();

    loop {
        // poll user input events
        tmr.update();
        for evt in events.poll_iter() {
            match ctl.view().handle(evt, &mut tmr) {
                Err(view::Quit) => return Ok(()),
                Ok(Some(action)) => ctl.handle(action),
                Ok(None) => {}
            }
        }

        // poll time based events
        while let Some(action) = tmr.poll() {
            ctl.handle(action);
        }

        // poll for engine progress
        ctl.poll_engine();

        // render and present
        ctl.view().render_labels(&texture_creator);
        ctl.view().paint(&mut canvas);
        canvas.present();
    }
}

trait FromConfigFile: (for<'de> serde::Deserialize<'de>) + Default {
    fn config_name() -> &'static str;

    fn from_config_file(dir: &std::path::Path) -> Result<Self> {
        let name = Self::config_name();
        let filename = format!("{}.json", name);
        if let Ok(f) = std::fs::File::open(dir.join(filename)) {
            serde_json::from_reader(f).map_err(|e| Error::ParseConfig(name, e))
        } else {
            log::warn!("{} config not found; using default", name);
            Ok(Self::default())
        }
    }
}

impl FromConfigFile for controls::Controls {
    fn config_name() -> &'static str {
        "controls"
    }
}

impl FromConfigFile for theme::Theme {
    fn config_name() -> &'static str {
        "theme"
    }
}
