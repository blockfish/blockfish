#![windows_subsystem = "windows"]

mod controller;
mod controls;
mod resources;
mod util;
mod view;

use block_stacker::{Config as BSConfig, Ruleset};
use blockfish::Config as BFConfig;

use argh::FromArgs;
use thiserror::Error;

static VERSION: &'static str = "DEVELOPMENT BUILD";

// Error handling
#[derive(Debug, Error)]
enum Error {
    #[error("{0}")]
    Sdl(String),
    #[error("failed to load resources")]
    Resources(#[from] resources::ResourceLoadError),
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
            let mut trace: Option<&(dyn std::error::Error + 'static)> = Some(&err);
            while let Some(err) = trace {
                log::error!("{}", err);
                trace = err.source();
            }
            1
        }
    });
}

// Entry point

fn entry(args: Args) -> Result<()> {
    let sdl = sdl2::init().map_err(sdl_error)?;
    let sdl_video = sdl.video().map_err(sdl_error)?;

    let ttf = sdl2::ttf::init().map_err(sdl_error)?;
    let resources = resources::Resources::load(&ttf)?;

    let mut canvas = sdl_video
        .window("Blockfish", view::DEFAULT_SIZE.0, view::DEFAULT_SIZE.1)
        .position_centered()
        .build()
        .map_err(sdl_error)?
        .into_canvas()
        .present_vsync()
        .build()
        .map_err(sdl_error)?;

    let mut events = sdl.event_pump().map_err(sdl_error)?;
    let texture_creator = canvas.texture_creator();

    let rules = std::rc::Rc::new(Ruleset::guideline());
    let mut ctl = controller::Controller::new(
        block_stacker::Stacker::new(rules.clone(), args.game_config()),
        view::View::new(resources, rules, controls::Controls::default(), VERSION),
        args.ai_config(),
    );

    loop {
        for evt in events.poll_iter() {
            match ctl.view().handle(evt) {
                Err(view::Quit) => return Ok(()),
                Ok(Some(action)) => ctl.handle(action),
                Ok(None) => {}
            }
        }

        ctl.poll_engine();
        ctl.view_mut().render_labels(&texture_creator);
        ctl.view().paint(&mut canvas);
        canvas.present();
    }
}
