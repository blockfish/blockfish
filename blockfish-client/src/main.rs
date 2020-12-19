#![windows_subsystem = "windows"]

mod controller;
mod resources;
mod util;
mod view;

use argh::FromArgs;
use block_stacker::{Config as BSConfig, Ruleset, Stacker};
use blockfish::Config as BFConfig;
use sdl2::{event::Event, keyboard::Keycode};
use std::{convert::TryFrom, time::Duration};
use thiserror::Error;

use crate::{controller::Controller, view::View};

static WINDOW_TITLE: &'static str = "Blockfish (v0.4.3)";

// Error handling

#[derive(Debug, Error)]
enum Error {
    #[error("{0}")]
    Sdl(String),
    #[error("failed to load resources")]
    Resources(#[from] resources::ResourceLoadError),
}

impl From<sdl2::video::WindowBuildError> for Error {
    fn from(e: sdl2::video::WindowBuildError) -> Self {
        Error::Sdl(format!("{}", e))
    }
}

impl From<sdl2::IntegerOrSdlError> for Error {
    fn from(e: sdl2::IntegerOrSdlError) -> Self {
        Error::Sdl(format!("{}", e))
    }
}

impl From<sdl2::ttf::InitError> for Error {
    fn from(e: sdl2::ttf::InitError) -> Self {
        Error::Sdl(format!("{}", e))
    }
}

// Args

/// Blockfish GUI client.
#[derive(FromArgs)]
struct Args {
    #[argh(positional)]
    goal: Option<usize>,
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
    fn to_game_config(&self) -> BSConfig {
        let mut cfg = BSConfig::default();
        cfg.prng_seed = self.seed;
        cfg.garbage.total_lines = self.goal;
        if let Some(h) = self.garbage {
            cfg.garbage.max_height = h;
        }
        if let Some(h) = self.min_garbage {
            cfg.garbage.min_height = h;
        }
        cfg
    }

    fn to_ai_config(&self) -> BFConfig {
        self.ai_params.clone().unwrap_or_default()
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
    let sdl = sdl2::init().map_err(Error::Sdl)?;
    let video = sdl.video().map_err(Error::Sdl)?;
    let ttf = sdl2::ttf::init()?;

    let window = video
        .window(WINDOW_TITLE, 900, 600)
        .position_centered()
        .build()?;
    let mut canvas = window.into_canvas().build()?;
    let texture_creator = canvas.texture_creator();
    let res = resources::Resources::load(&ttf)?;

    let rules = Ruleset::guideline().into();
    let stacker = Stacker::new(rules, args.to_game_config());
    let view = View::new(stacker.ruleset().clone(), res, &canvas, &texture_creator);
    let mut ctl = Controller::new(stacker, args.to_ai_config(), view);

    let mut event_pump = sdl.event_pump().map_err(Error::Sdl)?;

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                Event::KeyDown { keycode, .. } => {
                    match keycode.and_then(|c| Input::try_from(c).ok()) {
                        Some(Input::Game(i)) => ctl.on_game_input(i),
                        Some(Input::User(i)) => ctl.on_user_input(i),
                        None => {}
                    }
                }
                _ => {}
            }
        }

        ctl.poll_engine();
        ctl.view().draw(&mut canvas);
        canvas.present();
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }

    Ok(())
}

enum Input {
    Game(blockfish::Input),
    User(controller::UserInput),
}

impl TryFrom<Keycode> for Input {
    type Error = ();

    fn try_from(code: Keycode) -> std::result::Result<Self, ()> {
        match code {
            Keycode::Space => Ok(Input::Game(blockfish::Input::HD)),
            Keycode::Down => Ok(Input::Game(blockfish::Input::SD)),
            Keycode::Left => Ok(Input::Game(blockfish::Input::Left)),
            Keycode::Right => Ok(Input::Game(blockfish::Input::Right)),
            Keycode::Z => Ok(Input::Game(blockfish::Input::CCW)),
            Keycode::X => Ok(Input::Game(blockfish::Input::CW)),
            Keycode::LShift => Ok(Input::Game(blockfish::Input::Hold)),
            Keycode::S => Ok(Input::User(controller::UserInput::NextSuggestion)),
            Keycode::A => Ok(Input::User(controller::UserInput::PrevSuggestion)),
            _ => Err(()),
        }
    }
}
