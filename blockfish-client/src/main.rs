#![windows_subsystem = "windows"]

mod controller;
mod resources;
mod ruleset;
mod stacker;
mod view;

use sdl2::{event::Event, keyboard::Keycode};
use std::{convert::TryFrom, time::Duration};
use thiserror::Error;

use crate::{controller::Controller, ruleset::Ruleset, view::View};

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

type Result<T> = std::result::Result<T, Error>;

pub fn main() {
    pretty_env_logger::init();
    std::process::exit(match entry() {
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

fn entry() -> Result<()> {
    let sdl = sdl2::init().map_err(Error::Sdl)?;
    let video = sdl.video().map_err(Error::Sdl)?;
    let ttf = sdl2::ttf::init()?;

    let window = video
        .window("Blockfish (v0.3.1)", 900, 600)
        .position_centered()
        .build()?;
    let mut canvas = window.into_canvas().build()?;
    let texture_creator = canvas.texture_creator();

    let rules = Ruleset::guideline();
    let res = resources::Resources::load(&ttf)?;
    let view = View::new(rules.clone(), res, &canvas, &texture_creator);
    let mut ctl = Controller::new(rules, view);

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
