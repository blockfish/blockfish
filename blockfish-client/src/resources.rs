use sdl2::ttf::{Font, Sdl2TtfContext};
use thiserror::Error;

// Errors

#[derive(Debug, Error)]
pub enum ResourceLoadError {
    #[error("failed to load a font")]
    Font(#[from] FontLoadError),
}

#[derive(Debug, Error)]
#[error("{0}")]
pub struct FontLoadError(String);

pub type Result<T> = std::result::Result<T, ResourceLoadError>;

// Resources

pub struct Resources<'r> {
    pub hud_font: Font<'r, 'static>,
    pub hud_font_bold: Font<'r, 'static>,
    pub hud_font_small: Font<'r, 'static>,
    pub hud_font_small_bold: Font<'r, 'static>,
    pub progress_font: Font<'r, 'static>,
}

impl<'r> Resources<'r> {
    pub fn load(ttf: &'r Sdl2TtfContext) -> Result<Self> {
        log::info!("building resources from static font data");
        Ok(Resources {
            hud_font: FIRA_CODE_REGULAR.load(ttf, 16)?,
            hud_font_bold: FIRA_CODE_SEMIBOLD.load(ttf, 16)?,
            hud_font_small: FIRA_CODE_REGULAR.load(ttf, 14)?,
            hud_font_small_bold: FIRA_CODE_SEMIBOLD.load(ttf, 14)?,
            progress_font: FIRA_CODE_SEMIBOLD.load(ttf, 28)?,
        })
    }
}

// Fonts

struct FontData<'a> {
    bytes: &'a [u8],
}

static FIRA_CODE_REGULAR: FontData<'static> = FontData {
    bytes: include_bytes!("../../support/fira-code/FiraCode-Regular.ttf"),
};

static FIRA_CODE_SEMIBOLD: FontData<'static> = FontData {
    bytes: include_bytes!("../../support/fira-code/FiraCode-SemiBold.ttf"),
};

impl<'a> FontData<'a> {
    fn load<'ttf>(
        &self,
        ttf: &'ttf Sdl2TtfContext,
        size: u16,
    ) -> std::result::Result<Font<'ttf, 'a>, FontLoadError> {
        let rwops = sdl2::rwops::RWops::from_bytes(self.bytes).expect("bug: 0 bytes");
        ttf.load_font_from_rwops(rwops, size).map_err(FontLoadError)
    }
}
