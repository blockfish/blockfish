use std::convert::{TryFrom, TryInto};
use thiserror::Error;

/// AI configuration.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Config {
    pub search_limit: usize,
    pub parameters: Parameters,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            search_limit: 50_000,
            parameters: Parameters::default(),
        }
    }
}

/// Evaluation scoring parameters.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Parameters {
    pub row_factor: i64,
    pub piece_estimate_factor: i64,
    pub i_dependency_factor: i64,
    pub piece_penalty: i64,
}

impl Default for Parameters {
    fn default() -> Self {
        Self {
            row_factor: 5,
            piece_estimate_factor: 10,
            i_dependency_factor: 10,
            piece_penalty: 10,
        }
    }
}

// Parsing / printing

#[derive(Debug, Error)]
pub enum ParseConfigError {
    #[error("invalid integer format")]
    Int(#[from] std::num::ParseIntError),
    #[error("invalid score parameters")]
    Parameters(#[from] ParseParametersError),
    #[error("expected '<heap-size>' or '<heap-size>/<score-params>'")]
    Other,
}

impl std::str::FromStr for Config {
    type Err = ParseConfigError;
    fn from_str(s: &str) -> Result<Self, ParseConfigError> {
        let mut ss = s.split('/');
        let search_limit = ss.next().ok_or(ParseConfigError::Other)?;
        let search_limit = search_limit.parse::<usize>()? * 1_000;
        let parameters = match ss.next() {
            Some(s) => s
                .split(',')
                .map(|s| s.parse())
                .collect::<Result<Vec<_>, _>>()?
                .as_slice()
                .try_into()?,
            None => Parameters::default(),
        };
        if ss.next().is_some() {
            Err(ParseConfigError::Other)
        } else {
            Ok(Config {
                search_limit,
                parameters,
            })
        }
    }
}

impl std::fmt::Display for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let search_limit = (self.search_limit + 999) / 1_000;
        write!(f, "{}/", search_limit)?;
        for (i, &v) in self.parameters.to_array().iter().enumerate() {
            if i > 0 {
                f.write_str(",")?;
            }
            write!(f, "{}", v)?;
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
#[error("expected exactly 4 values")]
pub struct ParseParametersError;

impl<'a> TryFrom<&'a [i64]> for Parameters {
    type Error = ParseParametersError;
    fn try_from(vs: &'a [i64]) -> Result<Self, ParseParametersError> {
        match vs {
            [v1, v2, v3, v4] => Ok(Parameters {
                row_factor: *v1,
                piece_estimate_factor: *v2,
                i_dependency_factor: *v3,
                piece_penalty: *v4,
            }),
            _ => Err(ParseParametersError),
        }
    }
}

impl Parameters {
    fn to_array(&self) -> [i64; 4] {
        [
            self.row_factor,
            self.piece_estimate_factor,
            self.i_dependency_factor,
            self.piece_penalty,
        ]
    }
}

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_config() {
        assert_eq!(
            "15".parse::<Config>().unwrap(),
            Config {
                search_limit: 15_000,
                parameters: Parameters::default()
            }
        );
        assert_eq!(
            "15/1,2,3,4".parse::<Config>().unwrap(),
            Config {
                search_limit: 15_000,
                parameters: Parameters {
                    row_factor: 1,
                    piece_estimate_factor: 2,
                    i_dependency_factor: 3,
                    piece_penalty: 4,
                },
            }
        );
    }

    #[test]
    fn test_display_config() {
        assert_eq!(
            format!(
                "{}",
                Config {
                    search_limit: 15_000,
                    parameters: Parameters {
                        row_factor: 1,
                        piece_estimate_factor: 2,
                        i_dependency_factor: 3,
                        piece_penalty: 4,
                    },
                }
            ),
            "15/1,2,3,4"
        );
    }

    #[test]
    fn test_parse_params() {
        let params = Parameters {
            row_factor: 1,
            piece_estimate_factor: 2,
            i_dependency_factor: 3,
            piece_penalty: 4,
        };
        let values = params.to_array();
        assert_eq!(Parameters::try_from(&values[..]).unwrap(), params);
    }
}
