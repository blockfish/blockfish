use std::{
    convert::TryInto as _,
    io::{BufRead, Write},
};
use thiserror::Error;

use crate::{
    ai::{ai, Snapshot, Suggestion},
    common::{Color, Input},
    matrix::BasicMatrix,
};

// Errors

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error encountered when parsing snapshot")]
    ReadIo(std::io::Error),
    #[error("IO error encountered when sending suggested moves")]
    WriteIo(std::io::Error),
    #[error("invalid snapshot: no pieces in queue")]
    EmptyQueue,
    #[error("invalid snapshot: no rows in matrix")]
    EmptyMatrix,
}

type Result<T> = std::result::Result<T, Error>;

// REPL

pub fn repl() -> Result<()> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let mut parser = SnapshotParser::new();
    let mut writer = SuggestionWriter::new(&mut stdout);
    loop {
        let snap = match parser.parse(&mut stdin.lock())? {
            Some(parsed_snap) => parsed_snap,
            None => break,
        };
        let suggs = ai(snap);
        for sugg in suggs {
            writer.write(sugg)?;
        }
        writer.end()?;
    }
    log::info!("shutting down.");
    Ok(())
}

// Writing

struct SuggestionWriter<'w> {
    writer: &'w mut dyn Write,
    line: String,
}

impl<'w> SuggestionWriter<'w> {
    fn new(writer: &'w mut impl Write) -> Self {
        Self {
            writer,
            line: String::with_capacity(16),
        }
    }

    /// Sends an ASCII specification of suggestion `sugg` to the client.
    fn write(&mut self, sugg: Suggestion) -> Result<()> {
        use std::fmt::Write;
        let inputs = sugg.inputs.into_iter().map(input_to_char);
        let score = sugg.score;
        self.line.clear();
        self.line.extend(inputs);
        writeln!(&mut self.line, ". {}", score).expect("formatting error");
        self.writer
            .write_all(self.line.as_bytes())
            .map_err(Error::WriteIo)?;
        Ok(())
    }

    /// Signals to the client that there are no more suggestions.
    fn end(&mut self) -> Result<()> {
        self.writer.write_all(b"\n").map_err(Error::WriteIo)?;
        self.writer.flush().map_err(Error::WriteIo)?;
        Ok(())
    }
}

fn input_to_char(inp: Input) -> char {
    match inp {
        Input::Left => 'L',
        Input::Right => 'R',
        Input::CW => 'X',
        Input::CCW => 'Z',
        Input::Hold => 'h',
    }
}

// Parsing

struct SnapshotParser {
    /// Line of input read, to be reused after each line is interpreted.
    line: String,
    /// Matrix column row data, to be reused after each row is interpreted.
    row: Vec<Option<Color>>,
}

impl SnapshotParser {
    /// Constructs a new `SnapshotParser`.
    fn new() -> Self {
        Self {
            line: String::with_capacity(16),
            row: Vec::with_capacity(16),
        }
    }

    fn read_line(&mut self, rd: &mut impl BufRead) -> Result<usize> {
        rd.read_line(&mut self.line).map_err(Error::ReadIo)
    }

    /// Parses an ASCII `Snapshot` specification from the given reader. On successful
    /// parse, returns `Ok(Some(parsed_snapshot))`. If end-of-file is encountered while
    /// parsing, returns `Ok(None)`.
    ///
    /// # Arguments
    ///
    /// * `rd` - the input reader.
    fn parse(&mut self, rd: &mut impl BufRead) -> Result<Option<Snapshot>> {
        self.line.clear();
        if self.read_line(rd)? <= 1 {
            return Ok(None);
        }
        let hold = parse_hold(&self.line);

        self.line.clear();
        if self.read_line(rd)? == 0 {
            return Ok(None);
        }
        let mut queue = Vec::with_capacity(8);
        parse_queue(&self.line, &mut queue)?;

        let mut matrix = None;
        loop {
            self.line.clear();
            self.row.clear();
            match self.read_line(rd)? {
                0 => return Ok(None),
                1 => break,
                _ => {
                    parse_matrix_row(&self.line, &mut self.row);
                    let cols = self.row.len() as u16;
                    let matrix = matrix.get_or_insert_with(|| BasicMatrix::with_cols(cols));
                    matrix.push_row(self.row.iter().map(Option::is_some));
                }
            }
        }
        let matrix = matrix.ok_or(Error::EmptyMatrix)?;

        Ok(Some(Snapshot {
            hold,
            queue,
            matrix,
        }))
    }
}

/// Parses an ASCII specification of the hold piece.
fn parse_hold(line: &str) -> Option<Color> {
    line.chars().filter_map(|c| c.try_into().ok()).next()
}

/// Parses an ASCII specification of the next queue. Appends all specified pieces in the
/// queue to the given `Vec`.
fn parse_queue(line: &str, queue: &mut Vec<Color>) -> Result<()> {
    queue.extend(
        line.chars()
            .filter_map(|c| -> Option<Color> { c.try_into().ok() }),
    );
    if queue.is_empty() {
        return Err(Error::EmptyQueue);
    }
    Ok(())
}

/// Parses an ASCII specification of a row of the matrix. Appends cell information to the
/// given `Vec`.
fn parse_matrix_row(line: &str, row: &mut Vec<Option<Color>>) {
    for c in line.chars() {
        if let Ok(color) = c.try_into() {
            row.push(Some(color));
        } else if !c.is_whitespace() {
            row.push(None);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn row_string(mat: &BasicMatrix, i: u16) -> String {
        (0..mat.cols())
            .map(|j| if mat.get((i, j)) { 'x' } else { '.' })
            .collect()
    }

    #[test]
    fn test_parse_snapshots() {
        let mut reader = {
            let mut input = vec![];
            input.extend_from_slice(b".\nLJO\n.....\n\n");
            input.extend_from_slice(b"L\nJOZ\nGGGG..GG\nG...GGG.\n\n");
            std::io::Cursor::new(input)
        };
        let mut parser = SnapshotParser::new();

        let shot = parser.parse(&mut reader).unwrap().unwrap();
        assert_eq!(shot.hold, None);
        assert_eq!(shot.queue, vec![Color('L'), Color('J'), Color('O')]);
        assert_eq!(shot.matrix.cols(), 5);

        let shot = parser.parse(&mut reader).unwrap().unwrap();
        assert_eq!(shot.hold, Some(Color('L')));
        assert_eq!(shot.queue, vec![Color('J'), Color('O'), Color('Z')]);
        assert_eq!(row_string(&shot.matrix, 0), "xxxx..xx");
        assert_eq!(row_string(&shot.matrix, 1), "x...xxx.");

        assert!(parser.parse(&mut reader).unwrap().is_none());
    }
}
