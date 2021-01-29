use blockfish::protos;
use std::{
    convert::{Infallible, TryFrom},
    sync::mpsc,
};
use thiserror::Error;

#[derive(Debug, Error)]
enum Error {
    #[error("encountered IO error")]
    Io(#[from] std::io::Error),
    #[error("encountered Protobuf error")]
    Protobuf(#[from] protobuf::error::ProtobufError),
    #[error("unexpectedly quit")]
    EarlyExit,
}

type Result<T> = std::result::Result<T, Error>;

fn main() {
    pretty_env_logger::init();
    let err = entry();
    error_trace(&err);
    std::process::exit(1)
}

/// Logs the error trace.
fn error_trace(err: &(dyn std::error::Error + 'static)) {
    let mut trace = Some(err);
    while let Some(err) = trace {
        log::error!("{}", err);
        trace = err.source();
    }
}

/// Runs the main loop, blocking indefinitely or returning an error.
fn entry() -> Error {
    let (req_tx, req_rx) = mpsc::sync_channel(0);
    let (res_tx, res_rx) = mpsc::sync_channel(0);
    let (err_tx1, err_rx) = mpsc::sync_channel(0);
    let err_tx2 = err_tx1.clone();
    let err_tx3 = err_tx1.clone();
    std::thread::spawn(move || {
        let stdin = std::io::stdin();
        if let Err(err) = reader(stdin.lock(), req_tx) {
            let _ = err_tx1.send(err);
        }
    });
    std::thread::spawn(move || {
        let stdout = std::io::stdout();
        if let Err(err) = writer(stdout.lock(), res_rx) {
            let _ = err_tx2.send(err);
        }
    });
    std::thread::spawn(move || {
        let err = match service(res_tx, req_rx) {
            Ok(impossible) => match impossible {},
            Err(err) => err,
        };
        let _ = err_tx3.send(err);
    });
    err_rx.recv().unwrap()
}

/// Reader thread: deserializes requests from `rdr` into `tx`.
fn reader(mut rdr: impl std::io::Read, tx: mpsc::SyncSender<protos::Request>) -> Result<()> {
    let mut cis = protobuf::CodedInputStream::new(&mut rdr);
    loop {
        let req = cis.read_message()?;
        if tx.send(req).is_err() {
            return Ok(());
        }
    }
}

/// Writer thread: serializes responses from `rx` into `wtr`.
fn writer(mut wtr: impl std::io::Write, rx: mpsc::Receiver<protos::Response>) -> Result<()> {
    loop {
        let res = match rx.recv() {
            Ok(res) => res,
            Err(_) => return Ok(()),
        };
        let mut cos = protobuf::CodedOutputStream::new(&mut wtr);
        cos.write_message_no_tag(&res)?;
        cos.flush()?;
        wtr.flush()?; // https://github.com/stepancheg/rust-protobuf/issues/541
    }
}

struct Service {
    ai: blockfish::ai::AI,
    max_results: usize,
    max_placements: usize,
}

/// Service thread: responds to requests.
fn service(
    tx: mpsc::SyncSender<protos::Response>,
    rx: mpsc::Receiver<protos::Request>,
) -> Result<Infallible> {
    log::debug!("started service thread");

    // running ai instance
    let mut service = Service::new();
    tx.send(service.greeting()?).map_err(|_| Error::EarlyExit)?;

    loop {
        // recv & dispatch requests
        let mut req = rx.recv().map_err(|_| Error::EarlyExit)?;
        if req.has_set_config() {
            service.set_config(req.take_set_config())?;
        } else if req.has_set_ruleset() {
            service.set_ruleset(req.take_set_ruleset())?;
        } else if req.has_analyze() {
            let res_thunk = service.analyze(req.take_analyze())?;
            let tx = tx.clone();
            std::thread::spawn(move || tx.send(res_thunk()));
        } else {
            log::warn!("got an unknown request type");
        }
    }
}

impl Service {
    fn new() -> Self {
        Self {
            ai: blockfish::ai::AI::new(blockfish::Config::default()),
            max_results: std::usize::MAX,
            max_placements: std::usize::MAX,
        }
    }

    // initial greeting message
    fn greeting(&self) -> Result<protos::Response> {
        let mut grt = protos::Response_Greeting::new();
        grt.set_version(blockfish::version().to_string());
        grt.set_motd("Hello world".to_string());
        let mut res = protos::Response::new();
        res.set_greeting(grt);
        Ok(res)
    }

    /// Handles a "set_config" request.
    fn set_config(&mut self, msg: protos::Request_Config) -> Result<()> {
        let cfg = self.ai.config_mut();
        set_if_nonzero(&mut cfg.search_limit, msg.node_limit as usize);
        set_if_nonzero(&mut self.max_results, msg.max_results as usize);
        set_if_nonzero(&mut self.max_placements, msg.max_placements as usize);
        Ok(())
    }

    /// Handles a "set_ruleset" request.
    fn set_ruleset(&mut self, _: protos::Request_Ruleset) -> Result<()> {
        log::warn!("`set_ruleset` is useless currently");
        Ok(())
    }

    /// Handles a "analyze" request. Returns a thunk that, when called, blocks until the
    /// analysis finishes, returning a response.
    fn analyze(
        &mut self,
        msg: protos::Request_Analyze,
    ) -> Result<impl (FnOnce() -> protos::Response) + Send> {
        let id = msg.id;
        let ss = from_snapshot_proto(msg.get_snapshot());
        let count = self.max_results;
        let len = self.max_placements;
        let mut handle = self.ai.analyze(ss);
        Ok(move || {
            handle.wait();
            let mut res = protos::Response::new();
            let mut finished = res.mut_finished();
            finished.id = id;
            to_analysis_proto(handle, count, len, &mut finished);
            res
        })
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Below is all helper functions for converting between `protos::*` types
//////////////////////////////////////////////////////////////////////////////////////////

fn set_if_nonzero(y: &mut usize, x: usize) {
    if x != 0 {
        *y = x;
    }
}

fn from_snapshot_proto(ss: &protos::Snapshot) -> blockfish::ai::Snapshot {
    blockfish::ai::Snapshot {
        hold: ss.hold.chars().next().and_then(color),
        queue: ss.queue.chars().filter_map(color).collect(),
        matrix: matrix(ss.rows.iter().map(|s| s.as_str())),
    }
}

fn color(ch: char) -> Option<blockfish::Color> {
    blockfish::Color::try_from(ch).ok()
}

fn matrix<'a>(row_strs: impl Iterator<Item = &'a str>) -> blockfish::BasicMatrix {
    let mut mat = None;
    for (i, row_str) in row_strs.enumerate() {
        let mat = mat.get_or_insert_with(|| {
            let cols = row_str.chars().count();
            blockfish::BasicMatrix::with_cols(cols as u16)
        });
        for (j, ch) in row_str.chars().enumerate() {
            if blockfish::Color::try_from(ch).is_ok() {
                mat.set((i as u16, j as u16));
            }
        }
    }
    mat.unwrap_or_else(|| blockfish::BasicMatrix::with_cols(10))
}

fn to_analysis_proto(
    analysis: blockfish::ai::Analysis,
    count: usize,
    len: usize,
    out: &mut protos::Response_Analysis,
) {
    let mut move_ids = analysis.all_moves().collect::<Vec<_>>();
    move_ids.sort_by(|&m, &n| analysis.cmp(m, n));
    out.set_suggestions(
        move_ids
            .iter()
            .take(count)
            .map(|&m_id| to_suggestion_proto(&analysis.suggestion(m_id, len)))
            .collect(),
    );
    if let Some(stats) = analysis.stats() {
        out.set_stats(to_stats_proto(&stats));
    }
}

fn to_suggestion_proto(sugg: &blockfish::ai::Suggestion) -> protos::Suggestion {
    let mut proto = protos::Suggestion::new();
    proto.rating = sugg.rating;
    proto.inputs = sugg.inputs.iter().map(|&i| to_input_proto(i)).collect();
    proto
}

fn to_stats_proto(stats: &blockfish::ai::Stats) -> protos::Stats {
    let mut proto = protos::Stats::new();
    proto.nodes = stats.nodes as u64;
    proto.iterations = stats.iterations as u64;
    proto.time_taken_millis = stats.time_taken.as_millis() as u64;
    proto
}

fn to_input_proto(i: blockfish::Input) -> protos::Input {
    match i {
        blockfish::Input::Left => protos::Input::LEFT,
        blockfish::Input::Right => protos::Input::RIGHT,
        blockfish::Input::CW => protos::Input::CW,
        blockfish::Input::CCW => protos::Input::CCW,
        blockfish::Input::Hold => protos::Input::HOLD,
        blockfish::Input::SD => protos::Input::SD,
        blockfish::Input::HD => protos::Input::HD,
    }
}
