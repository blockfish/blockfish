use std::sync::mpsc;

use protobuf::Message as _;
use thiserror::Error;

use blockfish::protos;

//////////////////////////////////////////////////////////////////////////////////////////

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
        let err = service(res_tx, req_rx).unwrap_or_else(|e| e);
        let _ = err_tx3.send(err);
    });
    err_rx.recv().unwrap()
}

/// Reader thread: deserializes requests from `rdr` into `tx`.
fn reader(mut rdr: impl std::io::Read, tx: mpsc::SyncSender<protos::Request>) -> Result<()> {
    let mut cis = protobuf::CodedInputStream::new(&mut rdr);
    loop {
        let mut req = protos::Request::default();
        cis.merge_message(&mut req)?;
        if tx.send(req).is_err() {
            return Ok(());
        }
    }
}

/// Writer thread: serializes responses from `rx` into `wtr`.
fn writer(mut wtr: impl std::io::Write, rx: mpsc::Receiver<protos::Response>) -> Result<()> {
    let mut cos = protobuf::CodedOutputStream::new(&mut wtr);
    loop {
        let res = match rx.recv() {
            Ok(res) => res,
            Err(_) => return Ok(()),
        };
        res.write_length_delimited_to(&mut cos)?;
        cos.flush()?;
    }
}

/// Service thread: responds to requests.
fn service(
    tx: mpsc::SyncSender<protos::Response>,
    rx: mpsc::Receiver<protos::Request>,
) -> Result<Error> {
    loop {
        // send a greeting
        let mut res = protos::Response::new();
        res.set_greeting(protos::Response_Greeting {
            version: blockfish::version().to_string(),
            motd: "Hello world".to_string(),
            ..Default::default()
        });
        tx.send(res).map_err(|_| Error::EarlyExit)?;

        // get a request
        let req = rx.recv().map_err(|_| Error::EarlyExit)?;
        log::info!("{:?}", req);
    }
}
