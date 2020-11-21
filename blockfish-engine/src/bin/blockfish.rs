use std::io::Write as _;

fn main() {
    env_logger::builder()
        .format(|f, rec| writeln!(f, "{}: {}", rec.level(), rec.args()))
        .target(env_logger::Target::Stderr)
        .init();

    std::process::exit(match blockfish::repl() {
        Ok(_) => 0,
        Err(err) => {
            display_error(&err);
            1
        }
    })
}

fn display_error(mut err: &dyn std::error::Error) {
    loop {
        log::error!("{}", err);
        if let Some(src) = err.source() {
            err = src;
        } else {
            break;
        }
    }
}
