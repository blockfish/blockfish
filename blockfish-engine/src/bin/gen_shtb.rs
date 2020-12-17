fn main() {
    let guideline = block_stacker::Ruleset::guideline();
    let srs = blockfish::ShapeTable::from_ruleset(&guideline);
    let stdout = std::io::stdout();
    serde_json::to_writer(stdout.lock(), &srs).unwrap();
}
