fn main() {
    let guideline = block_stacker::Ruleset::guideline();
    let srs = blockfish::ShapeTable::from_ruleset(&guideline);
    println!("{}", serde_json::to_string(&srs).unwrap());
}
