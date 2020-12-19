#!/usr/bin/sh
cargo run -q \
      --manifest-path ./blockfish-engine/Cargo.toml \
      --features race \
      --bin blockfish-race -- \
      "$@"
