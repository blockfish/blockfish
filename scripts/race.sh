#!/usr/bin/sh
cargo run \
      -q --release \
      --manifest-path ./blockfish-engine/Cargo.toml \
      --features race \
      --bin blockfish-race -- \
      "$@"
