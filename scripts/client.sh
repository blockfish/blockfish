#!/usr/bin/sh
# export RUST_LOG=blockfish=info
cargo run \
      --manifest-path ./blockfish-client/Cargo.toml \
      --bin blockfish-client -- \
      "$@"
