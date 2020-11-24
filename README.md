# Blockfish

* Requires: `rust` version 1.48 (I think?)
* Requires SDL2 and SDL2_ttf dev libraries.
* Uses Fira Code font (no download required, see `support/fira-code`).

## Download

I will distribute working builds at some point...

## Compiling

This command builds the GUI client and the CLI REPL (read-eval-print-loop).

```
cargo build --release --all-features
```

The REPL is intended to use to allow integration with other clients, but the protocoly is
currently WIP. For now you can observe the engine by using the standalone client.

```
cargo run --release --bin blockfish-client
```

Mainly tested on Linux, but this should build fine on Windows as long as you have the
`.LIB` files for SDL2 and SDL2_ttf handy.

## Controls

- `<left>` - move left
- `<right>` - move right
- `X` - rotate clockwise
- `Z` - rotate counter-clockwise
- `<down>` - sonic drop
- `<space>` - hard drop
- `<shift>` - hold
- `S` - view next engine suggestion
- `A` - view previous engine suggestion
