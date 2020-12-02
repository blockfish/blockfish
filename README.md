# Blockfish

* Requires: `rust` version 1.48 (I think?)
* Requires SDL2 and SDL2_ttf dev libraries.
* Uses Fira Code font (no download required, see `support/fira-code`).

## Download

I will distribute working builds at some point...

## Compiling

This command compiles and runs the GUI client:

```
cargo run --release --bin blockfish-client
```

This command compiles and runs the command line 'cheese race' benchmarker:

```
cargo run --release --bin blockfish-race
```

Mainly tested on Linux, but this should build fine on Windows as long as you have the
`.LIB` files for SDL2 and SDL2_ttf handy.

## Controls

Controls are currently hard-coded in `blockfish-client/src/main.rs`:

- `<left>` - move left
- `<right>` - move right
- `X` - rotate clockwise
- `Z` - rotate counter-clockwise
- `<down>` - sonic drop
- `<space>` - hard drop
- `<shift>` - hold
- `S` - view next engine suggestion
- `A` - view previous engine suggestion
