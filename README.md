# Blockfish

* Requires: `racket` version 7.6+ (I think)

## Setup

```sh
# (in repository root)
raco pkg install ./blockfish-client-lib
```

## Running

```sh
racket -l blockfish/client
```

To run tests:

```sh
raco test $(find -name '*.rkt')
```
