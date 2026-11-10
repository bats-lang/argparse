# argparse

Command-line argument parser for the [Bats](https://github.com/bats-lang) programming language.

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

`unsafe = false`: every table access is proven in bounds by the types. Argument handles are flat (a spec index with an erased proof of its value kind), so they allocate nothing and need no freeing.
