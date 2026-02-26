# argparse

Command-line argument parser for the [Bats](https://github.com/bats-lang) programming language.

## Features

- Flag arguments (`--verbose`, `-v`)
- String options (`--name value`)
- Positional arguments
- Help text generation
- Parses from raw `/proc/self/cmdline`-style null-separated buffers

## Usage

```bats
#use argparse as AP

val p = $AP.parser_new(name_bv, name_len, desc_bv, desc_len)
val @(p, h_verbose) = $AP.add_flag(p, name_bv, name_len, shortcut, help_bv, help_len)
val @(p, h_file) = $AP.add_string(p, name_bv, name_len, shortcut, help_bv, help_len, false)
val result = $AP.parse(p, cmdline_bv, cmdline_max, argc)
```

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

`unsafe = true` — contains C code for low-level argument buffer parsing. Exposes a safe typed API.
