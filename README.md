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
val @(p, h_verbose) = $AP.add_flag(p, name_bv, name_len, $R.some(118) (* -v *), help_bv, help_len)
val @(p, h_file) = $AP.add_string(p, name_bv, name_len, $R.none(), help_bv, help_len, false)
val @(p, h_jobs) = $AP.add_int(p, name_bv, name_len, $R.none(), help_bv, help_len, 1, $AP.IntBetween(1, 8))
val result = $AP.parse(p, cmdline_bv, cmdline_max, argc)
```

A short name is `$R.some(c)` or `$R.none()`; an int's range is `AnyInt()` or
`IntBetween(low, high)`; the subcommand chosen (`get_subcmd`) is an option.
A failure is a `parse_error`: `err_unknown_long` (with the closest option,
if any), `err_unknown_short`, `err_range`, `err_not_int`, `err_exclusive`,
`err_choice` or `err_too_long`.

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

`unsafe = false`: every table access is proven in bounds by the types. Argument handles are flat (a spec index with an erased proof of its value kind), so they allocate nothing and need no freeing.
