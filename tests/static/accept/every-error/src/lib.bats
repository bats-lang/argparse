#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R

(* Every parse error named; a range given as one *)
fn describe (e: $AP.parse_error): string =
  case+ e of
  | ~$AP.err_unknown_long(closest) => let val () = $R.option_discard<int>(closest) in "unknown option" end
  | ~$AP.err_unknown_short(_) => "unknown short option"
  | ~$AP.err_range(_) => "out of range"
  | ~$AP.err_not_int(_) => "not an int"
  | ~$AP.err_exclusive(_) => "exclusive"
  | ~$AP.err_choice(_) => "no such subcommand"
  | ~$AP.err_too_long(_) => "too long"

#pub fn add_jobs {tp:nat | tp + 2 <= 8192}{ac:nat | ac < 64}{ln:agz}{lh:agz}
  (p: $AP.parser(tp, ac), name: !$A.borrow(byte, ln, 1), help: !$A.borrow(byte, lh, 1))
  : @($AP.parser(tp + 2, ac + 1), $AP.arg($AP.int_val))

implement add_jobs (p, name, help) = $AP.add_int(p, name, 1, $R.some(106), help, 1, 1, $AP.IntBetween(1, 8))

fn chosen (r: !$AP.parse_result): int =
  case+ $AP.get_subcmd(r) of
  | ~$R.some(index) => index
  | ~$R.none() => 0
