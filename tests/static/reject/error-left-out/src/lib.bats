#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R

(* A match on a parse error that leaves out an argument that is not an
   int *)
fn describe (e: $AP.parse_error): string =
  case+ e of
  | ~$AP.err_unknown_long(closest) => let val () = $R.option_discard<int>(closest) in "unknown option" end
  | ~$AP.err_unknown_short(_) => "unknown short option"
  | ~$AP.err_range(_) => "out of range"
  | ~$AP.err_exclusive(_) => "exclusive"
  | ~$AP.err_choice(_) => "no such subcommand"
  | ~$AP.err_too_long(_) => "too long"
