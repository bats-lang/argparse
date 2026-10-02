#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R

(* A short name is $R.some(c) or $R.none(), not a char code or 0 *)
#pub fn add_verbose {tp:nat | tp + 2 <= 8192}{ac:nat | ac < 64}{ln:agz}{lh:agz}
  (p: $AP.parser(tp, ac), name: !$A.borrow(byte, ln, 1), help: !$A.borrow(byte, lh, 1))
  : @($AP.parser(tp + 2, ac + 1), $AP.arg($AP.bool_val))

implement add_verbose (p, name, help) = $AP.add_flag(p, name, 1, 118, help, 1)
