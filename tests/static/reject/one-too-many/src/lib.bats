#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R

(* A parser already holding 64 arguments takes one more flag. The
   parser is indexed by its argument count, and adding requires fewer
   than 64, so a 65th argument is a type error, not a silent overwrite
   of the first spec. *)
#pub fn add_one {tp:nat | tp + 2 <= 8192}{ln:agz}{lh:agz}
  (p: $AP.parser(tp, 64), name: !$A.borrow(byte, ln, 1), help: !$A.borrow(byte, lh, 1))
  : @($AP.parser(tp + 2, 64 + 1), $AP.arg($AP.bool_val))

implement add_one (p, name, help) = $AP.add_flag(p, name, 1, $R.none(), help, 1)
