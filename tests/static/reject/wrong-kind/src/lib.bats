#include "share/atspre_staload.hats"
#use argparse as AP

(* A flag's handle read as an int. A handle carries its value kind in
   its type, so reading it as another kind is a type error. *)
#pub fn read_flag_as_int (r: !$AP.parse_result, h: $AP.arg($AP.bool_val)): int

implement read_flag_as_int (r, h) = $AP.get_int(r, h)
