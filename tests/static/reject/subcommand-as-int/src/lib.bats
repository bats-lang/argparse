#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R

(* The subcommand chosen is an option, not an index or ~1 *)
fn chose_none (r: !$AP.parse_result): bool = $AP.get_subcmd(r) = ~1
