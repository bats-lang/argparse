#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R
#use str as S

(* Runs a parser through its whole life and frees everything it made:
   a parse that succeeds (a subcommand, help formatted, the result
   freed), a parse that fails (the error freed) and a parser freed
   unparsed. It runs under valgrind: no block may be lost. Exits 1 on a
   wrong outcome. *)
fn new_parser (): $AP.parser(8, 0) = let
  var pn = @[char][4]('p', 'r', 'o', 'g')
  var ph = @[char][4]('h', 'e', 'l', 'p')
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(pn, 4))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(ph, 4))
  val p = $AP.parser_new(b1, 4, b2, 4)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in p end

(* 0: parsed, get_subcmd want_sub; 1: unknown long option; 2: other *)
fn run {tp:nat | tp <= 8192}{ac:nat | ac <= 64}{na:pos | na <= 1048576}
  (p: $AP.parser(tp, ac), av: &(@[char][na]), na: int na, argc: int, want_sub: int): int = let
  val @(fa, ba) = $A.freeze<byte>($S.from_char_array(av, na))
  val res = $AP.parse(p, ba, na, argc)
  val () = $A.drop<byte>(fa, ba)
  val () = $A.free<byte>($A.thaw<byte>(fa))
in
  case+ res of
  | ~$R.ok(r) => let
      val sub = $AP.get_subcmd(r)
      val hb = $A.alloc<byte>(256)
      val hn = $AP.format_help(r, hb, 256)
      val () = $A.free<byte>(hb)
      val () = $AP.parse_result_free(r)
    in if sub = want_sub && hn > 0 then 0 else 2 end
  | ~$R.err(e) => let
      val k = (case+ e of ~$AP.err_unknown_long(_) => 1 | e2 => let val () = $AP.parse_error_free(e2) in 2 end): int
    in k end
end

implement main0 () = let
  (* prog run: get_subcmd gives the token number of the first positional
     no spec takes, 1 *)
  val p = new_parser()
  var sn = @[char][3]('r', 'u', 'n')
  var sh = @[char][4]('r', 'u', 'n', 's')
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(sn, 3))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(sh, 4))
  val @(p, _) = $AP.add_subcommand(p, b1, 3, b2, 4)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
  val @(p, _) = $AP.add_exclusive_group(p)
  var a1 = @[char][9]('p', 'r', 'o', 'g', '\000', 'r', 'u', 'n', '\000')
  val r1 = run(p, a1, 9, 2, 1)
  (* prog --nope *)
  var a2 = @[char][12]('p', 'r', 'o', 'g', '\000', '-', '-', 'n', 'o', 'p', 'e', '\000')
  val r2 = run(new_parser(), a2, 12, 2, 0)
  val () = $AP.parser_free(new_parser())
  val ok = r1 = 0 && r2 = 1
  val () = (if ok then () else println! ("FAIL: ", r1, " ", r2))
in if ok then () else exit(1) end
