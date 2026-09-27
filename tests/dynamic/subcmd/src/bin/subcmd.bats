#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R
#use str as S

fn _str {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, sc: int, hc: &(@[char][nh]), nh: int nh, pos: bool)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.string_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_string(p, b1, nn, sc, b2, nh, pos)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end
fn _int {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, sc: int, hc: &(@[char][nh]), nh: int nh, d: int, lo: int, hi: int)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.int_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_int(p, b1, nn, sc, b2, nh, d, lo, hi)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end
fn _flag {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, sc: int, hc: &(@[char][nh]), nh: int nh)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.bool_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_flag(p, b1, nn, sc, b2, nh)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end
fn _count {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, sc: int, hc: &(@[char][nh]), nh: int nh)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.count_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_count(p, b1, nn, sc, b2, nh)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end

fun _print {l:agz}{m:pos}{i:nat | i <= m} .<m - i>.
  (buf: !$A.arr(byte, l, m), m: int m, i: int i, n: int): void =
  if i >= m then ()
  else if i >= n then ()
  else let
    val b = byte2int0($A.get<byte>(buf, i))
    val () = (if b = 10 then print! ("\\n") else print! (int2char0(b))): void
  in _print(buf, m, i + 1, n) end

fn _show_str (r: !$AP.parse_result, label: string, h: $AP.arg($AP.string_val)): void = let
  val buf = $A.alloc<byte>(64)
  val n = $AP.get_string_copy(r, h, buf, 64)
  val () = print! (" ", label, "=")
  val () = (if $AP.is_present(r, h) then print! ("") else print! ("(absent)")): void
  val () = _print(buf, 64, 0, (if $AP.is_present(r, h) then n else 0))
  val () = print! ("/", $AP.get_string_len(r, h))
in $A.free<byte>(buf) end

fn _help {m:pos | m <= 1048576} (r: !$AP.parse_result, m: int m): void = let
  val hb = $A.alloc<byte>(m)
  val hn = $AP.format_help(r, hb, m)
  val () = print! ("  help ", hn, " [")
  val () = _print(hb, m, 0, hn)
  val () = println! ("]")
in $A.free<byte>(hb) end

(* Subcommands: the first positional token naming one chooses it, and
   get_subcmd gives the index add_subcommand returned. Prints what each
   argv parses to. *)
fn _sub {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, hc: &(@[char][nh]), nh: int nh)
  : @($AP.parser(tp + nn + nh, ac + 1), int) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_subcommand(p, b1, nn, b2, nh)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end

fn _run {na:pos | na <= 1048576} (label: string, av: &(@[char][na]), na: int na, argc: int, help: int): void = let
  var pn = @[char][4]('p', 'r', 'o', 'g')
  var ph = @[char][4]('t', 'o', 'o', 'l')
  val @(fpn, bpn) = $A.freeze<byte>($S.from_char_array(pn, 4))
  val @(fph, bph) = $A.freeze<byte>($S.from_char_array(ph, 4))
  val p = $AP.parser_new(bpn, 4, bph, 4)
  val () = $A.drop<byte>(fpn, bpn)
  val () = $A.free<byte>($A.thaw<byte>(fpn))
  val () = $A.drop<byte>(fph, bph)
  val () = $A.free<byte>($A.thaw<byte>(fph))
  var n1 = @[char][3]('r', 'u', 'n')
  var h1 = @[char][4]('r', 'u', 'n', 's')
  val @(p, i_run) = _sub(p, n1, 3, h1, 4)
  var n2 = @[char][4]('f', 'i', 'l', 'e')
  var h2 = @[char][5]('i', 'n', 'p', 'u', 't')
  val @(p, hfile) = _str(p, n2, 4, 0, h2, 5, true)
  var n3 = @[char][5]('b', 'u', 'i', 'l', 'd')
  var h3 = @[char][6]('b', 'u', 'i', 'l', 'd', 's')
  val @(p, i_build) = _sub(p, n3, 5, h3, 6)
  var n4 = @[char][7]('v', 'e', 'r', 'b', 'o', 's', 'e')
  var h4 = @[char][6]('c', 'h', 'a', 't', 't', 'y')
  val @(p, hv) = _flag(p, n4, 7, 118, h4, 6)
  val @(fa, ba) = $A.freeze<byte>($S.from_char_array(av, na))
  val res = $AP.parse(p, ba, na, argc)
  val () = $A.drop<byte>(fa, ba)
  val () = $A.free<byte>($A.thaw<byte>(fa))
  val () = print! (label, ":")
in
  case+ res of
  | ~$R.ok(r) => let
      val sub = $AP.get_subcmd(r)
      val () = print! (" sub=", sub)
      val () = (if sub = i_run then print! (" (run)")
                else if sub = i_build then print! (" (build)") else ()): void
      val () = _show_str(r, "file", hfile)
      val () = (if $AP.get_bool(r, hv) then print! (" v") else ()): void
      val () = println! ()
      val () = (if help > 0 then _help(r, 512) else ()): void
    in $AP.parse_result_free(r) end
  | ~$R.err(e) => let
      val () = (case+ e of
        | ~$AP.err_unknown_long(i) => println! (" unknown long ", i)
        | ~$AP.err_unknown_short(c) => println! (" unknown short ", c)
        | ~$AP.err_range(i) => println! (" range ", i)
        | ~$AP.err_exclusive(g) => println! (" exclusive ", g)
        | ~$AP.err_choice(i) => println! (" choice ", i)
        | ~$AP.err_too_long(i) => println! (" too long ", i)): void
    in end
end

implement main0 () = let
  var a0 = @[char][9]('p', 'r', 'o', 'g', '\000', 'r', 'u', 'n', '\000')
  val () = _run("run", a0, 9, 2, 0)
  var a1 = @[char][11]('p', 'r', 'o', 'g', '\000', 'b', 'u', 'i', 'l', 'd', '\000')
  val () = _run("build", a1, 11, 2, 0)
  var a2 = @[char][5]('p', 'r', 'o', 'g', '\000')
  val () = _run("none", a2, 5, 1, 1)
  var a3 = @[char][20]('p', 'r', 'o', 'g', '\000', '-', 'v', '\000', 'b', 'u', 'i', 'l', 'd', '\000', 'x', '.', 't', 'x', 't', '\000')
  val () = _run("flag-first", a3, 20, 4, 0)
  var a4 = @[char][15]('p', 'r', 'o', 'g', '\000', 'x', '.', 't', 'x', 't', '\000', 'r', 'u', 'n', '\000')
  val () = _run("pos-first", a4, 15, 3, 0)
  var a5 = @[char][15]('p', 'r', 'o', 'g', '\000', 'r', 'u', 'n', '\000', 'b', 'u', 'i', 'l', 'd', '\000')
  val () = _run("twice", a5, 15, 3, 0)
  var a6 = @[char][10]('p', 'r', 'o', 'g', '\000', 'n', 'o', 'p', 'e', '\000')
  val () = _run("not-sub", a6, 10, 2, 0)
  var a7 = @[char][12]('p', 'r', 'o', 'g', '\000', 'a', '\000', 'n', 'o', 'p', 'e', '\000')
  val () = _run("choice", a7, 12, 3, 0)
  var a8 = @[char][11]('p', 'r', 'o', 'g', '\000', '-', '-', 'r', 'u', 'n', '\000')
  val () = _run("as-option", a8, 11, 2, 0)
in end
