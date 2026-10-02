#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R
#use str as S

fn _str {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, short_name: $R.option(int), hc: &(@[char][nh]), nh: int nh, pos: bool)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.string_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_string(p, b1, nn, short_name, b2, nh, pos)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end
fn _int {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, short_name: $R.option(int), hc: &(@[char][nh]), nh: int nh, d: int, range: $AP.int_range)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.int_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_int(p, b1, nn, short_name, b2, nh, d, range)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end
fn _flag {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, short_name: $R.option(int), hc: &(@[char][nh]), nh: int nh)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.bool_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_flag(p, b1, nn, short_name, b2, nh)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
in r end
fn _count {tp:nat}{ac:nat | ac < 64}{nn,nh:pos | tp + nn + nh <= 8192; nn <= 1048576; nh <= 1048576}
  (p: $AP.parser(tp, ac), nc: &(@[char][nn]), nn: int nn, short_name: $R.option(int), hc: &(@[char][nh]), nh: int nh)
  : @($AP.parser(tp + nn + nh, ac + 1), $AP.arg($AP.count_val)) = let
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(nc, nn))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(hc, nh))
  val r = $AP.add_count(p, b1, nn, short_name, b2, nh)
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

fn _show_sub (r: !$AP.parse_result): void =
  case+ $AP.get_subcmd(r) of
  | ~$R.some(i) => print! (" sub=", i)
  | ~$R.none() => print! (" sub=none")

(* Builds the test parser, parses argv, prints what came out. *)
fn _run {na:pos | na <= 1048576} (label: string, av: &(@[char][na]), na: int na, argc: int, help_max: int): void = let
  var pn = @[char][4]('p', 'r', 'o', 'g')
  var ph = @[char][9]('d', 'e', 'm', 'o', ' ', 't', 'o', 'o', 'l')
  val @(fpn, bpn) = $A.freeze<byte>($S.from_char_array(pn, 4))
  val @(fph, bph) = $A.freeze<byte>($S.from_char_array(ph, 9))
  val p = $AP.parser_new(bpn, 4, bph, 9)
  val () = $A.drop<byte>(fpn, bpn)
  val () = $A.free<byte>($A.thaw<byte>(fpn))
  val () = $A.drop<byte>(fph, bph)
  val () = $A.free<byte>($A.thaw<byte>(fph))
  var n1 = @[char][4]('f', 'i', 'l', 'e')
  var h1 = @[char][5]('i', 'n', 'p', 'u', 't')
  val @(p, hfile) = _str(p, n1, 4, $R.none(), h1, 5, true)
  var n2 = @[char][3]('o', 'u', 't')
  var h2 = @[char][6]('o', 'u', 't', 'p', 'u', 't')
  val @(p, hout) = _str(p, n2, 3, $R.some(111), h2, 6, false)
  var n3 = @[char][4]('j', 'o', 'b', 's')
  var h3 = @[char][7]('w', 'o', 'r', 'k', 'e', 'r', 's')
  val @(p, hjobs) = _int(p, n3, 4, $R.some(106), h3, 7, 1, $AP.IntBetween(1, 8))
  var n4 = @[char][7]('v', 'e', 'r', 'b', 'o', 's', 'e')
  var h4 = @[char][6]('c', 'h', 'a', 't', 't', 'y')
  val @(p, hv) = _flag(p, n4, 7, $R.some(118), h4, 6)
  var n5 = @[char][5]('d', 'e', 'b', 'u', 'g')
  var h5 = @[char][4]('m', 'o', 'r', 'e')
  val @(p, hd) = _count(p, n5, 5, $R.some(100), h5, 4)
  var n6 = @[char][5]('q', 'u', 'i', 'e', 't')
  var h6 = @[char][4]('h', 'u', 's', 'h')
  val @(p, hq) = _flag(p, n6, 5, $R.some(113), h6, 4)
  val @(p, g) = $AP.add_exclusive_group(p)
  val p = $AP.add_to_group(p, g, hv)
  val p = $AP.add_to_group(p, g, hq)
  val @(fa, ba) = $A.freeze<byte>($S.from_char_array(av, na))
  val res = $AP.parse(p, ba, na, argc)
  val () = $A.drop<byte>(fa, ba)
  val () = $A.free<byte>($A.thaw<byte>(fa))
  val () = print! (label, ":")
in
  case+ res of
  | ~$R.ok(r) => let
      val () = _show_str(r, "file", hfile)
      val () = _show_str(r, "out", hout)
      val () = print! (" jobs=", $AP.get_int(r, hjobs))
      val () = (if $AP.is_present(r, hjobs) then print! ("!") else ()): void
      val () = (if $AP.get_bool(r, hv) then print! (" v") else ()): void
      val () = (if $AP.get_bool(r, hq) then print! (" q") else ()): void
      val () = print! (" d=", $AP.get_count(r, hd))
      val () = _show_sub(r)
      val () = println! ()
      val () = (if help_max > 512 then _help(r, 512)
                else if help_max > 0 then _help(r, 20) else ()): void
    in $AP.parse_result_free(r) end
  | ~$R.err(e) => let
      val () = (case+ e of
        | ~$AP.err_unknown_long(closest) => (case+ closest of
          | ~$R.some(i) => println! (" unknown long ", i)
          | ~$R.none() => println! (" unknown long"))
        | ~$AP.err_not_int(i) => println! (" not an int ", i)
        | ~$AP.err_unknown_short(c) => println! (" unknown short ", c)
        | ~$AP.err_range(i) => println! (" range ", i)
        | ~$AP.err_exclusive(g) => println! (" exclusive ", g)
        | ~$AP.err_choice(i) => println! (" choice ", i)
        | ~$AP.err_too_long(i) => println! (" too long ", i)): void
    in end
end

implement main0 () = let
  var a0 = @[char][41]('p', 'r', 'o', 'g', '\000', 'i', 'n', '.', 't', 'x', 't', '\000', '-', 'o', '\000', 'o', 'u', 't', '.', 'b', 'i', 'n', '\000', '-', '-', 'j', 'o', 'b', 's', '\000', '4', '\000', '-', 'v', '\000', '-', 'd', '\000', '-', 'd', '\000')
  val () = _run("full", a0, 41, 9, 1000)
  var a1 = @[char][5]('p', 'r', 'o', 'g', '\000')
  val () = _run("defaults", a1, 5, 1, 20)
  var a2 = @[char][14]('p', 'r', 'o', 'g', '\000', '-', '-', 'j', 'b', 'o', 's', '\000', '3', '\000')
  val () = _run("typo", a2, 14, 3, 0)
  var a3 = @[char][18]('p', 'r', 'o', 'g', '\000', '-', '-', 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'z', 'z', '\000')
  val () = _run("far", a3, 18, 2, 0)
  var a4 = @[char][8]('p', 'r', 'o', 'g', '\000', '-', 'x', '\000')
  val () = _run("short", a4, 8, 2, 0)
  var a5 = @[char][14]('p', 'r', 'o', 'g', '\000', '-', '-', 'j', 'o', 'b', 's', '\000', '9', '\000')
  val () = _run("range-hi", a5, 14, 3, 0)
  var a6 = @[char][15]('p', 'r', 'o', 'g', '\000', '-', '-', 'j', 'o', 'b', 's', '\000', '-', '5', '\000')
  val () = _run("range-neg", a6, 15, 3, 0)
  var a7 = @[char][15]('p', 'r', 'o', 'g', '\000', '-', '-', 'j', 'o', 'b', 's', '\000', '4', 'x', '\000')
  val () = _run("bad-int", a7, 15, 3, 0)
  var a8 = @[char][11]('p', 'r', 'o', 'g', '\000', '-', '-', 'o', 'u', 't', '\000')
  val () = _run("no-value", a8, 11, 2, 0)
  var a9 = @[char][23]('p', 'r', 'o', 'g', '\000', '-', '-', 'o', 'u', 't', '\000', 'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\000')
  val () = _run("long-out", a9, 23, 3, 0)
  var a10 = @[char][11]('p', 'r', 'o', 'g', '\000', 'a', '\000', 'b', '\000', 'c', '\000')
  val () = _run("extra-pos", a10, 11, 4, 0)
  var a11 = @[char][9]('p', 'r', 'o', 'g', '\000', '\000', 'i', 'n', '\000')
  val () = _run("empty-tok", a11, 9, 3, 0)
  var a12 = @[char][7]('p', 'r', 'o', 'g', '\000', '-', '\000')
  val () = _run("dash", a12, 7, 2, 0)
  var a13 = @[char][11]('p', 'r', 'o', 'g', '\000', 'i', 'n', '\000', '-', 'v', '\000')
  val () = _run("argc-short", a13, 11, 2, 0)
  var a14 = @[char][14]('p', 'r', 'o', 'g', '\000', '-', '-', 'j', 'o', 'b', 's', '\000', '0', '\000')
  val () = _run("range-lo", a14, 14, 3, 0)
  var a15 = @[char][10]('p', 'r', 'o', 'g', '\000', '-', 'j', '\000', '8', '\000')
  val () = _run("range-edge", a15, 10, 3, 0)
  var a16 = @[char][11]('p', 'r', 'o', 'g', '\000', '-', 'v', '\000', '-', 'q', '\000')
  val () = _run("excl", a16, 11, 3, 0)
  var a17 = @[char][14]('p', 'r', 'o', 'g', '\000', 'i', 'n', '\000', '-', 'q', '\000', '-', 'd', '\000')
  val () = _run("excl-one", a17, 14, 4, 0)
in end

