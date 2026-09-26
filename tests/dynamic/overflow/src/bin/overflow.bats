#include "share/atspre_staload.hats"
#use array as A
#use argparse as AP
#use result as R
#use str as S

(* Two 5000-byte values do not fit the 8192-byte value buffer. The
   second one must be reported as err_too_long for spec 2 (--out), not
   written over the start of the buffer. argv is
   "prog\0" "a" * 5000 "\0" "--out\0" "b" * 5000 "\0". *)
fun fill {l:agz}{i:nat | i <= 10013} .<10013 - i>.
  (a: !$A.arr(byte, l, 10013), i: int i): void =
  if i >= 10013 then ()
  else let
    val c =
      (if i < 4 then (if i = 0 then 112 else if i = 1 then 114 else if i = 2 then 111 else 103)
       else if i = 4 then 0
       else if i < 5005 then 97
       else if i = 5005 then 0
       else if i = 5006 then 45 else if i = 5007 then 45
       else if i = 5008 then 111 else if i = 5009 then 117 else if i = 5010 then 116
       else if i = 5011 then 0
       else if i < 10012 then 98
       else 0): [c:nat | c < 256] int c
    val () = $A.set<byte>(a, i, $A.int2byte(c))
  in fill(a, i + 1) end

implement main0 () = let
  var pn = @[char][4]('p', 'r', 'o', 'g')
  var ph = @[char][1]('x')
  val @(f1, b1) = $A.freeze<byte>($S.from_char_array(pn, 4))
  val @(f2, b2) = $A.freeze<byte>($S.from_char_array(ph, 1))
  val p = $AP.parser_new(b1, 4, b2, 1)
  var n1 = @[char][4]('f', 'i', 'l', 'e')
  val @(f3, b3) = $A.freeze<byte>($S.from_char_array(n1, 4))
  val @(p, _) = $AP.add_string(p, b3, 4, 0, b2, 1, true)
  var n2 = @[char][3]('o', 'u', 't')
  val @(f4, b4) = $A.freeze<byte>($S.from_char_array(n2, 3))
  val @(p, _) = $AP.add_string(p, b4, 3, 111, b2, 1, false)
  val () = $A.drop<byte>(f1, b1)
  val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, b2)
  val () = $A.free<byte>($A.thaw<byte>(f2))
  val () = $A.drop<byte>(f3, b3)
  val () = $A.free<byte>($A.thaw<byte>(f3))
  val () = $A.drop<byte>(f4, b4)
  val () = $A.free<byte>($A.thaw<byte>(f4))
  val av = $A.alloc<byte>(10013)
  val () = fill(av, 0)
  val @(fa, ba) = $A.freeze<byte>(av)
  val res = $AP.parse(p, ba, 10013, 4)
  val () = $A.drop<byte>(fa, ba)
  val () = $A.free<byte>($A.thaw<byte>(fa))
in
  case+ res of
  | ~$R.ok(r) => let val () = println! ("parsed") in $AP.parse_result_free(r) end
  | ~$R.err(e) => (case+ e of
    | ~$AP.err_too_long(i) => println! ("too long ", i)
    | ~$AP.err_unknown_long(i) => println! ("unknown long ", i)
    | ~$AP.err_unknown_short(i) => println! ("unknown short ", i)
    | ~$AP.err_range(i) => println! ("range ", i)
    | ~$AP.err_exclusive(i) => println! ("exclusive ", i)
    | ~$AP.err_choice(i) => println! ("choice ", i))
end
