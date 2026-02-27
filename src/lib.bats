(* argparse -- type-safe CLI argument parser *)
(* Phantom-typed handles ensure correct value extraction. *)
(* Parser type indexed by text buffer position for compile-time bounds. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use result as R
#use env as E

(* ============================================================
   Value type tags (phantom types)
   ============================================================ *)

#pub abst@ype string_val = int
#pub abst@ype int_val = int
#pub abst@ype bool_val = int
#pub abst@ype count_val = int

(* Phantom-typed argument handle *)
#pub abst@ype arg(a:t@ype) = int

(* Internal castfns for phantom type conversion — private to this module *)
castfn _arg_of_int {a:t@ype} (idx: int): arg(a)
castfn _int_of_arg {a:t@ype} (h: arg(a)): int

(* ============================================================
   Spec constants
   ============================================================ *)

stadef MAX_ARGS = 64
stadef MAX_TEXT = 8192
stadef SPEC_STRIDE = 16

(* ============================================================
   Parser (linear, indexed by text position)
   ============================================================ *)

#pub datavtype parser(int) =
  | {ls:agz}{lt:agz}{lsc:agz}{tp:nat | tp <= 8192}
    parser_mk(tp) of (
      $A.arr(int, ls, 1024),      (* specs: MAX_ARGS * SPEC_STRIDE *)
      $A.arr(byte, lt, 8192),     (* text buffer *)
      $A.arr(int, lsc, 64),       (* subcmd data *)
      int, int tp, int, int, int, (* arg_count, text_pos, pos_count, subcmd_count, group_count *)
      int, int, int, int          (* prog name_off, name_len, help_off, help_len *)
    )

(* ============================================================
   Parse result (linear)
   ============================================================ *)

#pub datavtype parse_result =
  | {ls:agz}{lm:agz}{li:agz}{lb:agz}{lp:agz}{lt:agz}{lsp:agz}
    parse_result_mk of (
      $A.arr(byte, ls, 8192),     (* string values *)
      $A.arr(int, lm, 128),       (* string meta: off+len per arg *)
      $A.arr(int, li, 64),        (* int values *)
      $A.arr(int, lb, 64),        (* bool/count values *)
      $A.arr(int, lp, 64),        (* present flags *)
      int, int, int,              (* arg_count, text_pos, subcmd_idx *)
      $A.arr(byte, lt, 8192),     (* spec text for help *)
      $A.arr(int, lsp, 1024)      (* spec data for help *)
    )

(* ============================================================
   Parse error (linear)
   ============================================================ *)

#pub datavtype parse_error =
  | err_unknown_long of (int)
  | err_unknown_short of (int)
  | err_range of (int)
  | err_exclusive of (int)
  | err_choice of (int)

(* ============================================================
   API — Construction
   ============================================================ *)

#pub fn parser_new
  {lp:agz}{np:pos}{lh:agz}{nh:pos | np + nh <= 8192}
  (name: !$A.borrow(byte, lp, np), nlen: int np,
   help: !$A.borrow(byte, lh, nh), hlen: int nh): parser(np + nh)

#pub fn add_string
  {tp:nat | tp <= 8192}{ln:agz}{nn:pos}{lh:agz}{nh:pos | tp + nn + nh <= 8192}
  (p: parser(tp), name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh,
   positional: bool): @(parser(tp + nn + nh), arg(string_val))

#pub fn add_int
  {tp:nat | tp <= 8192}{ln:agz}{nn:pos}{lh:agz}{nh:pos | tp + nn + nh <= 8192}
  (p: parser(tp), name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh,
   default_val: int, min_val: int, max_val: int): @(parser(tp + nn + nh), arg(int_val))

#pub fn add_flag
  {tp:nat | tp <= 8192}{ln:agz}{nn:pos}{lh:agz}{nh:pos | tp + nn + nh <= 8192}
  (p: parser(tp), name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh): @(parser(tp + nn + nh), arg(bool_val))

#pub fn add_count
  {tp:nat | tp <= 8192}{ln:agz}{nn:pos}{lh:agz}{nh:pos | tp + nn + nh <= 8192}
  (p: parser(tp), name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh): @(parser(tp + nn + nh), arg(count_val))

#pub fn add_subcommand
  {tp:nat | tp <= 8192}{ln:agz}{nn:pos}{lh:agz}{nh:pos | tp + nn + nh <= 8192}
  (p: parser(tp), name: !$A.borrow(byte, ln, nn), nlen: int nn,
   help: !$A.borrow(byte, lh, nh), hlen: int nh): @(parser(tp + nn + nh), int)

(* ============================================================
   API — Parsing
   ============================================================ *)

#pub fn parse
  {tp:nat | tp <= 8192}{la:agz}{na:pos}
  (p: parser(tp), argv: !$A.borrow(byte, la, na), argv_len: int na,
   argc: int): $R.result(parse_result, parse_error)

(* ============================================================
   API — Extraction (phantom-typed)
   ============================================================ *)

#pub fn get_string_len(r: !parse_result, h: arg(string_val)): int

#pub fn get_string_copy
  {l:agz}{n:pos}
  (r: !parse_result, h: arg(string_val),
   buf: !$A.arr(byte, l, n), max_len: int n): int

#pub fn get_int(r: !parse_result, h: arg(int_val)): int

#pub fn get_bool(r: !parse_result, h: arg(bool_val)): bool

#pub fn get_count(r: !parse_result, h: arg(count_val)): int

#pub fn is_present {a:t@ype} (r: !parse_result, h: arg(a)): bool

#pub fn get_subcmd(r: !parse_result): int

(* ============================================================
   API — Cleanup
   ============================================================ *)

#pub fn format_help
  {l:agz}{n:pos}
  (r: !parse_result, buf: !$A.arr(byte, l, n), max_len: int n): int

#pub fn add_exclusive_group {tp:nat | tp <= 8192} (p: parser(tp)): @(parser(tp), int)

#pub fn add_to_group {tp:nat | tp <= 8192}{a:t@ype} (p: parser(tp), group_id: int, handle: arg(a)): parser(tp)

#pub fn parse_result_free(r: parse_result): void

#pub fn parse_error_free(e: parse_error): void

#pub fn parser_free {tp:nat | tp <= 8192} (p: parser(tp)): void

(* ============================================================
   Implementation helpers
   ============================================================ *)

fn _text_write
  {lt:agz}{lb:agz}{n:pos}{p:nat | p + n <= 8192}
  (tbuf: !$A.arr(byte, lt, 8192), pos: int p,
   src: !$A.borrow(byte, lb, n), len: int n): int(p + n) = let
  fun loop {lt2:agz}{lb2:agz}{nn:pos}{pp:nat | pp + nn <= 8192}{i:nat | i <= nn} .<nn - i>.
    (dst: !$A.arr(byte, lt2, 8192), src: !$A.borrow(byte, lb2, nn),
     p: int pp, si: int i, n: int nn): void =
    if si >= n then ()
    else let
      val () = $A.set<byte>(dst, p + si, $A.read<byte>(src, si))
    in loop(dst, src, p, si + 1, n) end
  val () = loop(tbuf, src, pos, 0, len)
in pos + len end

fn _spec_set {ls:agz}
  (specs: !$A.arr(int, ls, 1024), idx: int, field: int, v: int): void =
  $A.set<int>(specs, $AR.checked_idx(idx * 16 + field, 1024), v)

fn _spec_get {ls:agz}
  (specs: !$A.arr(int, ls, 1024), idx: int, field: int): int =
  $A.get<int>(specs, $AR.checked_idx(idx * 16 + field, 1024))

(* ============================================================
   Implementations — Construction
   ============================================================ *)

implement parser_new {lp}{np}{lh}{nh} (name, nlen, help, hlen) = let
  val specs = $A.alloc<int>(1024)
  val tbuf = $A.alloc<byte>(8192)
  val subcmds = $A.alloc<int>(64)
  val tp = _text_write(tbuf, 0, name, nlen)
  val tp2 = _text_write(tbuf, tp, help, hlen)
in parser_mk(specs, tbuf, subcmds, 0, tp2, 0, 0, 0, 0, nlen, tp, hlen) end

implement parser_free {tp0} (p) = let
  val+ ~parser_mk(specs, tbuf, subcmds, _, _, _, _, _, _, _, _, _) = p
in $A.free<int>(specs); $A.free<byte>(tbuf); $A.free<int>(subcmds) end

fn _add_base
  {tp0:nat | tp0 <= 8192}{ln:agz}{nn:pos}{lh:agz}{nh:pos | tp0 + nn + nh <= 8192}
  (p: parser(tp0), kind: int, vtype: int,
   name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int,
   help: !$A.borrow(byte, lh, nh), hlen: int nh,
   def_int: int): @(parser(tp0 + nn + nh), int) = let
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p
  val idx = ac
  val noff = tp
  val tp2 = _text_write(tbuf, tp, name, nlen)
  val hoff = tp2
  val tp3 = _text_write(tbuf, tp2, help, hlen)
  val () = _spec_set(specs, idx, 0, kind)
  val () = _spec_set(specs, idx, 1, vtype)
  val () = _spec_set(specs, idx, 2, noff)
  val () = _spec_set(specs, idx, 3, nlen)
  val () = _spec_set(specs, idx, 4, hoff)
  val () = _spec_set(specs, idx, 5, hlen)
  val () = _spec_set(specs, idx, 6, short_ch)
  val () = _spec_set(specs, idx, 7, def_int)
  val new_pc = if kind = 0 then pc + 1 else pc
in @(parser_mk(specs, tbuf, subcmds, ac + 1, tp3, new_pc, sc, gc, pno, pnl, pho, phl), idx) end

implement add_string {tp0}{ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen, positional) = let
  val kind = if positional then 0 else 1
  val @(p2, idx) = _add_base(p, kind, 0, name, nlen, short_ch, help, hlen, 0)
in @(p2, _arg_of_int{string_val}(idx)) end

implement add_int {tp0}{ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen, def, mn, mx) = let
  val @(p2, idx) = _add_base(p, 1, 1, name, nlen, short_ch, help, hlen, def)
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p2
  val () = _spec_set(specs, idx, 10, mn)
  val () = _spec_set(specs, idx, 11, mx)
in @(parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl),
     _arg_of_int{int_val}(idx)) end

implement add_flag {tp0}{ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen) = let
  val @(p2, idx) = _add_base(p, 2, 2, name, nlen, short_ch, help, hlen, 0)
in @(p2, _arg_of_int{bool_val}(idx)) end

implement add_count {tp0}{ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen) = let
  val @(p2, idx) = _add_base(p, 2, 3, name, nlen, short_ch, help, hlen, 0)
in @(p2, _arg_of_int{count_val}(idx)) end

implement add_subcommand {tp0}{ln}{nn}{lh}{nh} (p, name, nlen, help, hlen) = let
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p
  val si = sc
  val noff = tp
  val tp2 = _text_write(tbuf, tp, name, nlen)
  val hoff = tp2
  val tp3 = _text_write(tbuf, tp2, help, hlen)
  val base = si * 4
  val () = $A.set<int>(subcmds, $AR.checked_idx(base, 64), noff)
  val () = $A.set<int>(subcmds, $AR.checked_idx(base + 1, 64), nlen)
  val () = $A.set<int>(subcmds, $AR.checked_idx(base + 2, 64), hoff)
  val () = $A.set<int>(subcmds, $AR.checked_idx(base + 3, 64), hlen)
in @(parser_mk(specs, tbuf, subcmds, ac, tp3, pc, sc + 1, gc, pno, pnl, pho, phl), si) end

(* ============================================================
   Implementations — Parse
   ============================================================ *)

(* Compare bytes in argv at offset against spec text *)
fn _bytes_eq
  {la:agz}{na:pos}{lt:agz}
  (argv: !$A.borrow(byte, la, na), av_off: int, av_len: int na,
   tbuf: !$A.arr(byte, lt, 8192), tb_off: int, cmp_len: int): bool = let
  val cl = $AR.checked_nat(cmp_len)
  fun loop {la2:agz}{na2:pos}{lt2:agz}{nn:nat}{i:nat | i <= nn} .<nn - i>.
    (argv: !$A.borrow(byte, la2, na2), av_len: int na2,
     tbuf: !$A.arr(byte, lt2, 8192),
     ao: int, to: int, n: int nn, i: int i): bool =
    if i >= n then true
    else let
      val ab = byte2int0($A.read<byte>(argv, $AR.checked_idx(ao + i, na2)))
      val tb = byte2int0($A.get<byte>(tbuf, $AR.checked_idx(to + i, 8192)))
    in
      if $AR.eq_int_int(ab, tb) then loop(argv, av_len, tbuf, ao, to, n, i + 1)
      else false
    end
in loop(argv, av_len, tbuf, av_off, tb_off, cl, 0) end

(* Find arg index matching --name in argv starting at av_off *)
fun _find_by_name
  {la:agz}{na:pos}{ls:agz}{lt:agz}{fuel:nat} .<fuel>.
  (argv: !$A.borrow(byte, la, na), av_off: int, name_len: int, av_len: int na,
   specs: !$A.arr(int, ls, 1024), tbuf: !$A.arr(byte, lt, 8192),
   ac: int, i: int, fuel: int fuel): $R.option(int) =
  if fuel <= 0 then $R.none()
  else if i >= ac then $R.none()
  else let
    val kind = _spec_get(specs, i, 0)
  in
    if kind > 0 then let
      val snoff = _spec_get(specs, i, 2)
      val snlen = _spec_get(specs, i, 3)
    in
      if $AR.eq_int_int(snlen, name_len) then
        if _bytes_eq(argv, av_off, av_len, tbuf, snoff, name_len) then $R.some(i)
        else _find_by_name(argv, av_off, name_len, av_len, specs, tbuf, ac, i + 1, fuel - 1)
      else _find_by_name(argv, av_off, name_len, av_len, specs, tbuf, ac, i + 1, fuel - 1)
    end
    else _find_by_name(argv, av_off, name_len, av_len, specs, tbuf, ac, i + 1, fuel - 1)
  end

(* Find arg index matching short flag -c *)
fun _find_by_short
  {ls:agz}{fuel:nat} .<fuel>.
  (specs: !$A.arr(int, ls, 1024), ch: int, ac: int, i: int, fuel: int fuel): $R.option(int) =
  if fuel <= 0 then $R.none()
  else if i >= ac then $R.none()
  else let
    val kind = _spec_get(specs, i, 0)
    val sc = _spec_get(specs, i, 6)
  in
    if kind > 0 then
      if $AR.eq_int_int(sc, ch) then $R.some(i)
      else _find_by_short(specs, ch, ac, i + 1, fuel - 1)
    else _find_by_short(specs, ch, ac, i + 1, fuel - 1)
  end

(* Parse int from bytes. Returns (value, success). *)
fun _parse_int
  {la:agz}{na:pos}{fuel:nat} .<fuel>.
  (argv: !$A.borrow(byte, la, na), off: int, len: int, av_len: int na,
   fuel: int fuel): @(int, bool) = let
  fun digit_loop
    {la:agz}{na:pos}{fuel:nat} .<fuel>.
    (argv: !$A.borrow(byte, la, na), off: int, len: int, av_len: int na,
     acc: int, i: int, fuel: int fuel): @(int, bool) =
    if fuel <= 0 then @(acc, true)
    else if i >= len then @(acc, true)
    else let
      val b = byte2int0($A.read<byte>(argv, $AR.checked_idx(off + i, av_len)))
    in
      if b >= 48 then
        if b <= 57 then
          digit_loop(argv, off, len, av_len, acc * 10 + (b - 48), i + 1, fuel - 1)
        else @(0, false)
      else @(0, false)
    end
in
  if len <= 0 then @(0, false)
  else let
    val first = byte2int0($A.read<byte>(argv, $AR.checked_idx(off, av_len)))
  in
    if $AR.eq_int_int(first, 45) then let
      val @(v, ok) = digit_loop(argv, off + 1, len - 1, av_len, 0, 0, $AR.checked_nat(len))
    in @(0 - v, ok) end
    else digit_loop(argv, off, len, av_len, 0, 0, $AR.checked_nat(len))
  end
end

(* Store string value: copy from argv to result string buffer *)
fn _store_str
  {la:agz}{na:pos}{ls:agz}{lm:agz}
  (argv: !$A.borrow(byte, la, na), av_off: int, val_len: int, av_len: int na,
   str_buf: !$A.arr(byte, ls, 8192), str_meta: !$A.arr(int, lm, 128),
   idx: int, str_pos: int): int = let
  val vl = $AR.checked_nat(val_len)
  fun copy {la2:agz}{na2:pos}{ls2:agz}{nn:nat}{i:nat | i <= nn} .<nn - i>.
    (src: !$A.borrow(byte, la2, na2), dst: !$A.arr(byte, ls2, 8192),
     av_len: int na2, ao: int, sp: int, n: int nn, i: int i): void =
    if i >= n then ()
    else let
      val () = $A.set<byte>(dst, $AR.checked_idx(sp + i, 8192), $A.read<byte>(src, $AR.checked_idx(ao + i, na2)))
    in copy(src, dst, av_len, ao, sp, n, i + 1) end
  val () = copy(argv, str_buf, av_len, av_off, str_pos, vl, 0)
  val () = $A.set<int>(str_meta, $AR.checked_idx(idx * 2, 128), str_pos)
  val () = $A.set<int>(str_meta, $AR.checked_idx(idx * 2 + 1, 128), val_len)
in str_pos + val_len end

(* Find end of a null-terminated token in argv *)
fun _find_tok_end
  {la:agz}{na:pos}{f:nat} .<f>.
  (argv: !$A.borrow(byte, la, na), pos: int, av_len: int na, f: int f): int =
  if f <= 0 then pos
  else if pos >= av_len then pos
  else let
    val p1 = $AR.checked_idx(pos, av_len)
  in
    if $AR.eq_int_int(byte2int0($A.read<byte>(argv, p1)), 0) then pos
    else _find_tok_end(argv, pos + 1, av_len, f - 1)
  end

(* Find the nth positional arg spec *)
fun _find_pos_spec
  {ls:agz}{f:nat} .<f>.
  (specs: !$A.arr(int, ls, 1024), ac: int, i: int, pi: int, f: int f): $R.option(int) =
  if f <= 0 then $R.none()
  else if i >= ac then $R.none()
  else let
    val kind = _spec_get(specs, i, 0)
  in
    if $AR.eq_int_int(kind, 0) then
      if $AR.eq_int_int(pi, 0) then $R.some(i)
      else _find_pos_spec(specs, ac, i + 1, pi - 1, f - 1)
    else _find_pos_spec(specs, ac, i + 1, pi, f - 1)
  end

(* Mark an arg present in the present array *)
fn _mark_present
  {lp:agz}
  (present: !$A.arr(int, lp, 64), idx: int): void =
  $A.set<int>(present, $AR.checked_idx(idx, 64), 1)

(* Increment a bool/count value *)
fn _inc_bool
  {lb:agz}
  (bool_vals: !$A.arr(int, lb, 64), idx: int): void = let
  val pi = $AR.checked_idx(idx, 64)
  val cur = $A.get<int>(bool_vals, pi)
in $A.set<int>(bool_vals, pi, cur + 1) end

(* Store an int value *)
fn _set_int_val
  {li:agz}
  (int_vals: !$A.arr(int, li, 64), idx: int, v: int): void =
  $A.set<int>(int_vals, $AR.checked_idx(idx, 64), v)

(* Process a known option (long or short) given its spec index.
   Returns @(pos_idx, str_pos, next_av_pos, next_tok_num, subcmd_idx). *)
fn _process_option
  {la:agz}{na:pos}{ls:agz}{lm:agz}{li:agz}{lb:agz}{lp:agz}{lsb:agz}
  (argv: !$A.borrow(byte, la, na), av_len: int na,
   specs: !$A.arr(int, ls, 1024),
   str_buf: !$A.arr(byte, lsb, 8192), str_meta: !$A.arr(int, lm, 128),
   int_vals: !$A.arr(int, li, 64), bool_vals: !$A.arr(int, lb, 64),
   present: !$A.arr(int, lp, 64),
   idx: int,
   pos_idx: int, str_pos: int, next_pos: int, tok_num: int, subcmd_idx: int
  ): @(int, int, int, int, int) = let
  val kind = _spec_get(specs, idx, 0)
  val vtype = _spec_get(specs, idx, 1)
  val () = _mark_present(present, idx)
in
  if $AR.eq_int_int(kind, 2) then let
    val () = _inc_bool(bool_vals, idx)
  in @(pos_idx, str_pos, next_pos, tok_num + 1, subcmd_idx) end
  else let
    val vs = next_pos
    val ve = _find_tok_end(argv, vs, av_len, av_len)
    val vl = ve - vs
    val np2 = ve + 1
  in
    if $AR.eq_int_int(vtype, 1) then let
      val @(iv, _) = _parse_int(argv, vs, vl, av_len, $AR.checked_nat(if vl > 0 then vl else 0))
      val () = _set_int_val(int_vals, idx, iv)
    in @(pos_idx, str_pos, np2, tok_num + 2, subcmd_idx) end
    else let
      val sp2 = _store_str(argv, vs, vl, av_len, str_buf, str_meta, idx, str_pos)
    in @(pos_idx, sp2, np2, tok_num + 2, subcmd_idx) end
  end
end

(* Classify a token: 0=skip, 1=long option, 2=short option, 3=positional *)
fn _classify_token
  {la:agz}{na:pos}
  (argv: !$A.borrow(byte, la, na), av_len: int na,
   tok_start: int, tok_len: int): int = let
  val b0 = byte2int0($A.read<byte>(argv, $AR.checked_idx(tok_start, av_len)))
in
  if $AR.eq_int_int(b0, 45) then let
    val b1 = byte2int0($A.read<byte>(argv, $AR.checked_idx(tok_start + 1, av_len)))
  in
    if $AR.eq_int_int(b1, 45) then 1
    else 2
  end
  else 3
end

(* Get the short char byte at tok_start+1 *)
fn _get_short_char
  {la:agz}{na:pos}
  (argv: !$A.borrow(byte, la, na), av_len: int na, tok_start: int): int =
  byte2int0($A.read<byte>(argv, $AR.checked_idx(tok_start + 1, av_len)))

(* Simple edit distance between two byte sequences in different buffers.
   Used for "did you mean?" suggestions. O(n*m) but names are short. *)
fun _edit_dist
  {la:agz}{na:pos}{lt:agz}{fuel:nat} .<fuel>.
  (argv: !$A.borrow(byte, la, na), a_off: int, a_len: int, av_len: int na,
   tbuf: !$A.arr(byte, lt, 8192), b_off: int, b_len: int,
   fuel: int fuel): int =
  if fuel <= 0 then 999
  else if a_len <= 0 then b_len
  else if b_len <= 0 then a_len
  else let
    val ac = byte2int0($A.read<byte>(argv, $AR.checked_idx(a_off, av_len)))
    val bc = byte2int0($A.get<byte>(tbuf, $AR.checked_idx(b_off, 8192)))
    val cost = if $AR.eq_int_int(ac, bc) then 0 else 1
    val d1 = $AR.add_int_int(_edit_dist(argv, a_off + 1, a_len - 1, av_len, tbuf, b_off, b_len, fuel - 1), 1)
    val d2 = $AR.add_int_int(_edit_dist(argv, a_off, a_len, av_len, tbuf, b_off + 1, b_len - 1, fuel - 1), 1)
    val d3 = $AR.add_int_int(_edit_dist(argv, a_off + 1, a_len - 1, av_len, tbuf, b_off + 1, b_len - 1, fuel - 1), cost)
    val mn = if $AR.lt_int_int(d1, d2) then d1 else d2
  in if $AR.lt_int_int(d3, mn) then d3 else mn end

(* Find the closest matching option name, return its index or -1 *)
fun _find_closest
  {la:agz}{na:pos}{ls:agz}{lt:agz}{fuel:nat} .<fuel>.
  (argv: !$A.borrow(byte, la, na), name_off: int, name_len: int, av_len: int na,
   specs: !$A.arr(int, ls, 1024), tbuf: !$A.arr(byte, lt, 8192),
   ac: int, i: int, best_idx: int, best_dist: int, fuel: int fuel): int =
  if fuel <= 0 then best_idx
  else if i >= ac then best_idx
  else let
    val kind = _spec_get(specs, i, 0)
  in
    if kind > 0 then let
      val snoff = _spec_get(specs, i, 2)
      val snlen = _spec_get(specs, i, 3)
      val d = _edit_dist(argv, name_off, name_len, av_len, tbuf, snoff, snlen,
        $AR.checked_nat((name_len + snlen) * 3))
    in
      if $AR.lt_int_int(d, best_dist) then
        _find_closest(argv, name_off, name_len, av_len, specs, tbuf, ac, i + 1, i, d, fuel - 1)
      else
        _find_closest(argv, name_off, name_len, av_len, specs, tbuf, ac, i + 1, best_idx, best_dist, fuel - 1)
    end
    else _find_closest(argv, name_off, name_len, av_len, specs, tbuf, ac, i + 1, best_idx, best_dist, fuel - 1)
  end

fn _free_parse_temps
  {ls:agz}{lm:agz}{li:agz}{lb:agz}{lp:agz}{lt:agz}{lsp:agz}
  (str_buf: $A.arr(byte, ls, 8192), str_meta: $A.arr(int, lm, 128),
   int_vals: $A.arr(int, li, 64), bool_vals: $A.arr(int, lb, 64),
   present: $A.arr(int, lp, 64), tbuf: $A.arr(byte, lt, 8192),
   specs: $A.arr(int, lsp, 1024)): void = let
  val () = $A.free<byte>(str_buf)
  val () = $A.free<int>(str_meta)
  val () = $A.free<int>(int_vals)
  val () = $A.free<int>(bool_vals)
  val () = $A.free<int>(present)
  val () = $A.free<byte>(tbuf)
  val () = $A.free<int>(specs)
in end

implement parse {tp0}{la}{na} (p, argv, argv_len, argc) = let
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p
  val str_buf = $A.alloc<byte>(8192)
  val str_meta = $A.alloc<int>(128)
  val int_vals = $A.alloc<int>(64)
  val bool_vals = $A.alloc<int>(64)
  val present = $A.alloc<int>(64)

  (* Initialize defaults *)
  fun init_defs {ls:agz}{li:agz}{lb:agz}{k:nat | k <= 64} .<64 - k>.
    (specs: !$A.arr(int, ls, 1024), ivals: !$A.arr(int, li, 64),
     bvals: !$A.arr(int, lb, 64), i: int k, n: int): void =
    if i >= n then ()
    else if i >= 64 then ()
    else let
      val () = $A.set<int>(ivals, i, _spec_get(specs, i, 7))
      val () = $A.set<int>(bvals, i, 0)
    in init_defs(specs, ivals, bvals, i + 1, n) end
  val () = init_defs(specs, int_vals, bool_vals, 0, ac)

  fun scan_argv
    {la:agz}{na:pos}{ls:agz}{lt:agz}{li:agz}{lb:agz}{lp:agz}{lm:agz}{lsb:agz}{fuel:nat} .<fuel>.
    (argv: !$A.borrow(byte, la, na), av_len: int na,
     specs: !$A.arr(int, ls, 1024), tbuf: !$A.arr(byte, lt, 8192),
     str_buf: !$A.arr(byte, lsb, 8192), str_meta: !$A.arr(int, lm, 128),
     int_vals: !$A.arr(int, li, 64), bool_vals: !$A.arr(int, lb, 64),
     present: !$A.arr(int, lp, 64),
     ac: int, pos_idx: int, str_pos: int,
     av_pos: int, tok_num: int, argc: int, subcmd_idx: int,
     fuel: int fuel): @(int, int, int) =
    if fuel <= 0 then @(str_pos, subcmd_idx, 0)
    else if tok_num >= argc then @(str_pos, subcmd_idx, 0)
    else let
      val tok_start = av_pos
      val tok_end = _find_tok_end(argv, tok_start, av_len, av_len)
      val tok_len = tok_end - tok_start
      val next_pos = tok_end + 1
    in
      if tok_len <= 0 then
        scan_argv(argv, av_len, specs, tbuf, str_buf, str_meta, int_vals, bool_vals, present,
          ac, pos_idx, str_pos, next_pos, tok_num + 1, argc, subcmd_idx, fuel - 1)
      else let
        val cls = _classify_token(argv, av_len, tok_start, tok_len)
      in
        if $AR.eq_int_int(cls, 1) then let (* long option *)
          val name_off = tok_start + 2
          val name_len = tok_len - 2
          val opt_idx = _find_by_name(argv, name_off, name_len, av_len, specs, tbuf, ac, 0, $AR.checked_nat(ac))
        in
          case+ opt_idx of
          | ~$R.some(idx) => let
              val @(pi2, sp2, np2, tn2, si2) = _process_option(argv, av_len, specs,
                str_buf, str_meta, int_vals, bool_vals, present,
                idx, pos_idx, str_pos, next_pos, tok_num, subcmd_idx)
            in
              scan_argv(argv, av_len, specs, tbuf, str_buf, str_meta, int_vals, bool_vals, present,
                ac, pi2, sp2, np2, tn2, argc, si2, fuel - 1)
            end
          | ~$R.none() => let
              val closest = _find_closest(argv, name_off, name_len, av_len,
                specs, tbuf, ac, 0, ~1, 999, $AR.checked_nat(ac))
            in
              if closest >= 0 then @(str_pos, subcmd_idx, 3000 + closest)
              else @(str_pos, subcmd_idx, 3000)
            end
        end
        else if $AR.eq_int_int(cls, 2) then let (* short option *)
          val ch = _get_short_char(argv, av_len, tok_start)
          val opt_idx = _find_by_short(specs, ch, ac, 0, $AR.checked_nat(ac))
        in
          case+ opt_idx of
          | ~$R.some(idx) => let
              val @(pi2, sp2, np2, tn2, si2) = _process_option(argv, av_len, specs,
                str_buf, str_meta, int_vals, bool_vals, present,
                idx, pos_idx, str_pos, next_pos, tok_num, subcmd_idx)
            in
              scan_argv(argv, av_len, specs, tbuf, str_buf, str_meta, int_vals, bool_vals, present,
                ac, pi2, sp2, np2, tn2, argc, si2, fuel - 1)
            end
          | ~$R.none() =>
              @(str_pos, subcmd_idx, 4000 + ch)
        end
        else if $AR.eq_int_int(cls, 3) then let (* positional *)
          val opt_pidx = _find_pos_spec(specs, ac, 0, pos_idx, $AR.checked_nat(ac))
        in
          case+ opt_pidx of
          | ~$R.some(pidx) => let
              val () = _mark_present(present, pidx)
              val sp2 = _store_str(argv, tok_start, tok_len, av_len, str_buf, str_meta, pidx, str_pos)
            in
              scan_argv(argv, av_len, specs, tbuf, str_buf, str_meta, int_vals, bool_vals, present,
                ac, pos_idx + 1, sp2, next_pos, tok_num + 1, argc, subcmd_idx, fuel - 1)
            end
          | ~$R.none() =>
              scan_argv(argv, av_len, specs, tbuf, str_buf, str_meta, int_vals, bool_vals, present,
                ac, pos_idx, str_pos, next_pos, tok_num + 1, argc, tok_num, fuel - 1)
        end
        else (* cls=0: skip *)
          scan_argv(argv, av_len, specs, tbuf, str_buf, str_meta, int_vals, bool_vals, present,
            ac, pos_idx, str_pos, next_pos, tok_num + 1, argc, subcmd_idx, fuel - 1)
      end
    end

  (* Skip first token (program name) *)
  val first_end = _find_tok_end(argv, 0, argv_len, argv_len)
  val start_pos = first_end + 1

  val @(final_sp, final_subcmd, err) =
    scan_argv(argv, argv_len, specs, tbuf, str_buf, str_meta, int_vals, bool_vals, present,
      ac, 0, 0, start_pos, 1, argc, ~1, $AR.checked_nat(argc * 2))

  val scan_err = err

  (* Int range validation *)
  fun check_ranges {ls:agz}{li:agz}{lp:agz}{k:nat | k <= 64} .<64 - k>.
    (specs: !$A.arr(int, ls, 1024), int_vals: !$A.arr(int, li, 64),
     present: !$A.arr(int, lp, 64), i: int k, ac: int): int =
    if i >= ac then 0
    else if i >= 64 then 0
    else let
      val vtype = _spec_get(specs, i, 1)
    in
      if $AR.eq_int_int(vtype, 1) then let
        val mn = _spec_get(specs, i, 10)
        val mx = _spec_get(specs, i, 11)
      in
        if $AR.eq_int_int(mn, 0) then
          if $AR.eq_int_int(mx, 0) then check_ranges(specs, int_vals, present, i + 1, ac)
          else let
            val pv = $A.get<int>(present, i)
          in
            if $AR.gt_int_int(pv, 0) then let
              val v = $A.get<int>(int_vals, i)
            in
              if $AR.lt_int_int(v, mn) then i + 1
              else if $AR.gt_int_int(v, mx) then i + 1
              else check_ranges(specs, int_vals, present, i + 1, ac)
            end
            else check_ranges(specs, int_vals, present, i + 1, ac)
          end
        else check_ranges(specs, int_vals, present, i + 1, ac)
      end
      else check_ranges(specs, int_vals, present, i + 1, ac)
    end

  val range_err = check_ranges(specs, int_vals, present, 0, ac)

  (* Exclusive group validation *)
  fun check_exclusive {ls:agz}{lp:agz}{k:nat | k <= 64} .<64 - k>.
    (specs: !$A.arr(int, ls, 1024), present: !$A.arr(int, lp, 64),
     i: int k, ac: int, gid: int, count: int): int =
    if i >= ac then count
    else if i >= 64 then count
    else let
      val g = _spec_get(specs, i, 15)
      val pv = $A.get<int>(present, i)
      val nc = if $AR.eq_int_int(g, gid) then
        if $AR.gt_int_int(pv, 0) then count + 1 else count
      else count
    in check_exclusive(specs, present, i + 1, ac, gid, nc) end

  fun check_all_groups {ls:agz}{lp:agz}{k:nat | k <= 8} .<8 - k>.
    (specs: !$A.arr(int, ls, 1024), present: !$A.arr(int, lp, 64),
     g: int k, gc: int, ac: int): int =
    if g >= gc then 0
    else if g >= 8 then 0
    else let
      val cnt = check_exclusive(specs, present, 0, ac, g, 0)
    in
      if cnt > 1 then g + 1
      else check_all_groups(specs, present, g + 1, gc, ac)
    end

  val group_err = check_all_groups(specs, present, 0, gc, ac)

  (* Choice validation *)
  fun _check_str_match
    {ls:agz}{lt:agz}{fuel:nat} .<fuel>.
    (str_buf: !$A.arr(byte, ls, 8192), s_off: int, s_len: int,
     tbuf: !$A.arr(byte, lt, 8192), c_off: int, c_len: int,
     fuel: int fuel): bool =
    if fuel <= 0 then true
    else if s_len <= 0 then true
    else if c_len <= 0 then true
    else let
      val sb = byte2int0($A.get<byte>(str_buf, $AR.checked_idx(s_off, 8192)))
      val cb = byte2int0($A.get<byte>(tbuf, $AR.checked_idx(c_off, 8192)))
    in
      if $AR.eq_int_int(sb, cb) then
        _check_str_match(str_buf, s_off + 1, s_len - 1, tbuf, c_off + 1, c_len - 1, fuel - 1)
      else false
    end

  fun _check_one_choice
    {ls:agz}{lt:agz}{fuel:nat} .<fuel>.
    (str_buf: !$A.arr(byte, ls, 8192), s_off: int, s_len: int,
     tbuf: !$A.arr(byte, lt, 8192), c_off: int, c_total_len: int,
     choice_count: int, ci: int, fuel: int fuel): bool =
    if fuel <= 0 then false
    else if ci >= choice_count then false
    else let
      fun _find_choice_end {lt:agz}{f:nat} .<f>.
        (tbuf: !$A.arr(byte, lt, 8192), pos: int, limit: int, f: int f): int =
        if f <= 0 then pos
        else if pos >= limit then pos
        else
          if $AR.eq_int_int(byte2int0($A.get<byte>(tbuf, $AR.checked_idx(pos, 8192))), 0) then pos
          else _find_choice_end(tbuf, pos + 1, limit, f - 1)
      val ce = _find_choice_end(tbuf, c_off, c_off + c_total_len, $AR.checked_nat(c_total_len))
      val clen = ce - c_off
    in
      if $AR.eq_int_int(clen, s_len) then
        if _check_str_match(str_buf, s_off, s_len, tbuf, c_off, clen, $AR.checked_nat(clen)) then true
        else _check_one_choice(str_buf, s_off, s_len, tbuf, ce + 1, c_total_len - clen - 1, choice_count, ci + 1, fuel - 1)
      else _check_one_choice(str_buf, s_off, s_len, tbuf, ce + 1, c_total_len - clen - 1, choice_count, ci + 1, fuel - 1)
    end

  fun check_choices
    {ls:agz}{lt:agz}{lm:agz}{lp:agz}{lsp:agz}{k:nat | k <= 64} .<64 - k>.
    (specs: !$A.arr(int, lsp, 1024), str_buf: !$A.arr(byte, ls, 8192),
     str_meta: !$A.arr(int, lm, 128), tbuf: !$A.arr(byte, lt, 8192),
     present: !$A.arr(int, lp, 64), i: int k, ac: int): int =
    if i >= ac then 0
    else if i >= 64 then 0
    else let
      val vtype = _spec_get(specs, i, 1)
      val cc = _spec_get(specs, i, 14)
    in
      if $AR.eq_int_int(vtype, 0) then
        if $AR.gt_int_int(cc, 0) then let
          val pv = $A.get<int>(present, i)
        in
          if $AR.gt_int_int(pv, 0) then let
            val s_off = $A.get<int>(str_meta, $AR.checked_idx(i * 2, 128))
            val s_len = $A.get<int>(str_meta, $AR.checked_idx(i * 2 + 1, 128))
            val c_off = _spec_get(specs, i, 12)
            val c_len = _spec_get(specs, i, 13)
            val ok = _check_one_choice(str_buf, s_off, s_len, tbuf, c_off, c_len, cc, 0, $AR.checked_nat(cc))
          in
            if ok then check_choices(specs, str_buf, str_meta, tbuf, present, i + 1, ac)
            else i + 1
          end
          else check_choices(specs, str_buf, str_meta, tbuf, present, i + 1, ac)
        end
        else check_choices(specs, str_buf, str_meta, tbuf, present, i + 1, ac)
      else check_choices(specs, str_buf, str_meta, tbuf, present, i + 1, ac)
    end

  val choice_err = check_choices(specs, str_buf, str_meta, tbuf, present, 0, ac)

  (* Env var fallback *)
  fun check_env_fallback
    {ls:agz}{lt:agz}{lm:agz}{lp:agz}{lsp:agz}{k:nat | k <= 64} .<64 - k>.
    (specs: !$A.arr(int, lsp, 1024), str_buf: !$A.arr(byte, ls, 8192),
     str_meta: !$A.arr(int, lm, 128), tbuf: !$A.arr(byte, lt, 8192),
     present: !$A.arr(int, lp, 64), i: int k, ac: int, str_pos: int): int =
    if i >= ac then str_pos
    else if i >= 64 then str_pos
    else let
      val kind = _spec_get(specs, i, 0)
      val vtype = _spec_get(specs, i, 1)
      val pv = $A.get<int>(present, i)
    in
      if $AR.eq_int_int(kind, 1) then
        if $AR.eq_int_int(vtype, 0) then
          if $AR.eq_int_int(pv, 0) then let
            val e_off = _spec_get(specs, i, 12)
            val e_len = _spec_get(specs, i, 13)
            val cc = _spec_get(specs, i, 14)
          in
            if $AR.gt_int_int(e_len, 0) then
              if $AR.eq_int_int(cc, 0) then let
                val env_name = $A.alloc<byte>(256)
                val elen2 = if e_len > 255 then 255 else e_len
                val el = $AR.checked_nat(elen2)
                fun _copy_ename {lt2:agz}{le2:agz}{nn:nat}{i:nat | i <= nn} .<nn - i>.
                  (tbuf: !$A.arr(byte, lt2, 8192), ebuf: !$A.arr(byte, le2, 256),
                   s: int, n: int nn, i: int i): void =
                  if i >= n then ()
                  else let
                    val () = $A.set<byte>(ebuf, $AR.checked_idx(i, 256), $A.get<byte>(tbuf, $AR.checked_idx(s + i, 8192)))
                  in _copy_ename(tbuf, ebuf, s, n, i + 1) end
                val () = _copy_ename(tbuf, env_name, e_off, el, 0)
                val () = $A.set<byte>(env_name, $AR.checked_idx(elen2, 256), $A.int2byte($AR.checked_byte(0)))
                val env_val = $A.alloc<byte>(4096)
                val env_result = $E.get_cstr(env_name, env_val, 4096)
                val () = $A.free<byte>(env_name)
              in
                case+ env_result of
                | ~$R.some(env_len) => let
                    val el2 = $AR.checked_nat(env_len)
                    fun copy_val {ls2:agz}{le2:agz}{nn:nat}{i:nat | i <= nn} .<nn - i>.
                      (dst: !$A.arr(byte, ls2, 8192), src: !$A.arr(byte, le2, 4096),
                       d: int, n: int nn, i: int i): void =
                      if i >= n then ()
                      else let
                        val () = $A.set<byte>(dst, $AR.checked_idx(d + i, 8192), $A.get<byte>(src, $AR.checked_idx(i, 4096)))
                      in copy_val(dst, src, d, n, i + 1) end
                    val () = copy_val(str_buf, env_val, str_pos, el2, 0)
                    val () = $A.free<byte>(env_val)
                    val () = $A.set<int>(str_meta, $AR.checked_idx(i * 2, 128), str_pos)
                    val () = $A.set<int>(str_meta, $AR.checked_idx(i * 2 + 1, 128), env_len)
                    val () = $A.set<int>(present, i, 1)
                  in check_env_fallback(specs, str_buf, str_meta, tbuf, present, i + 1, ac, str_pos + env_len) end
                | ~$R.none() => let
                    val () = $A.free<byte>(env_val)
                  in check_env_fallback(specs, str_buf, str_meta, tbuf, present, i + 1, ac, str_pos) end
              end
              else check_env_fallback(specs, str_buf, str_meta, tbuf, present, i + 1, ac, str_pos)
            else check_env_fallback(specs, str_buf, str_meta, tbuf, present, i + 1, ac, str_pos)
          end
          else check_env_fallback(specs, str_buf, str_meta, tbuf, present, i + 1, ac, str_pos)
        else check_env_fallback(specs, str_buf, str_meta, tbuf, present, i + 1, ac, str_pos)
      else check_env_fallback(specs, str_buf, str_meta, tbuf, present, i + 1, ac, str_pos)
    end

  val final_sp2 = check_env_fallback(specs, str_buf, str_meta, tbuf, present, 0, ac, final_sp)

  val () = $A.free<int>(subcmds)
in
  if $AR.gt_int_int(scan_err, 0) then let
    val () = _free_parse_temps(str_buf, str_meta, int_vals, bool_vals, present, tbuf, specs)
  in
    if scan_err >= 4000 then $R.err(err_unknown_short(scan_err - 4000))
    else $R.err(err_unknown_long(scan_err - 3000))
  end
  else if $AR.gt_int_int(range_err, 0) then let
    val () = _free_parse_temps(str_buf, str_meta, int_vals, bool_vals, present, tbuf, specs)
  in $R.err(err_range(range_err)) end
  else if $AR.gt_int_int(group_err, 0) then let
    val () = _free_parse_temps(str_buf, str_meta, int_vals, bool_vals, present, tbuf, specs)
  in $R.err(err_exclusive(group_err)) end
  else if $AR.gt_int_int(choice_err, 0) then let
    val () = _free_parse_temps(str_buf, str_meta, int_vals, bool_vals, present, tbuf, specs)
  in $R.err(err_choice(choice_err)) end
  else
    $R.ok(parse_result_mk(str_buf, str_meta, int_vals, bool_vals, present,
      ac, final_sp, final_subcmd, tbuf, specs))
end

(* ============================================================
   Implementations — Extraction
   ============================================================ *)

implement get_string_len(r, h) = let
  val+ @parse_result_mk(_, smeta, _, _, _, _, _, _, _, _) = r
  val idx = _int_of_arg(h)
  val len = $A.get<int>(smeta, $AR.checked_idx(idx * 2 + 1, 128))
  prval () = fold@(r)
in len end

implement get_string_copy {l}{n} (r, h, buf, max_len) = let
  val+ @parse_result_mk(sbuf, smeta, _, _, _, _, _, _, _, _) = r
  val idx = _int_of_arg(h)
  val off = $A.get<int>(smeta, $AR.checked_idx(idx * 2, 128))
  val len = $A.get<int>(smeta, $AR.checked_idx(idx * 2 + 1, 128))
  val copy_len = (if $AR.gt_int_int(len, max_len) then max_len else len): int
  val cl = $AR.checked_nat(copy_len)
  val () = let
    fun loop {lb:agz}{nd:pos}{ls2:agz}{nn:nat}{i:nat | i <= nn} .<nn - i>.
      (dst: !$A.arr(byte, lb, nd), src: !$A.arr(byte, ls2, 8192),
       nd: int nd, o: int, n: int nn, i: int i): void =
      if i >= n then ()
      else let
        val () = $A.set<byte>(dst, $AR.checked_idx(i, nd), $A.get<byte>(src, $AR.checked_idx(o + i, 8192)))
      in loop(dst, src, nd, o, n, i + 1) end
  in loop(buf, sbuf, max_len, off, cl, 0) end
  prval () = fold@(r)
in copy_len end

implement get_int(r, h) = let
  val+ @parse_result_mk(_, _, ivals, _, _, _, _, _, _, _) = r
  val idx = _int_of_arg(h)
  val v = $A.get<int>(ivals, $AR.checked_idx(idx, 64))
  prval () = fold@(r)
in v end

implement get_bool(r, h) = let
  val+ @parse_result_mk(_, _, _, bvals, _, _, _, _, _, _) = r
  val idx = _int_of_arg(h)
  val v = $A.get<int>(bvals, $AR.checked_idx(idx, 64))
  prval () = fold@(r)
in $AR.gt_int_int(v, 0) end

implement get_count(r, h) = let
  val+ @parse_result_mk(_, _, _, bvals, _, _, _, _, _, _) = r
  val idx = _int_of_arg(h)
  val v = $A.get<int>(bvals, $AR.checked_idx(idx, 64))
  prval () = fold@(r)
in v end

implement is_present {a} (r, h) = let
  val+ @parse_result_mk(_, _, _, _, pres, _, _, _, _, _) = r
  val idx = _int_of_arg(h)
  val v = $A.get<int>(pres, $AR.checked_idx(idx, 64))
  prval () = fold@(r)
in $AR.gt_int_int(v, 0) end

implement get_subcmd(r) = let
  val+ @parse_result_mk(_, _, _, _, _, _, _, si, _, _) = r
  val v = si
  prval () = fold@(r)
in v end

(* ============================================================
   Implementations — Exclusive groups
   ============================================================ *)

implement add_exclusive_group {tp0} (p) = let
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p
  val gid = gc
in @(parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc + 1, pno, pnl, pho, phl), gid) end

implement add_to_group {tp0}{a} (p, group_id, handle) = let
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p
  val idx = _int_of_arg(handle)
  val () = _spec_set(specs, idx, 15, group_id)
in parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) end

(* ============================================================
   Implementations — Help formatting
   ============================================================ *)

fn _help_put
  {l:agz}{n:pos}
  (buf: !$A.arr(byte, l, n), pos: int, b: int, max_len: int n): int = let
  val p = $AR.checked_nat(pos)
in
  if p < max_len then let
    val () = $A.set<byte>(buf, p, $A.int2byte($AR.checked_byte(
      if b >= 0 then if b < 256 then b else 63 else 63)))
  in pos + 1 end
  else pos
end

fn _help_copy
  {l:agz}{n:pos}{lt:agz}
  (buf: !$A.arr(byte, l, n), dst: int,
   tbuf: !$A.arr(byte, lt, 8192), src: int, len: int,
   max_len: int n): int = let
  val cl = $AR.checked_nat(len)
  fun loop {l2:agz}{n2:pos}{lt2:agz}{nn:nat}{i:nat | i <= nn} .<nn - i>.
    (buf: !$A.arr(byte, l2, n2), tbuf: !$A.arr(byte, lt2, 8192),
     max_len: int n2, d: int, s: int, n: int nn, i: int i): int =
    if i >= n then d + i
    else let val di = $AR.checked_nat(d + i) in
      if di < max_len then let
        val b = byte2int0($A.get<byte>(tbuf, $AR.checked_idx(s + i, 8192)))
        val () = $A.set<byte>(buf, di, $A.int2byte($AR.checked_byte(
          if b >= 0 then if b < 256 then b else 0 else 0)))
      in loop(buf, tbuf, max_len, d, s, n, i + 1) end
      else d + i
    end
in loop(buf, tbuf, max_len, dst, src, cl, 0) end

implement format_help {l}{n} (r, buf, max_len) = let
  val+ @parse_result_mk(_, _, _, _, _, ac, _, _, tbuf, specs) = r
  val pos = _help_put(buf, 0, 85, max_len)
  val pos = _help_put(buf, pos, 115, max_len)
  val pos = _help_put(buf, pos, 97, max_len)
  val pos = _help_put(buf, pos, 103, max_len)
  val pos = _help_put(buf, pos, 101, max_len)
  val pos = _help_put(buf, pos, 58, max_len)
  val pos = _help_put(buf, pos, 10, max_len)
  val pos = _help_put(buf, pos, 10, max_len)

  fun fmt_args {l:agz}{n:pos}{ls:agz}{lt:agz}{k:nat | k <= 64} .<64 - k>.
    (buf: !$A.arr(byte, l, n), tbuf: !$A.arr(byte, lt, 8192),
     specs: !$A.arr(int, ls, 1024), pos: int, i: int k, ac: int, max_len: int n): int =
    if i >= ac then pos
    else if i >= 64 then pos
    else let
      val noff = _spec_get(specs, i, 2)
      val nlen = _spec_get(specs, i, 3)
      val hoff = _spec_get(specs, i, 4)
      val hlen = _spec_get(specs, i, 5)
      val sch = _spec_get(specs, i, 6)
      val kind = _spec_get(specs, i, 0)
      val pos = _help_put(buf, pos, 32, max_len)
      val pos = _help_put(buf, pos, 32, max_len)
    in
      if $AR.eq_int_int(kind, 0) then let
        val pos = _help_copy(buf, pos, tbuf, noff, nlen, max_len)
        val pos = _help_put(buf, pos, 32, max_len)
        val pos = _help_put(buf, pos, 32, max_len)
        val pos = _help_copy(buf, pos, tbuf, hoff, hlen, max_len)
        val pos = _help_put(buf, pos, 10, max_len)
      in fmt_args(buf, tbuf, specs, pos, i + 1, ac, max_len) end
      else let
        val pos = if sch >= 0 then let
          val p = _help_put(buf, pos, 45, max_len)
          val p = _help_put(buf, p, sch, max_len)
          val p = _help_put(buf, p, 44, max_len)
          val p = _help_put(buf, p, 32, max_len)
        in p end
        else let
          val p = _help_put(buf, pos, 32, max_len)
          val p = _help_put(buf, p, 32, max_len)
          val p = _help_put(buf, p, 32, max_len)
          val p = _help_put(buf, p, 32, max_len)
        in p end
        val pos = _help_put(buf, pos, 45, max_len)
        val pos = _help_put(buf, pos, 45, max_len)
        val pos = _help_copy(buf, pos, tbuf, noff, nlen, max_len)
        val pos = _help_put(buf, pos, 32, max_len)
        val pos = _help_put(buf, pos, 32, max_len)
        val pos = _help_copy(buf, pos, tbuf, hoff, hlen, max_len)
        val pos = _help_put(buf, pos, 10, max_len)
      in fmt_args(buf, tbuf, specs, pos, i + 1, ac, max_len) end
    end

  val final_pos = fmt_args(buf, tbuf, specs, pos, 0, ac, max_len)
  prval () = fold@(r)
in final_pos end

(* ============================================================
   Implementations — Cleanup
   ============================================================ *)

implement parse_result_free(r) = let
  val+ ~parse_result_mk(sb, sm, iv, bv, pr, _, _, _, tb, sp) = r
in
  $A.free<byte>(sb); $A.free<int>(sm);
  $A.free<int>(iv); $A.free<int>(bv); $A.free<int>(pr);
  $A.free<byte>(tb); $A.free<int>(sp)
end

implement parse_error_free(e) =
  case+ e of
  | ~err_unknown_long(_) => ()
  | ~err_unknown_short(_) => ()
  | ~err_range(_) => ()
  | ~err_exclusive(_) => ()
  | ~err_choice(_) => ()
