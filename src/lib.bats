(* argparse -- type-safe CLI argument parser *)
(* Phantom-typed handles ensure correct value extraction. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use result as R

(* ============================================================
   Value type tags (phantom types)
   ============================================================ *)

#pub abst@ype string_val = int
#pub abst@ype int_val = int
#pub abst@ype bool_val = int
#pub abst@ype count_val = int

(* Phantom-typed argument handle *)
#pub abst@ype arg(a:t@ype) = int

(* ============================================================
   Spec constants
   ============================================================ *)

stadef MAX_ARGS = 64
stadef MAX_TEXT = 8192
stadef SPEC_STRIDE = 16

(* ============================================================
   Parser (linear)
   ============================================================ *)

#pub datavtype parser =
  | {ls:agz}{lt:agz}{lsc:agz}
    parser_mk of (
      $A.arr(int, ls, 1024),      (* specs: MAX_ARGS * SPEC_STRIDE *)
      $A.arr(byte, lt, 8192),     (* text buffer *)
      $A.arr(int, lsc, 64),       (* subcmd data *)
      int, int, int, int, int,    (* arg_count, text_pos, pos_count, subcmd_count, group_count *)
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
   API — Construction
   ============================================================ *)

#pub fn parser_new
  {lp:agz}{np:pos}{lh:agz}{nh:pos}
  (name: !$A.borrow(byte, lp, np), nlen: int np,
   help: !$A.borrow(byte, lh, nh), hlen: int nh): parser

#pub fn add_string
  {ln:agz}{nn:pos}{lh:agz}{nh:pos}
  (p: parser, name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh,
   positional: bool): @(parser, arg(string_val))

#pub fn add_int
  {ln:agz}{nn:pos}{lh:agz}{nh:pos}
  (p: parser, name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh,
   default_val: int, min_val: int, max_val: int): @(parser, arg(int_val))

#pub fn add_flag
  {ln:agz}{nn:pos}{lh:agz}{nh:pos}
  (p: parser, name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh): @(parser, arg(bool_val))

#pub fn add_count
  {ln:agz}{nn:pos}{lh:agz}{nh:pos}
  (p: parser, name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int, help: !$A.borrow(byte, lh, nh), hlen: int nh): @(parser, arg(count_val))

#pub fn add_subcommand
  {ln:agz}{nn:pos}{lh:agz}{nh:pos}
  (p: parser, name: !$A.borrow(byte, ln, nn), nlen: int nn,
   help: !$A.borrow(byte, lh, nh), hlen: int nh): @(parser, int)

(* ============================================================
   API — Parsing
   ============================================================ *)

#pub fn parse
  {la:agz}{na:pos}
  (p: parser, argv: !$A.borrow(byte, la, na), argv_len: int na,
   argc: int): $R.result(parse_result)

(* ============================================================
   API — Extraction (phantom-typed)
   ============================================================ *)

#pub fn get_string_len(r: !parse_result, h: arg(string_val)): int

#pub fn get_int(r: !parse_result, h: arg(int_val)): int

#pub fn get_bool(r: !parse_result, h: arg(bool_val)): bool

#pub fn get_count(r: !parse_result, h: arg(count_val)): int

#pub fn is_present {a:t@ype} (r: !parse_result, h: arg(a)): bool

#pub fn get_subcmd(r: !parse_result): int

(* ============================================================
   API — Cleanup
   ============================================================ *)

#pub fn parse_result_free(r: parse_result): void

#pub fn parser_free(p: parser): void

(* ============================================================
   Implementation helpers
   ============================================================ *)

fn _text_write
  {lt:agz}{lb:agz}{n:pos}
  (tbuf: !$A.arr(byte, lt, 8192), pos: int,
   src: !$A.borrow(byte, lb, n), len: int n): int = let
  fun loop {ld:agz}{lb:agz}{n:pos}{i:nat | i <= n} .<n - i>.
    (dst: !$A.arr(byte, ld, 8192), src: !$A.borrow(byte, lb, n),
     di: int, si: int i, n: int n): void =
    if si >= n then ()
    else let val di1 = g1ofg0(di) in
      if di1 >= 0 then if di1 < 8192 then let
        val () = $A.set<byte>(dst, di1, $A.read<byte>(src, si))
      in loop(dst, src, di + 1, si + 1, n) end
    end
  val () = loop(tbuf, src, pos, 0, len)
in pos + len end

fn _spec_set {ls:agz}
  (specs: !$A.arr(int, ls, 1024), idx: int, field: int, v: int): void = let
  val off = g1ofg0(idx * 16 + field)
in if off >= 0 then if off < 1024 then $A.set<int>(specs, off, v) end

fn _spec_get {ls:agz}
  (specs: !$A.arr(int, ls, 1024), idx: int, field: int): int = let
  val off = g1ofg0(idx * 16 + field)
in
  if off >= 0 then if off < 1024 then $A.get<int>(specs, off) else 0 else 0
end

(* ============================================================
   Implementations — Construction
   ============================================================ *)

implement parser_new {lp}{np}{lh}{nh} (name, nlen, help, hlen) = let
  val specs = $A.alloc<int>(1024)
  val tbuf = $A.alloc<byte>(8192)
  val subcmds = $A.alloc<int>(64)
  val noff = 0
  val tp = _text_write(tbuf, 0, name, nlen)
  val hoff = tp
  val tp2 = _text_write(tbuf, tp, help, hlen)
in parser_mk(specs, tbuf, subcmds, 0, tp2, 0, 0, 0, noff, nlen, hoff, hlen) end

implement parser_free(p) = let
  val+ ~parser_mk(specs, tbuf, subcmds, _, _, _, _, _, _, _, _, _) = p
in $A.free<int>(specs); $A.free<byte>(tbuf); $A.free<int>(subcmds) end

fn _add_base
  {ln:agz}{nn:pos}{lh:agz}{nh:pos}
  (p: parser, kind: int, vtype: int,
   name: !$A.borrow(byte, ln, nn), nlen: int nn,
   short_ch: int,
   help: !$A.borrow(byte, lh, nh), hlen: int nh,
   def_int: int): @(parser, int) = let
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

implement add_string {ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen, positional) = let
  val kind = if positional then 0 else 1
  val @(p2, idx) = _add_base(p, kind, 0, name, nlen, short_ch, help, hlen, 0)
in @(p2, $UNSAFE begin $UNSAFE.cast{arg(string_val)}(idx) end) end

implement add_int {ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen, def, mn, mx) = let
  val @(p2, idx) = _add_base(p, 1, 1, name, nlen, short_ch, help, hlen, def)
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p2
  val () = _spec_set(specs, idx, 10, mn)
  val () = _spec_set(specs, idx, 11, mx)
in @(parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl),
     $UNSAFE begin $UNSAFE.cast{arg(int_val)}(idx) end) end

implement add_flag {ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen) = let
  val @(p2, idx) = _add_base(p, 2, 2, name, nlen, short_ch, help, hlen, 0)
in @(p2, $UNSAFE begin $UNSAFE.cast{arg(bool_val)}(idx) end) end

implement add_count {ln}{nn}{lh}{nh} (p, name, nlen, short_ch, help, hlen) = let
  val @(p2, idx) = _add_base(p, 2, 3, name, nlen, short_ch, help, hlen, 0)
in @(p2, $UNSAFE begin $UNSAFE.cast{arg(count_val)}(idx) end) end

implement add_subcommand {ln}{nn}{lh}{nh} (p, name, nlen, help, hlen) = let
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p
  val si = sc
  val noff = tp
  val tp2 = _text_write(tbuf, tp, name, nlen)
  val hoff = tp2
  val tp3 = _text_write(tbuf, tp2, help, hlen)
  val base = g1ofg0(si * 4)
  val () = if base >= 0 then if base + 3 < 64 then let
    val () = $A.set<int>(subcmds, base, noff)
    val () = $A.set<int>(subcmds, $AR.checked_idx(base + 1, 64), nlen)
    val () = $A.set<int>(subcmds, $AR.checked_idx(base + 2, 64), hoff)
    val () = $A.set<int>(subcmds, $AR.checked_idx(base + 3, 64), hlen)
  in end
in @(parser_mk(specs, tbuf, subcmds, ac, tp3, pc, sc + 1, gc, pno, pnl, pho, phl), si) end

(* ============================================================
   Implementations — Parse
   ============================================================ *)

implement parse {la}{na} (p, argv, argv_len, argc) = let
  val+ ~parser_mk(specs, tbuf, subcmds, ac, tp, pc, sc, gc, pno, pnl, pho, phl) = p
  val str_buf = $A.alloc<byte>(8192)
  val str_meta = $A.alloc<int>(128)
  val int_vals = $A.alloc<int>(64)
  val bool_vals = $A.alloc<int>(64)
  val present = $A.alloc<int>(64)
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
  val () = $A.free<int>(subcmds)
in
  $R.ok(parse_result_mk(str_buf, str_meta, int_vals, bool_vals, present,
    ac, 0, ~1, tbuf, specs))
end

(* ============================================================
   Implementations — Extraction
   ============================================================ *)

implement get_string_len(r, h) = let
  val+ @parse_result_mk(_, smeta, _, _, _, _, _, _, _, _) = r
  val idx = $UNSAFE begin $UNSAFE.cast{int}(h) end
  val mi = g1ofg0(idx * 2 + 1)
  val len = if mi >= 0 then if mi < 128 then $A.get<int>(smeta, mi) else 0 else 0
  prval () = fold@(r)
in len end

implement get_int(r, h) = let
  val+ @parse_result_mk(_, _, ivals, _, _, _, _, _, _, _) = r
  val idx = g1ofg0($UNSAFE begin $UNSAFE.cast{int}(h) end)
  val v = if idx >= 0 then if idx < 64 then $A.get<int>(ivals, idx) else 0 else 0
  prval () = fold@(r)
in v end

implement get_bool(r, h) = let
  val+ @parse_result_mk(_, _, _, bvals, _, _, _, _, _, _) = r
  val idx = g1ofg0($UNSAFE begin $UNSAFE.cast{int}(h) end)
  val v = if idx >= 0 then if idx < 64 then $A.get<int>(bvals, idx) else 0 else 0
  prval () = fold@(r)
in $AR.gt_int_int(v, 0) end

implement get_count(r, h) = let
  val+ @parse_result_mk(_, _, _, bvals, _, _, _, _, _, _) = r
  val idx = g1ofg0($UNSAFE begin $UNSAFE.cast{int}(h) end)
  val v = if idx >= 0 then if idx < 64 then $A.get<int>(bvals, idx) else 0 else 0
  prval () = fold@(r)
in v end

implement is_present {a} (r, h) = let
  val+ @parse_result_mk(_, _, _, _, pres, _, _, _, _, _) = r
  val idx = g1ofg0($UNSAFE begin $UNSAFE.cast{int}(h) end)
  val v = if idx >= 0 then if idx < 64 then $A.get<int>(pres, idx) else 0 else 0
  prval () = fold@(r)
in $AR.gt_int_int(v, 0) end

implement get_subcmd(r) = let
  val+ @parse_result_mk(_, _, _, _, _, _, _, si, _, _) = r
  val v = si
  prval () = fold@(r)
in v end

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
