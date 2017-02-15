module Stre = Str
open Parsetree
open Ast_helper
module AM = Ast_mapper

let error ppf =
  Format.kasprintf (fun s ->
    let loc = !default_loc in
    Exp.extension ~loc @@
    AM.extension_of_error (Location.error ~loc s))
    ppf

let mklid m lid =
  Exp.ident
    {Location.loc = !default_loc ; txt = Longident.(Ldot (Lident m, lid))}
let app m lid l =
  Exp.(apply (mklid m lid)
      (List.map (fun e -> Asttypes.Nolabel, e) l))

let try_int s =
  try `Int32 (Int32.of_string s)
  with _ ->
  try `Int64 (Int64.of_string s)
  with _ -> `ZInt s

let integer m s =
  match try_int s with
  | `Int32 0l -> mklid m "zero"
  | `Int32 1l -> mklid m "one"
  | `Int32 (-1l) -> mklid m "minus_one"
  | `Int32 i -> app m "of_int32" [Exp.constant @@ Const.int32 i]
  | `Int64 i -> app m "of_int64" [Exp.constant @@ Const.int64 i]
  | `ZInt s -> app m "of_string" [Exp.constant @@ Const.string s]

let integer_z = integer "Z"
let integer_q = integer "Q"

(* float_of_string doesn't error on floats that are not representable so
   we need to be careful.
   Example: 9007199254740993.0

   Instead we use a regular expression to get the pieces and recombine them.
   The format is I.FeX
   We produce I * 10^E + F * 10^k where k = E - |F|

   This fits in an integer if E >= |F|
*)

let re = Stre.regexp_case_fold
    {|^\(-?\)\([0-9]+\)\(\.\([0-9]+\)?\)?\(e\([+-]?[0-9]+\)\)?$|}
let match_float s =
  if Stre.string_match re s 0 then
    let sign = Stre.matched_group 1 s = "" in
    let i = Stre.matched_group 2 s in
    let f =
      try Some (Stre.matched_group 4 s)
      with Not_found -> None in
    let e =
      try int_of_string @@ Stre.matched_group 6 s
      with Not_found -> 0 in
    Some (sign, i, e, f)
  else
    None

let ten = app "Z" "of_int" [Exp.constant @@ Const.int 10]
let e10 n = app "Z" "pow" [ten; Exp.constant @@ Const.int n]
let add m = app m "add"
let neg_if b m x = if b then x else app m "neg" [x]

let mul_10exp a n =
  if n = 0 then `Z a
  else if n < 0 then `Q (app "Q" "make" [a; e10 (-n)])
  else `Z (app "Z" "mul" [a; e10 n])
let as_q = function `Q q -> q | `Z z -> app "Q" "of_bigint" [z]
let addx a b = match a,b with
  | `Z a, `Z b -> `Z (add "Z" [a;b])
  | `Z a, `Q b | `Q b, `Z a ->`Q (add "Q" [app "Q" "of_bigint" [a]; b])
  | `Q a, `Q b -> `Q (add "Q" [a;b])

let make_float i e f =
  let a = mul_10exp (integer_z i) e in
  match f with
  | None -> a
  | Some f ->
    let b = mul_10exp (integer_z f) (e - String.length f) in
    addx a b

let is_float_exa s =
  String.length s >= 2 &&
  let pre = String.sub s 0 2 in
  pre = "0x" || pre = "0X"
let fail_exa () =
  error "%a" Format.pp_print_text
    "Hexadecimal floating point numbers are not accepted. \
     Please use hexadecimal integers, or regular floating point numbers."

let float_z s =
  if is_float_exa s then fail_exa ()
  else match match_float s with
    | None -> error "This literal is not a valid zarith rational number."
    | Some (pos, i, e, f) ->
      match make_float i e f with
      | `Q _ -> error "This literal does not fit in an integer."
      | `Z z -> neg_if pos "Z" z

let float_q s =
  if is_float_exa s then fail_exa ()
  else match match_float s with
    | None -> error "This is not a valid zarith rational number."
    | Some (pos, i, e, f) ->
      neg_if pos "Q" @@ match make_float i e f with
      | `Q q -> q
      | `Z z -> app "Q" "of_bigint" [z]

(** Boilerplate to recognize z and q prefixes. *)

let expr mapper expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (s, Some 'z')) ->
    with_default_loc loc @@ fun () -> integer_z s
  | Pexp_constant (Pconst_float (s, Some 'z')) ->
    with_default_loc loc @@ fun () -> float_z s

  | Pexp_constant (Pconst_integer (s, Some 'q')) ->
    with_default_loc loc @@ fun () -> integer_q s
  | Pexp_constant (Pconst_float (s, Some 'q')) ->
    with_default_loc loc @@ fun () -> float_q s

  | _ -> AM.default_mapper.AM.expr mapper expr

let mapper = { AM.default_mapper with AM.expr }
let () = AM.run_main (fun _ -> mapper)
