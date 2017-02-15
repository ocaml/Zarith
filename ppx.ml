open Parsetree
open Ast_helper
module AM = Ast_mapper

let error ~loc =
  Format.kasprintf @@ fun s ->
  Exp.extension ~loc @@
  AM.extension_of_error (Location.error ~loc s)

let mklid loc m lid =
  Exp.ident ~loc {Location.loc ; txt = Longident.(Ldot (Lident m, lid))}
let app loc m lid x =
  with_default_loc loc @@ fun () ->
  Exp.(apply (mklid loc m lid) [Asttypes.Nolabel, constant x])

let try_int s =
  try `Int32 (Int32.of_string s)
  with _ ->
  try `Int64 (Int64.of_string s)
  with _ -> `ZInt s

let integer m loc s =
  match try_int s with
  | `Int32 0l -> mklid loc m "zero"
  | `Int32 1l -> mklid loc m "one"
  | `Int32 (-1l) -> mklid loc m "minus_one"
  | `Int32 i -> app loc m "of_int32" (Const.int32 i)
  | `Int64 i -> app loc m "of_int64" (Const.int64 i)
  | `ZInt s -> app loc m "of_string" (Const.string s)

let integer_z = integer "Z"
let integer_q = integer "Q"

let is_float_exa s =
  String.length s >= 2 &&
  let pre = String.sub s 0 2 in
  pre = "0x" || pre = "0X"

(* float_of_string doesn't error on floats that are not representable so
   we need to be extra careful.
   Example: 9007199254740993.0
*)
let float_q ~loc s =
  if is_float_exa s then
    error ~loc "%a" Format.pp_print_text
      "Hexadecimal floating point numbers are not accepted. \
       Please use hexadecimal integers, or regular floating point numbers."
  else
    app loc "Q" "of_string" (Const.string s)

(** Boilerplate to recognize z and q prefixes. *)

let expr mapper expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (s, Some 'z')) ->
    integer_z loc s

  | Pexp_constant (Pconst_float (s, Some 'z')) ->
    error ~loc "This is not a valid zarith integer."

  | Pexp_constant (Pconst_integer (s, Some 'q')) ->
    integer_q loc s
  | Pexp_constant (Pconst_float (s, Some 'q')) ->
    float_q ~loc s

  | _ -> AM.default_mapper.AM.expr mapper expr

let mapper = { AM.default_mapper with AM.expr }
let () = AM.run_main (fun _ -> mapper)
