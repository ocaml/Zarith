(**
   Rationals.


   This file is part of the Zarith library
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.

   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

 *)

type t = {
    num: Z.t; (** Numerator. *)
    den: Z.t; (** Denominator, >= 0 *)
  }
(* Type of rationals.
   Invariants:
   - den is always >= 0;
   - num and den have no common factor;
   - if den=0, then num is -1, 0 or 1.
   - if num=0, then den is -1, 0 or 1.
 *)



(* creation *)
(* -------- *)

(* make *)
let mk n d =
  { num = n; den = d; }

(* make and normalize n/d, assuming d > 0 *)
let make_real n d =
  if n == Z.zero || d == Z.one then mk n Z.one
  else
    let g = Z.gcd n d in
    if g == Z.one
    then mk n d
    else mk (Z.divexact n g) (Z.divexact d g)

(* make and normalize any fraction *)
let make n d =
  let sd = Z.sign d in
  if sd = 0 then mk (Z.of_int (Z.sign n)) Z.zero else
  if sd > 0 then make_real n d else
    make_real (Z.neg n) (Z.neg d)

let of_bigint n = mk n Z.one
(* n/1 *)

let of_int n = of_bigint (Z.of_int n)

let of_int32 n = of_bigint (Z.of_int32 n)

let of_int64 n = of_bigint (Z.of_int64 n)

let of_nativeint n = of_bigint (Z.of_nativeint n)

let of_ints n d = make (Z.of_int n) (Z.of_int d)

let zero = of_bigint Z.zero
(* 0/1 *)

let one = of_bigint Z.one
(* 1/1 *)

let minus_one = of_bigint Z.minus_one
(* -1/1 *)

let inf = mk Z.one Z.zero
(* 1/0 *)

let minus_inf = mk Z.minus_one Z.zero
(* -1/0 *)

let undef = mk Z.zero Z.zero
(* 0/0 *)

let of_float d =
  if d = infinity then inf else
  if d = neg_infinity then minus_inf else
  if classify_float d = FP_nan then undef else
  let m,e = frexp d in
  (* put into the form m * 2^e, where m is an integer *)
  let m,e = Z.of_float (ldexp m 53), e-53 in
  if e >= 0 then of_bigint (Z.shift_left m e)
  else make_real m (Z.shift_left Z.one (-e))

let of_string s =
  try
    let i  = String.index s '/' in
    make
      (Z.of_substring s 0 i)
      (Z.of_substring s (i+1) (String.length s-i-1))
  with Not_found ->
    if s = "inf" || s = "+inf" then inf
    else if s = "-inf" then minus_inf
    else if s = "undef" then undef
    else of_bigint (Z.of_string s)


(* queries *)
(* ------- *)

type kind =
  | ZERO   (* 0 *)
  | INF    (* 1/0 *)
  | MINF   (* -1/0 *)
  | UNDEF  (* 0/0 *)
  | NZERO  (* non-special, non-0 *)

let classify n =
  if n.den == Z.zero then
    match Z.sign n.num with
    | 1  -> INF
    | -1 -> MINF
    | _ -> UNDEF
  else
    if n.num == Z.zero
    then ZERO
    else NZERO

let is_real n = (n.den != Z.zero)

let num x = x.num

let den x = x.den

let sign x = Z.sign x.num
(* sign undef = 0
   sign inf = 1
   sign -inf = -1
*)

let equal x y =
  (Z.equal x.num y.num) && (Z.equal x.den y.den)

let compare x y =
  match classify x, classify y with
  | UNDEF,UNDEF | INF,INF | MINF,MINF -> 0
  | UNDEF,_ -> -1
  | _,UNDEF -> 1
  | MINF,_ | _,INF -> -1
  | INF,_ | _,MINF -> 1
  | _ ->
    if x.den == y.den (* implies equality,
                         especially if immediate value and not a pointer,
                         in particular in the case den = 1 *)
    then Z.compare x.num y.num
    else
      Z.compare
        (Z.mul x.num y.den)
        (Z.mul y.num x.den)

let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

let leq a b = compare a b <= 0
let geq a b = compare a b >= 0
let lt a b = compare a b < 0
let gt a b = compare a b > 0

let to_string n =
  match classify n with
  | UNDEF -> "undef"
  | INF -> "+inf"
  | MINF -> "-inf"
  | ZERO -> "0"
  | NZERO ->
      if Z.equal n.den Z.one then Z.to_string n.num
      else (Z.to_string n.num) ^ "/" ^ (Z.to_string n.den)

let to_bigint x = Z.div x.num x.den
(* raises a Division by zero in case x is undefined or infinity *)

let to_int x = Z.to_int (to_bigint x)

let to_int32 x = Z.to_int32 (to_bigint x)

let to_int64 x = Z.to_int64 (to_bigint x)

let to_nativeint x = Z.to_nativeint (to_bigint x)



(* operations *)
(* ---------- *)

let neg x =
  mk (Z.neg x.num) x.den
(* neg undef = undef
   neg inf = -inf
   neg -inf = inf
 *)

let abs x =
  mk (Z.abs x.num) x.den
(* abs undef = undef
   abs inf = abs -inf = inf
 *)

(* addition or substraction (zaors) of finite numbers *)
let aors zaors x y =
  if x.den == y.den then  (* implies equality,
                             especially if immediate value and not a pointer,
                             in particular in the case den = 1 *)
    make_real (zaors x.num y.num) x.den
  else
    make_real
      (zaors
         (Z.mul x.num y.den)
         (Z.mul y.num x.den))
      (Z.mul x.den y.den)

let add x y =
  if x.den == Z.zero || y.den == Z.zero then match classify x, classify y with
  | ZERO,_ -> y
  | _,ZERO -> x
  | UNDEF,_ | _,UNDEF -> undef
  | INF,MINF | MINF,INF -> undef
  | INF,_ | _,INF -> inf
  | MINF,_ | _,MINF -> minus_inf
  | NZERO,NZERO -> failwith "impossible case"
  else
    aors Z.add x y
(* undef + x = x + undef = undef
   inf + -inf = -inf + inf = undef
   inf + x = x + inf = inf
   -inf + x = x + -inf = -inf
 *)

let sub x y =
  if x.den == Z.zero || y.den == Z.zero then match classify x, classify y with
  | ZERO,_ -> neg y
  | _,ZERO -> x
  | UNDEF,_ | _,UNDEF -> undef
  | INF,INF | MINF,MINF -> undef
  | INF,_ | _,MINF -> inf
  | MINF,_ | _,INF -> minus_inf
  | NZERO,NZERO -> failwith "impossible case"
  else
    aors Z.sub x y
(* sub x y = add x (neg y) *)

let mul x y =
  if x.den == Z.zero || y.den == Z.zero then
    mk
      (Z.of_int ((Z.sign x.num) * (Z.sign y.num)))
      Z.zero
  else
    make_real (Z.mul x.num y.num) (Z.mul x.den y.den)

(* undef * x = x * undef = undef
   0 * inf = inf * 0 = 0 * -inf = -inf * 0 = undef
   inf * x = x * inf = sign x * inf
   -inf * x = x * -inf = - sign x * inf
*)

let inv x =
  match Z.sign x.num with
  | 1 -> mk x.den x.num
  | -1 -> mk (Z.neg x.den) (Z.neg x.num)
  | _ ->  if x.den == Z.zero then undef else inf
(* 1 / undef = undef
   1 / inf = 1 / -inf = 0
   1 / 0 = inf

   note that: inv (inv -inf) = inf <> -inf
 *)

let div x y =
  if Z.sign y.num >= 0
  then mul x (mk y.den y.num)
  else mul x (mk (Z.neg y.den) (Z.neg y.num))
(* undef / x = x / undef = undef
   0 / 0 = undef
   inf / inf = inf / -inf = -inf / inf = -inf / -inf = undef
   0 / inf = 0 / -inf = x / inf = x / -inf = 0
   inf / x = sign x * inf
   -inf / x = - sign x * inf
   inf / 0 = inf
   -inf / 0 = -inf
   x / 0 = sign x * inf

   we have div x y = mul x (inv y)
*)

let  mul_2exp x n =
  if x.den == Z.zero then x
  else make_real (Z.shift_left x.num n) x.den

let  div_2exp x n =
  if x.den == Z.zero then x
  else make_real x.num (Z.shift_left x.den n)


(* printing *)
(* -------- *)

let print x = print_string (to_string x)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)


(* prefix and infix *)
(* ---------------- *)

let (~-) = neg
let (~+) x = x
let (+)  = add
let (-) = sub
let ( * ) = mul
let (/) = div
let (lsl) = mul_2exp
let (asr) = div_2exp
let (~$) = of_int
let (//) = of_ints
let (~$$) = of_bigint
let (///) = make
