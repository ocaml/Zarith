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

(* The current implementation supports plain decimals, decimal points,
   scientific notation ('e' or 'E' for base 10 litteral and 'p' or 'P'
   for base 16), and fraction of integers (eg. 1/2). In particular it
   accepts any numeric litteral -without underscores ('_')- accepted
   by OCaml's lexer.
   Restrictions:
   - does not handle '_' as their removal should probably be common to
     Z.of_string and Q.of_string
   - exponents in scientific notation should fit on an integer
   - scientific notation only available in hexa and decimal (as in OCaml) *)
let of_string s =
  (* splits a (non-empty) string into a sign and the rest of the string *)
  let split_sign s =
    match s.[0] with
    | ('-' | '+') as c -> c,(String.sub s 1 (String.length s-1))
    | _ -> '+',s
  in
  (* splits a (non-empty) string into a base converted to int and the
     rest of the string *)
  let split_base s =
    if String.length s < 2 then 10,s
    else
      match String.sub s 0 2 with
      | "0x" -> 16,String.sub s 2 (String.length s-2)
      | "0o" -> 8,String.sub s 2 (String.length s-2)
      | "0b" -> 2,String.sub s 2 (String.length s-2)
      | _ -> 10,s
  in
  (* plain decimals handling *)
  let default b s = of_bigint (Z.of_string_base b s) in
  (* point decimals handling. Also shifts the '.' char to the right by
     [shift] characters and complete with some '0' if needed. [shift]
     is alwayas 0 except when called from scientific_notation *)
  let of_decimal_point b s shift =
    try
      let i =  String.index s '.' in
      let size_dec = String.length s-i-1 in
      let rmv_dot = (String.sub s 0 i)^(String.sub s (i+1) size_dec) in
      if size_dec > shift then
        make (Z.of_string_base b rmv_dot) (Z.pow (Z.of_int b) (size_dec-shift))
      else
        default b (rmv_dot^(String.make (shift-size_dec) '0'))
    with Not_found -> default b (s^String.make shift '0')
  in
  let of_scientific_notation s =
    let sign,s' = split_sign s in
    let b,s = split_base s' in
    let i = ref 0 in
    try
      String.iter
        (fun c ->
          incr i;
          match c with
          | 'e' | 'E' ->
             if b=10 then raise Exit
             else if b = 16 then ()
             else invalid_arg "Q.of_string: scientific notations have \
                               to be in decimal or hexadecimal"
          | 'p' | 'P' -> if b=16 then raise Exit else invalid_arg "Q.of_string"
          | _ -> ()) s;
      (* not a scientific notation *)
      if sign='+' then of_decimal_point b s 0 else of_decimal_point b ("-"^s) 0
    with
    | Exit ->
       let m = String.sub s 0 (!i-1) in
       let e = Z.of_string_base b (String.sub s !i (String.length s- !i)) in
       if sign='+' then of_decimal_point b m (Z.to_int e)
       else of_decimal_point b ("-"^m) (Z.to_int e)
  in
  let of_ratio s =
    try
      let i  = String.index s '/' in
      make
        (Z.of_substring s 0 i)
        (Z.of_substring s (i+1) (String.length s-i-1))
    with Not_found -> of_scientific_notation s
  in
  if s = "" then zero
  else if s = "inf" || s = "+inf" then inf
  else if s = "-inf" then minus_inf
  else if s = "undef" then undef
  else of_ratio s

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
  (Z.equal x.num y.num) && (Z.equal x.den y.den) && (classify x <> UNDEF)

let compare x y =
  match classify x, classify y with
  | UNDEF,UNDEF | INF,INF | MINF,MINF -> 0
  | UNDEF,_ -> -1
  | _,UNDEF -> 1
  | MINF,_ | _,INF -> -1
  | INF,_ | _,MINF -> 1
  | _ ->
    if x.den = y.den (* implies equality,
                        especially if immediate value and not a pointer,
                        in particular in the case den = 1 *)
    then Z.compare x.num y.num
    else
      Z.compare
        (Z.mul x.num y.den)
        (Z.mul y.num x.den)

let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b


let leq x y =
  match classify x, classify y with
  | UNDEF,_ | _,UNDEF -> false
  | MINF,_ | _,INF -> true
  | INF,_ | _,MINF -> false
  | _ ->
     if x.den = y.den
     then Z.leq x.num y.num
     else
       Z.leq
         (Z.mul x.num y.den)
         (Z.mul y.num x.den)

let lt x y =
  match classify x, classify y with
  | UNDEF,_ | _,UNDEF -> false
  | INF,_ | _,MINF -> false
  | MINF,_ | _,INF -> true
  | _ ->
     if x.den = y.den
     then Z.lt x.num y.num
     else
       Z.lt
         (Z.mul x.num y.den)
         (Z.mul y.num x.den)

let geq x y = leq y x
let gt x y = lt y x

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

let to_float x =
  match classify x with
  | ZERO -> 0.0
  | INF  -> infinity
  | MINF -> neg_infinity
  | UNDEF -> nan
  | NZERO ->
    let p = x.num and q = x.den in
    let np = Z.numbits p and nq = Z.numbits q in
    if np <= 53 && nq <= 53 then
      (* p and q convert to floats exactly; use FP division to get the
         correctly-rounded result. *)
      Int64.to_float (Z.to_int64 p) /. Int64.to_float (Z.to_int64 q)
    else begin
      let negat =
        if Z.lt p Z.zero then -1 else 1
      in
      (* p is in [2^(np-1), 2^np)
         q is in [2^(nq-1), 2^nq)
         We define n,p',q' such that p'/q'*2^n=p/q and |p'/q'| is in [1, 2). *)
      let n = np - nq in
      (* Scaling p/q by 2^n *)
      let (p', q') =
        if n >= 0
        then p, Z.shift_left q n
        else Z.shift_left p (-n), q
      in
      let (p', n) =
        if Z.geq (Z.abs p') q'
        then (p', n)
        else (Z.shift_left p' 1, pred n)
      in
      (* If we divided p' by q' now, the resulting quotient would
         have one significant digit. *)
      let p' = Z.shift_left p' 54 in
      (* When we divide p' by q' next, the resulting quotient will
         have 55 significant digits. The strategy is:
         - First, compute the quotient with 55 significant digits in
           round-to-odd, and
         - Second, round that number to the number of effective
           significant digits we desire for the result, which is 53
           for a normal result and less than 53 for a subnormal result.
         We cannot afford an intermediate rounding at 53 significant digits
         if the end-result is subnormal. See
         https://github.com/ocaml/Zarith/issues/29 *)
      (* Euclidean division of p' by q' *)
      let (quo, rem) = Z.ediv_rem p' q' in
      if n <= -1080
      then
        (* The end result is +0.0 or -0.0 (depending on negat)
           or perhaps the next floating-point number of the same
           sign (depending on the current rounding mode. *)
        ldexp (float_of_int negat) (-1080)
      else
        let offset =
          if n <= -1023
          then
            (* The end result will be subnormal, add an offset
               to make the rounding happen directly at the place
               where it should happend.
               quo has the form:       1xxxx...
               we add:               1000000...
               so as to end up with: 101xxxx... *)
            Z.shift_left (Z.of_int negat) (55 + (-1023 - n))
          else
            Z.zero
        in
        let quo = Z.add offset quo in
        let quo =
          if Z.sign rem = 0
          then quo
          else Z.(lor) Z.one quo (* round to odd *)
        in
        (* The FPU rounding mode affects the Z.to_float that comes next,
           making the rounding computed according to the current FPU rounding
           mode. *)
        let f = Z.to_float quo in
        (* The subtraction that comes next is exact, so that the rounding
           mode does not change what it does. *)
        let f = f -. (Z.to_float offset)
        in
        (* ldexp is also exact and unaffected by the rounding mode.
           We have made sure that if the end result is going to be subnormal,
           then f has exactly the correct number of significant digits for
           no rounding to happen here. *)
        ldexp f (n - 54)
    end

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
let (=) = equal
let (<) = lt
let (>) = gt
let (<=) = leq
let (>=) = geq
let (<>) a b = not (equal a b)
