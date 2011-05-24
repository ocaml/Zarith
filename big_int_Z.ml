(**
   [Big_int] interface for Z module.
   
   This modules provides an interface compatible with [Big_int], but using
   [Z] functions internally.


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

type big_int = Z.t

let zero_big_int = Z.zero

let unit_big_int = Z.one

let minus_big_int = Z.neg

let abs_big_int = Z.abs

let add_big_int = Z.add

let succ_big_int = Z.succ

let add_int_big_int x y = Z.add (Z.of_int x) y

let sub_big_int = Z.sub

let pred_big_int = Z.pred

let mult_big_int  = Z.mul

let mult_int_big_int x y = Z.mul (Z.of_int x) y

let square_big_int x = Z.mul x x

let sqrt_big_int = Z.sqrt

let quomod_big_int a b =
  (* we have a = a * b + r, but [Big_int]'s remainder satisfies 0 <= r < |b|,
     while [Z]'s remainder satisfies -|b| < r < |b| and sign(r) = sign(a)
   *)
   let q,r = Z.div_rem a b in
   if Z.sign r >= 0 then q,r else
   if Z.sign b >= 0 then Z.pred q, Z.add r b
   else Z.succ q, Z.sub r b

let div_big_int a b =
   if Z.sign b >= 0 then Z.fdiv a b else Z.cdiv a b

let mod_big_int a b =
   let r = Z.rem a b in
   if Z.sign r >= 0 then r else Z.add r (Z.abs b)

let gcd_big_int = Z.gcd


(* WARNING: not tail recursive *)
let rec power a b =
  if b = 0 then Z.one else
  let aa = power a (b asr 1) in
  if b land 1 = 0 then Z.mul aa aa else Z.mul (Z.mul aa aa) a

(* WARNING: not tail recursive *)
let rec power_big a b =
  if Z.sign b = 0 then Z.one else
  let aa = power_big a (Z.shift_right b 1) in
  if Z.sign (Z.logand b Z.one) = 0 then Z.mul aa aa else Z.mul (Z.mul aa aa) a

let power_int_positive_int a b =
  if b < 0 then raise (Invalid_argument "power_int_positive_int");
  power (Z.of_int a) b

let power_big_int_positive_int a b =
  if b < 0 then raise (Invalid_argument "power_big_int_positive_int");
  power a b

let power_int_positive_big_int a b =
  if Z.sign b < 0 then raise (Invalid_argument "power_int_positive_big_int");
  power_big (Z.of_int a) b

let power_big_int_positive_big_int a b =
  if Z.sign b < 0 then raise (Invalid_argument "power_big_int_positive_big_int");
  power_big a b

let sign_big_int = Z.sign

let compare_big_int = Z.compare

let eq_big_int = Z.equal

let le_big_int a b = Z.compare a b <= 0

let ge_big_int a b = Z.compare a b >= 0

let lt_big_int a b = Z.compare a b > 0

let gt_big_int a b = Z.compare a b < 0

let max_big_int = Z.max

let min_big_int = Z.min

let num_digits_big_int = Z.size

let string_of_big_int = Z.to_string

let big_int_of_string = Z.of_string

let big_int_of_int = Z.of_int

let is_int_big_int = Z.fits_int

let int_of_big_int x = 
   try Z.to_int x with Z.Overflow -> failwith "int_of_big_int"

let big_int_of_int32 = Z.of_int32

let big_int_of_nativeint = Z.of_nativeint

let big_int_of_int64 = Z.of_int64

let int32_of_big_int x =
   try Z.to_int32 x with Z.Overflow -> failwith "int32_of_big_int"

let nativeint_of_big_int x =
   try Z.to_nativeint x with Z.Overflow -> failwith "nativeint_of_big_int"

let int64_of_big_int x =
   try Z.to_int64 x with Z.Overflow -> failwith "int64_of_big_int"

let float_of_big_int = Z.to_float

let and_big_int = Z.logand

let or_big_int = Z.logor

let xor_big_int = Z.logxor

let shift_left_big_int = Z.shift_left

let shift_right_big_int = Z.shift_right

let shift_right_towards_zero_big_int = Z.shift_right_trunc

let extract_big_int = Z.extract

let toto x = match x with `A x -> x | `B y -> Z.of_float y


