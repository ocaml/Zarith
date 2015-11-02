(* Testing Z.to_float *)

open Printf

type rounding_mode =
  FE_DOWNWARD | FE_TONEAREST | FE_TOWARDZERO | FE_UPWARD

external setround: rounding_mode -> bool = "caml_ztest_setround"

external format_float: string -> float -> string = "caml_format_float"

let hex_of_float f = format_float "%a" f

(* For testing, we use randomly-generated integers of the form
     <signed 64-bit integer> * 2^<exponent>
   We can predict their FP value by converting the integer part to FP,
   then scale by the exponent using ldexp. *)

let test1 (mant: int64) (exp: int) =
  let expected = ldexp (Int64.to_float mant) exp in
  let actual = Z.to_float (Z.shift_left (Z.of_int64 mant) exp) in
  if actual = expected then true else begin
    printf "%Ld * 2^%d: expected %s, got %s\n"
           mant exp (hex_of_float expected) (hex_of_float actual);
    false
  end

let testN numrounds =
  printf " (%d tests)... %!" numrounds;
  let errors = ref 0 in
  (* Some random int64 values *)
  for i = 1 to numrounds do
    let m = Random.int64 Int64.max_int in
    if not (test1 m 0) then incr errors;
    if not (test1 (Int64.neg m) 0) then incr errors
  done;
  (* Some random int64 values scaled by some random power of 2 *)
  for i = 1 to numrounds do
    let m1 = Random.bits() in           (* 30 bits *)
    let m2 = Random.bits() in           (* 30 bits *)
    let m3 = Random.bits() in
    let m = Int64.(logor (of_int m1)
                         (logor (shift_left (of_int m2) 30)
                                (shift_left (of_int m3) 60))) in
    let exp = Random.int 1100 in        (* sometimes +inf will result *)
    if not (test1 m exp) then incr errors
  done;
  (* Special test close to a rounding point *)
  for i = 0 to 15 do
    let m = Int64.(add 0xfffffffffffff0L (of_int i)) in
    if not (test1 m 32) then incr errors;
    if not (test1 (Int64.neg m) 32) then incr errors
  done;
  if !errors = 0
  then printf "passed\n%!"
  else printf "FAILED (%d errors)\n%!" !errors

let _ =
  let numrounds =
    if Array.length Sys.argv >= 2
    then int_of_string Sys.argv.(1)
    else 100_000 in
  printf "Default rounding mode";
  testN numrounds;
  if setround FE_TOWARDZERO then begin
    printf "Round toward zero";
    testN numrounds
  end else begin
    printf "Round toward zero not supported, skipping\n"
  end;
  if setround FE_DOWNWARD then begin
    printf "Round downward";
    testN numrounds
  end else begin
    printf "Round downward not supported, skipping\n"
  end;
  if setround FE_UPWARD then begin
    printf "Round upward";
    testN numrounds
  end else begin
    printf "Round upward not supported, skipping\n"
  end;
  if setround FE_TONEAREST then begin
    printf "Round to nearest";
    testN numrounds
  end else begin
    printf "Round to nearest not supported, skipping\n"
  end




