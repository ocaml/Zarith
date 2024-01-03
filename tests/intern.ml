(* Unmarshal big integers from the given file, and report errors *)

open Printf

let expect ic n =
  try
    let m = (input_value ic : Z.t) in
    if Z.equal m n then printf " OK" else printf " Wrong"
  with Failure _ ->
    printf " Fail"

let _ =
  let file = Sys.argv.(1) in
  let ic = open_in_bin file in
  for nbits = 16 to 128 do
    printf "%d:" nbits;
    let x = Z.shift_left Z.one nbits in
    expect ic (Z.pred (Z.neg x));
    expect ic (Z.neg x);
    expect ic (Z.pred x);
    expect ic x;
    print_newline()
  done;
  close_in ic
