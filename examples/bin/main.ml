let integers () =
  (* Build something bigger than an OCaml int can hold. *)
  let z = Z.of_int max_int in
  let bigz = Z.mul z z in
    print_endline (Z.to_string bigz);
  (* Go even bigger *)
  let biggerz = Z.shift_left z 100 in
    print_endline (Z.to_string biggerz);
    Printf.printf "This has %i significant bits, of which %i are set\n" (Z.numbits biggerz) (Z.popcount biggerz);
    Printf.printf "Does it fit in an Int64.t? %b\n" (Z.fits_int64 biggerz);
    (* Logarithm (as an int, because it is small) *)
    Printf.printf "Logarithm, base two: %i\n" (Z.log2 biggerz);
    (* Prime just larger than this (probably) *)
    Printf.printf "Next prime... %s\n" (Z.to_string (Z.nextprime biggerz));
    (* There are alternative printers: *)
    print_endline "Decimal:";
    print_endline (Z.format "%i" biggerz);
    print_endline "Octal:";
    print_endline (Z.format "%o" biggerz);
    print_endline "Binary:";
    print_endline (Z.format "%b" biggerz);
    print_endline "Hexadecimal:";
    print_endline (Z.format "%0#X" biggerz)

let rationals () =
  (* 1 / 100 *)
  let q = Q.make (Z.of_int 1) (Z.of_int 100) in
  (* Or, more easily 1 / 1000 *)
  let q2 = Q.of_ints 1 1000 in
  (* Or, even more easily 1 / 10000 *)
  let open Q in
  let q3 = 1 // 10000 in
  (* Or, from a string *)
  let q4 = Q.of_string "1/10000" in
    (* Show parts *)
    Printf.printf "Parts of q3: %s, %s\n" (Z.to_string q3.num) (Z.to_string q3.den);
    (* Printing *)
    Q.print q3;
    print_newline ();
    (* Arithmetic *)
    let q5 = Q.mul q3 q4 in
      print_endline "Multiplied:";
      Q.print q5;
      print_newline ();
      Printf.printf "As a float: %f\n" (Q.to_float q4)

let () =
  match Sys.argv with
  | [|_; "integers"|] -> integers ()
  | [|_; "rationals"|] -> rationals ()
  | _ -> Printf.eprintf "zarith example: unknown command line\n"
