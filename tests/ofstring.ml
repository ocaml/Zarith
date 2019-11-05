let pow2 n =
  let rec doit acc n =
    if n<=0 then acc else doit (Z.add acc acc) (n-1)
  in
  doit Z.one n

let p30 = pow2 30
let p62 = pow2 62
let p300 = pow2 300
let p120 = pow2 120
let p121 = pow2 121

let test_of_string_Z () =
  let round_trip_Z () =
    let round_trip fmt x=
      (Z.equal (Z.of_string (Z.format fmt x)) x)
    in
    let formats = [
      "%i"; "%#b"; "%#o"; "%#x"; "%#X";
      "%+i"; "%#+b"; "%#+o"; "%#+x"; "%#+X";
      "%+0i"; "%#+0b"; "%#+0o"; "%#+0x"; "%#+0X";
    ] in
    let numbers =
      let (+) = Z.add in
      let  l = [p30; p62; p30 + p62; p300; p120; p121] in
      l @ (List.map Z.neg l)
    in
    List.iter
      (fun fmt ->
         assert
           (
             List.for_all
               (fun x -> round_trip fmt x)
               numbers
           )
      )
      formats
  in
  let fail d f x =
    try
      ignore (f x);
      Printf.printf "%s should fail on %s\n" d x
    with _ -> ()
  in
  let succ d f x y =
    try
      let z = f x in
      if Z.equal z  y
      then ()
      else
        Printf.printf
          "%s(%s) returned %s, expected %s\n"
          d
          x
          (Z.to_string z)
          (Z.to_string y)
    with _ ->
      Printf.printf "%s failed. Expected %s\n" d (Z.to_string y)
  in

  round_trip_Z ();

  fail "Z.of_string" Z.of_string "0b2";
  fail "Z.of_string" Z.of_string "0o8";
  fail "Z.of_string" Z.of_string "0xg";
  fail "Z.of_string" Z.of_string "0xG";
  fail "Z.of_string" Z.of_string "0A";
  succ "Z.of_string" Z.of_string "" Z.zero;
  succ "Z.of_string" Z.of_string "+" Z.zero;
  succ "Z.of_string" Z.of_string "-" Z.zero;
  succ "Z.of_string" Z.of_string "0x" Z.zero;
  succ "Z.of_string" Z.of_string "0b" Z.zero;

  fail "Z.of_substring" (Z.of_substring ~pos:1 ~len:2) "0b2";
  fail "Z.of_substring" (Z.of_substring ~pos:1 ~len:2) "0o8";
  fail "Z.of_substring" (Z.of_substring ~pos:1 ~len:2) "0xg";
  fail "Z.of_substring" (Z.of_substring ~pos:1 ~len:2) "0xG";
  fail "Z.of_substring" (Z.of_substring ~pos:1 ~len:1) "0A";
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:0) "+" Z.zero;
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:1) "-+" Z.zero;
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:2)"--1-" (Z.minus_one);
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:2)"--1\000" (Z.minus_one);
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:2)"\000-1\000" (Z.minus_one);
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:1)"00b1" Z.zero;
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:2)"00b1" Z.zero;
  succ "Z.of_substring" (Z.of_substring ~pos:1 ~len:3)"00b1" Z.one;

  let s = Z.format "%#b" p120 in
  let n = String.length s in
  for i = 0 to n - 3 do
    succ "Z.of_substring"
      (Z.of_substring ~pos:0 ~len:(n - i))
      s
      (Z.shift_right p120 i)
  done

let _ = test_of_string_Z ()

let test_of_string_Q () =
  let round_trip_Q () =
    let round_trip fmt x=
      let os = Q.of_string (Z.to_string x) in
      let ob = Q.of_bigint x in
      if Q.equal os ob then
        true
      else begin
          Format.printf "%a not equal to %a\n" Q.pp_print os Q.pp_print ob;
          false
        end
    in
    let formats = [
      "%i"; "%#b"; "%#o"; "%#x"; "%#X";
      "%+i"; "%#+b"; "%#+o"; "%#+x"; "%#+X";
      "%+0i"; "%#+0b"; "%#+0o"; "%#+0x"; "%#+0X";
    ] in
    let numbers =
      let (+) = Z.add in
      let  l = [p30; p62; p30 + p62; p300; p120; p121] in
      (l @ (List.map Z.neg l))
    in
    List.iter
      (fun fmt ->
         assert
           (
             List.for_all
               (fun x -> round_trip fmt x)
               numbers
           )
      )
      formats
  in
  let fail d f x =
    try
      let s = f x in
      Printf.printf "%s should fail on %s. Got %s\n" d x (Q.to_string s)
    with _ -> ()
  in
  let succ d f x y =
    try
      let z = f x in
      if Q.equal z  y
      then ()
      else
        Printf.printf
          "%s(%s) returned %s, expected %s\n"
          d
          x
          (Q.to_string z)
          (Q.to_string y)
    with exc ->
      Printf.printf "%s failed. Expected %s. Got %s\n" d (Q.to_string y)
                    (Printexc.to_string exc)
  in

  round_trip_Q ();

  fail "Q.of_string" Q.of_string "0b2";
  fail "Q.of_string" Q.of_string "0o8";
  fail "Q.of_string" Q.of_string "0xg";
  fail "Q.of_string" Q.of_string "0xG";
  fail "Q.of_string" Q.of_string "0A";
  succ "Q.of_string" Q.of_string "" Q.zero;
  succ "Q.of_string" Q.of_string "+" Q.zero;
  succ "Q.of_string" Q.of_string "-" Q.zero;
  succ "Q.of_string" Q.of_string "0x" Q.zero;
  succ "Q.of_string" Q.of_string "0b" Q.zero;

  fail "Q.of_string" Q.of_string "0b2";
  fail "Q.of_string" Q.of_string "0o8";
  fail "Q.of_string" Q.of_string "0xg";
  fail "Q.of_string" Q.of_string "0xG";
  fail "Q.of_string" Q.of_string "0A";
  fail "Q.of_string" Q.of_string "-0b0.1e1";
  fail "Q.of_string" Q.of_string "-0o0.1E1";
  fail "Q.of_string" Q.of_string "-0b0.1P1";
  fail "Q.of_string" Q.of_string "-0o0.1p1";
  fail "Q.of_string" Q.of_string "-0.1P1";
  fail "Q.of_string" Q.of_string "-0.1p1";
  succ "Q.of_string" Q.of_string "0x1e2" (Q.of_int 482);
  succ "Q.of_string" Q.of_string "1e2" (Q.of_int 100);
  succ "Q.of_string" Q.of_string "+" Q.zero;
  succ "Q.of_string" Q.of_string "-+" Q.zero;
  succ "Q.of_string" Q.of_string "-1" Q.minus_one;
  succ "Q.of_string" Q.of_string "+0xFF.8" (Q.of_float 255.5);
  succ "Q.of_string" Q.of_string "+0xff.8" (Q.of_float 255.5);
  succ "Q.of_string" Q.of_string "-0xFF.8" (Q.of_float (-255.5));
  succ "Q.of_string" Q.of_string "-0xff.8" (Q.of_float (-255.5));
  succ "Q.of_string" Q.of_string "-0.1e1" (Q.minus_one);
  succ "Q.of_string" Q.of_string "-0.1E1" (Q.minus_one);
  succ "Q.of_string" Q.of_string "-0x0.1P1" (Q.minus_one);
  succ "Q.of_string" Q.of_string "-0x0.1p1" (Q.minus_one);
  succ "Q.of_string" Q.of_string "6.674e-11" (Q.of_string "0.00000000006674")

let _ = test_of_string_Q ()
