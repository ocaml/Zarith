open Printf

let test_cases = [
  (Z.zero, None, `Big, false, "");
  (Z.zero, Some 2, `Big, false, "\x00\x00");
  (Z.zero, Some 2, `Little, true, "\x00\x00");
  (Z.one, None, `Big, false, "\x01");
  (Z.one, None, `Little, false, "\x01");
  (Z.one, None, `Big, true, "\x01");
  (Z.one, None, `Little, true, "\x01");
  (Z.of_int 127, None, `Little, true, "\x7F");
  (Z.of_int (-128), None, `Little, true, "\x80");
  (Z.of_int 0xABCD, None, `Big, false, "\xAB\xCD");
  (Z.of_int 0xABCD, None, `Little, false, "\xCD\xAB");
  (Z.of_int 0xABCD, Some 4, `Big, false, "\x00\x00\xAB\xCD");
  (Z.of_int 0xABCD, Some 4, `Little, false, "\xCD\xAB\x00\x00");
  (Z.of_int (-1234), None, `Big, true, "\xFB\x2E");
  (Z.of_int (-1234), None, `Little, true, "\x2E\xFB");
  (Z.of_int (-1234), Some 4, `Big, true, "\xFF\xFF\xFB\x2E");
  (Z.of_int (-1234), Some 4, `Little, true, "\x2E\xFB\xFF\xFF");
  (Z.of_int 0xABCD, None, `Big, true, "\x00\xAB\xCD");
  (Z.of_int 0xABCD, None, `Little, true, "\xCD\xAB\x00");
  (Z.of_string "0x123456789ABCDEF0123456789ABCDEF", None, `Big, false,
   "\x01\x23\x45\x67\x89\xAB\xCD\xEF\x01\x23\x45\x67\x89\xAB\xCD\xEF");
  (Z.of_string "0x123456789ABCDEF0123456789ABCDEF", None, `Little, false,
   "\xEF\xCD\xAB\x89\x67\x45\x23\x01\xEF\xCD\xAB\x89\x67\x45\x23\x01");
  (Z.of_string "0x123456789ABCDEF0123456789ABCDEF", Some 18, `Big, false,
   "\x00\x00\x01\x23\x45\x67\x89\xAB\xCD\xEF\x01\x23\x45\x67\x89\xAB\xCD\xEF")
]

let failure_cases = [
  (Z.of_int 0xABCD, 1, false);
  (Z.of_int 0xABCD, 2, true);
  (Z.of_int (-1), 1, false);
  (Z.of_int (-1234), 1, true);
  (Z.of_string "0x123456789ABCDEF0123456789ABCDEF", 14, false)
]

let _ =
  List.iteri
    (fun i (x, len, endian, signed, res) ->
      let s = Z.to_bytes ?len ~endian ~signed x in
      if s <> res
      then printf "Test #%d: failed\n" i;
      if Z.of_bytes ~endian ~signed res <> x
      then printf "Test #%d: round-trip failed\n" i)
    test_cases;
  List.iteri
    (fun i (x, len, signed) ->
      try
        ignore (Z.to_bytes ~len ~endian:`Big ~signed x);
        printf "Failure test #%d did not fail!\n" i
      with Invalid_argument _ | Z.Overflow -> ())
    failure_cases
