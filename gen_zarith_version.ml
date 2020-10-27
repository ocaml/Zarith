let () =
  let version_r = Str.regexp "^version *= *\"\\([^\"]*\\)\" *$" in
  let rec find_version_exn ic =
    let s = input_line ic in
    if Str.string_match version_r s 0
    then Str.matched_group 1 s
    else find_version_exn ic
  in
  let input = open_in "META" in
  try
    let version = find_version_exn input in
    Printf.printf "let version = %S\n" version
  with
    End_of_file ->
    close_in input;
    exit 1

