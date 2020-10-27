let archname = ref ""
let noalloc = ref true
let version = ref "VERSION"
let usage = "Usage: './z_pp architecture"

let () =
  Arg.parse
    ["-noalloc", Arg.Set noalloc, "noalloc attribute available";
     "-ov", Arg.Set_string version, "ocaml compiler version"]
    (fun name -> archname := name)
    usage;
  if !archname = ""
  then begin
      print_endline usage;
      exit 1
    end

let noalloc_str = if !noalloc then "[@@noalloc]" else "\"noalloc\""

let asmfilename = "caml_z_" ^ !archname ^ ".S"

module StringSet = Set.Make(String)

let funcnames = ref StringSet.empty

let () =
  let rPROLOG = Str.regexp "[ 	]*PROLOG(\\([^)]*\\))" in
  let input = open_in asmfilename in
  Printf.printf "found assembly file %s\n" asmfilename;
  try
    while true do
      let s = input_line input in
      if Str.string_match rPROLOG s 0
      then
	let funcname = Str.matched_group 1 s in
	Printf.printf "  found %s\n" funcname;
	funcnames := StringSet.add funcname !funcnames
    done
  with
    End_of_file ->
      close_in input


let treat_file =
  let rASM = Str.regexp "\\(.*\\) \\([A-Za-z0-9_]+\\)@ASM\\(.*\\)" in
  let funcnames = !funcnames in
  function extension ->
    let outputname = "z." ^ extension in
    let inputname = outputname ^ "p" in

    let input = open_in inputname in
    let output = open_out outputname in
    Printf.fprintf output
      "(* This file was automatically generated by z_pp.ml from %s *)\n"
      inputname;
    try
      while true do
	let line_in = input_line input in
        let line_in = Str.(global_replace (regexp "@VERSION") (Printf.sprintf "%S" !version) line_in) in
	let line_out =
	  if Str.string_match rASM line_in 0
	  then
	    let funcname = Str.matched_group 2 line_in in
	    if StringSet.mem funcname funcnames
	    then Str.replace_matched
	      "\\1 \"ml_z_\\2\" \"ml_as_z_\\2\"\\3"
	      line_in
	    else
	      Str.replace_matched
		"\\1 \"ml_z_\\2\"\\3"
		line_in
	  else line_in
	in
	Printf.fprintf output "%s\n" line_out
      done
    with
      End_of_file ->
	close_in input
;;

let generate_config filename =
  let oc = open_out filename in
  StringSet.iter
    (fun f -> Printf.fprintf oc "#define Z_ASM_%s\n" f)
    !funcnames;
  close_out oc
;;

let _ = treat_file "ml"
let _ = treat_file "mli"
let _ = generate_config "z_features.h"
