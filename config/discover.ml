module C = Configurator.V1

module Version = struct
  type t = { major : int; minor : int }

  let num = function '0' .. '9' -> true | _ -> false

  let of_string_exn s : t =
    try Scanf.sscanf s "%d.%d" (fun major minor -> { major; minor })
    with _ -> failwith (Printf.sprintf "unable to parse ocaml version %S" s)

  let compare a b =
    match compare a.major b.major with 0 -> compare a.minor b.minor | n -> n
end

type libmode = [ `Auto | `Gmp | `Mpir ]

let lib : libmode ref = ref `Auto
let perf = ref false

let lookup_with_pkg_config c ~lib =
  match C.Pkg_config.get c with
  | None -> None
  | Some pc -> (
      match C.Pkg_config.query pc ~package:lib with
      | Some deps -> Some deps
      | None -> None)

let lookup_manually c ~lib ~include_h =
  let libflag = Printf.sprintf "-l%s" lib in
  let c_code =
    Printf.sprintf {|
#include <%s>"
int main() { return 1; }
|} include_h
  in
  if C.c_test c ~link_flags:[ libflag ] c_code then
    Some { C.Pkg_config.libs = [ libflag ]; cflags = [] }
  else None

let () =
  C.main
    ~args:
      [
        ( "-gmp",
          Unit (fun () -> lib := `Gmp),
          "use GMP library (default if found)" );
        ( "-mpir",
          Unit (fun () -> lib := `Mpir),
          "use MPIR library instead of GMP" );
        ("-perf", Set perf, "enable performance statistics");
      ]
    ~name:"zarith"
    (fun c ->
      let version =
        Version.of_string_exn (C.ocaml_config_var_exn c "version")
      in
      let use_legacy =
        Version.compare version { Version.major = 4; minor = 8 } < 0
      in
      let find_one ~lib ~include_h =
        match lookup_with_pkg_config c ~lib with
        | Some x -> x
        | None -> match lookup_manually c ~lib ~include_h with
          | Some x -> x
          | None -> failwith (Printf.sprintf "Unable to find the %s c library" lib)
      in

      let backend, conf =
        let rec find = function
          | `Auto -> (
              try find `Gmp with e -> ( try find `Mpir with _ -> raise e))
          | `Gmp -> (`Gmp, find_one ~lib:"gmp" ~include_h:"gmp.h")
          | `Mpir -> (`Mpir, find_one ~lib:"mpir" ~include_h:"mpir.h")
        in
        find !lib
      in
      let defs =
        match backend with `Gmp -> [ "-DHAS_GMP" ] | `Mpir -> [ "-DHAS_MPIR" ]
      in
      let defs = if !perf then "-DZ_PERF_COUNTER" :: defs else defs in
      let defs =
        if use_legacy then "DZ_OCAML_LEGACY_CUSTOM_OPERATIONS" :: defs else defs
      in

      C.Flags.write_sexp "c-flags.sexp"
        (conf.cflags @ defs (* @ [ "-O3"; "-Wall"; "-Wextra" ] *));
      C.Flags.write_sexp "c-library-flags.sexp" conf.libs)
