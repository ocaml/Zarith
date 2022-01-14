(* A simple executable to figure out where is our libgmp.a.
 *
 * This executable wants to generate an environment which contains:
 * - `CC` as the compiler
 * - `CFLAGS` and `CPPFLAGS` as C options
 * - `LDFLAGS` as options at the link time
 *
 * If [--with-conf-gmp] (related to the virtual OPAM package [conf-gmp]) is
 * set, the script will just set `CC` and compile Zarith with the **host**'s
 * `libgmp.a`.
 *
 * If [--with-gmp=<path>] is set (related to the [gmp] package), Zarith will be
 * compile with the location of the [gmp] package. However, [gmp] can be
 * located into an OPAM switch (if the <path> is absolute) or a local
 * directory. The second case appears when you use [opam monorepo] which pulls
 * dependencies into a [duniverse] local directory.
 *
 * The second case appears for the MirageOS 4.0 support too when we want to use
 * a cross-compiled version of `libgmp.a` which should be available into our
 * source-tree (compiled by `dune`).
 *
 * This script wants to help us to compile Zarith in these contexts:
 * - as a simple OPAM dependency (which will be installed into a switch)
 * - as a dependency brought by [opam monorepo]
 * - in the situation where we use [opam monorepo] and the cross-compilation
 *)

let always x _ = x

let deadbeef = "\xde\xad\xbe\xef"
let cc = ref deadbeef
let gmp_path = ref deadbeef
let with_conf_gmp = ref false

let dir_sep_char = '/'
let is_relative p = p.[0] <> dir_sep_char
let ( / ) = Filename.concat

let split s =
  let min = 0 and max = max_int and sat chr = chr <> dir_sep_char in
  if min > max || max = 0 then (s, "") else
  let len = String.length s in
  let max_idx = len - 1 in
  let min_idx = let k = len - max in (if k < 0 then 0 else k) in
  let need_idx = max_idx - min in
  let rec loop i =
    if i >= min_idx && sat s.[i] then loop (i - 1) else
    if i > need_idx || i = max_idx then (s, "") else
    if i = -1 then ("", s) else
    let cut = i + 1 in
    String.sub s 0 cut, String.sub s cut (len - cut)
  in
  loop max_idx

let is_prefix ~affix s =
  let len_a = String.length affix in
  let len_s = String.length s in
  if len_a > len_s then false else
  let max_idx_a = len_a - 1 in
  let rec loop i =
    if i > max_idx_a then true else
    if affix.[i] <> s.[i] then false else loop (i + 1)
  in
  loop 0

let is_prefix ~prefix p =
  if not (is_prefix ~affix:prefix p) then false else
  let suff_start = String.length prefix in
  if prefix.[suff_start - 1] = dir_sep_char then true else
  if suff_start = String.length p then (* suffix empty *) true else
  p.[suff_start] = dir_sep_char

let spec =
  [ "--with-gmp", Arg.Set_string gmp_path, "Location of libgmp.a"
  ; "--with-conf-gmp", Arg.Set with_conf_gmp, "Use the host's libgmp.a"
  ; "--cc", Arg.Set_string cc, "C compiler" ]

let usage = Format.asprintf "%s --cc <compiler> [--with-gmp=<path>] [--with-conf-gmp]\n%!" Sys.argv.(0)

let where () = match !gmp_path, !with_conf_gmp with
  | gmp_path, _ when gmp_path <> deadbeef ->
    let gmp_path, _libgmp_a = split gmp_path in
    let cwd = Sys.getcwd () in
    if is_relative gmp_path || is_prefix ~prefix:cwd gmp_path
    then `Source (cwd / gmp_path)
    else `Switch gmp_path
  | _, true -> `Host
  | _, false -> `Missing

let env = function
  | `Source gmp_path when is_relative gmp_path ->
    let gmp_path = Sys.getcwd () / gmp_path in
    Format.asprintf "CC=\"%s\" LDFLAGS=\"-L%s\" CFLAGS=\"-I%s\" CPPFLAGS=\"-I%s\""
      !cc gmp_path gmp_path gmp_path
  | `Source gmp_path
  | `Switch gmp_path ->
    Format.asprintf "CC=\"%s\" LDFLAGS=\"-L%s\" CFLAGS=\"-I%s\" CPPFLAGS=\"-I%s\""
      !cc gmp_path gmp_path gmp_path
  | `Host ->
    Format.asprintf "CC=\"%s\"" !cc
  | `Missing -> failwith "Zarith requires gmp."

let () =
  Arg.parse spec (always ()) usage ;
  if !cc = deadbeef
  then ( Format.eprintf "%s%!" usage ; exit 1 ) ;
  let where = where () in
  let env = env where in
  Format.printf "%s%!" env
  (* XXX(dinosaure): we must **not** append '\n'. Otherwise,
   * we don't set the environment. *)
