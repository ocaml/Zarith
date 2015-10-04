(* to compile:
   ocamlopt -I .. -ccopt "-L.." zarith.cmxa nums.cmxa -o $timing.exe timing.ml
*)

open Format

let size = 8192

(* test en 64 bits *)

let rec z_random nbbits =
  if nbbits <= 62 then
    Z.of_int64 @@ Random.int64 Int64.(shift_left one nbbits)
  else
    let z0 = Z.of_int64 @@ Random.int64 Int64.(shift_left one 62) in
    Z.add z0
      (Z.shift_left (z_random (nbbits-62)) 62)

let z_random ~signed nbbits =
  let z = z_random nbbits in
  if not signed then z else
  if (Random.int 2) = 0 then z else Z.neg z

let q_random ~signed nbbits =
  Q.make
    (z_random ~signed nbbits)
    (Z.succ (z_random ~signed:false nbbits))

let test1 str op nb tab1 =
  let t1 = Sys.time () in
  for i=0 to nb-1 do
    for j=0 to size-1 do
      ignore (op tab1.(j))
    done;
  done;
  let t2 = Sys.time () in
  printf "%s (%4i times) = %.2f@." str nb (t2 -. t1)

let test2 str op nb tab1 tab2 =
  let t1 = Sys.time () in
  for i=0 to nb-1 do
    for j=0 to size-1 do
      ignore (op tab1.(j) tab2.(j))
    done;
  done;
  let t2 = Sys.time () in
  printf "%s (%4i times) = %.2f@." str nb (t2 -. t1)

let test3 str op nb tab1 tab2 tab3 =
  let t1 = Sys.time () in
  for i=0 to nb-1 do
    for j=0 to size-1 do
      ignore (op tab1.(j) tab2.(j) tab3.(j))
    done;
  done;
  let t2 = Sys.time () in
  printf "%s (%4i times) = %.2f@." str nb (t2 -. t1)

let test nbbits =  
  printf "NBBITS : %i@." nbbits;
  (* div and divexact *)
  let tabden = Array.init size (fun _ -> Z.succ (z_random ~signed:false (nbbits/2))) in
  let tabnum = Array.init size (fun i -> Z.mul tabden.(i) (z_random ~signed:true (nbbits/2))) in
  test2 "Z.div           " Z.div 5000 tabnum tabden;
  test2 "Z.divexact      " Z.divexact 5000 tabnum tabden;
  (* Q *)
  let tab1 = Array.init size (fun _ -> q_random ~signed:true (nbbits/2)) in
  let tab2 = Array.init size (fun _ -> q_random ~signed:true (nbbits/2)) in
  test2 "Q.add           " Q.add 200 tab1 tab2;
  test2 "Q.mul           " Q.mul 200 tab1 tab2;
  (* powm *)
  let tabbase = Array.init size (fun _ -> z_random ~signed:true (nbbits/4)) in
  let tabexp = Array.init size (fun _ -> Z.succ (z_random ~signed:false 10)) in
  let tabmod = Array.init size (fun _ -> Z.succ 
                                   (Z.shift_left 
                                      (z_random ~signed:false (nbbits-1)) 
                                      1)) 
  in
  test3 "Z.powm          " Z.powm 100 tabbase tabexp tabmod;
  test3 "Z.powm_sec      " Z.powm_sec 100 tabbase tabexp tabmod;
  (* pow *)
  let tabbase = Array.init size (fun _ -> z_random ~signed:true (nbbits/4)) in
  let tabexp = Array.init size (fun _ -> Random.int 10) in
  test2 "Z.pow           " Z.pow 1000 tabbase tabexp;
  (* root *)
  let tabbase = Array.init size (fun _ -> z_random ~signed:false nbbits) in
  let tabexp = Array.init size (fun _ -> 1 + (Random.int 10)) in
  test2 "Z.root          " Z.root 100 tabbase tabexp;
  (* perfect power and other unary functions *)
  let tab = Array.init size (fun _ -> z_random ~signed:false nbbits) in
  test1 "Z.perfect_power " Z.perfect_power 100 tab;
  test1 "Z.perfect_square" Z.perfect_square 1000 tab;
  test1 "Z.probab_prime 5" (fun z -> Z.probab_prime z 5) 10 tab;
  test1 "Z.next_prime    " Z.nextprime 1 tab;
  ()

let _ =
  Random.init 0;
  printf "random array(s) size : %i@." size;
  test 60;
  test 120;
  test 500;
  test 2000;
  ()






(*
128 bits * (factor 0 <= 1 lsl 29)

orig: 1.104 / 2.78
caml_Z.c: 1.068 / 0.776
caml_Z.S: 1.076 / 0.776

32 bits * (factor 0 <= 1 lsl 29)

orig: 0.236 / 0.26
caml_Z.c: 0.24 / 0.25
caml_Z.S: 0.24 / 0.24
*)
(*
ERREUR dans q.ml div_2exp
*)
