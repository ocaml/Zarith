#use "topfind";;
#require "zarith";;
#install_printer Z.pp_print ;;
#install_printer Q.pp_print ;;

(* Integers *)
0z ;;
1z ;;
-1z ;;
123z ;;
-0b010z ;;
0o147z ;;
-0O123z ;;
0x18z ;;
-0Xaaz ;;

4294967297z ;; (* 2^32+1 *)
0x100000001z ;;
-4294967297z ;;
18446744073709551617z ;; (* 2^64+1 *)
0x10000000000000001z ;;
-18446744073709551617z ;;
0x8fffffffz ;;

0q ;;
1q ;;
-1q ;;
123q ;;
-0b010q ;;
0o147q ;;
-0O123q ;;
0x18q ;;
0xaaq ;;

4294967297q ;; (* 2^32+1 *)
0x100000001q ;;
-4294967297q ;;
18446744073709551617q ;; (* 2^64+1 *)
0x10000000000000001q ;;
-18446744073709551617q ;;
0x8fffffffq ;;

(* Fail *)
0b2z ;;
0O8z ;;
0x12g8z ;;
0b2q ;;
0O8q ;;
0x12g8q ;;

(* Test error positions *)
let foo = [ Q.neg 0x12.4q ] ;;

(* Floats *)
1.z;;
1.2e10z;;
-1135.32135e50z;;

1.q;;
0.324q;;
123.545q;;
-1.5e-1q;;
1.e-4q;;
1.2e10q;;
-1135.32135e50q;;

(* Rejected *)
1.2z;;
1.e-4z;;

