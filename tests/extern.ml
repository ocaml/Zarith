(* Marshal some interesting big integers to the given file *)

let _ =
  let file = Sys.argv.(1) in
  let oc = open_out_bin file in
  for nbits = 2 to 128 do
    let x = Z.shift_left Z.one nbits in
    output_value oc x;
    output_value oc (Z.pred x);
    output_value oc (Z.neg x);
    output_value oc (Z.neg (Z.pred x))
  done;
  close_out oc

