let write file str =
  let oc = open_out file in
  Printf.fprintf oc "%s" str;
  close_out oc
;;

let to_weak op file =
  let ic = open_in file in
  try
    let v = op ic in
    close_in ic;
    v
  with
  | e ->
    close_in_noerr ic;
    raise e
;;

let to_string file = to_weak In_channel.input_all file
let to_lines file = to_weak In_channel.input_lines file

let append_line file str = 
    let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o640 file in
    Printf.fprintf oc "\n%s" str;
    close_out oc
