let home_dir () =
  try Sys.getenv "XDG_GET_CONFIG_HOME" with
  | Not_found ->
    (try Filename.concat (Sys.getenv "HOME") ".config" with
     | Not_found -> raise (Sys_error "Failed to locate config dir."))
;;

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
  let oc = open_out_gen [ Open_append; Open_creat; Open_text ] 0o640 file in
  Printf.fprintf oc "\n%s" str;
  close_out oc
;;

let cap_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let v = In_channel.input_all ic in
  In_channel.close ic;
  v
;;
