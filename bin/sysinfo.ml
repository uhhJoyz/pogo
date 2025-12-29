let is_macos () =
  match File.cap_cmd "uname" with
  | "Darwin\n" -> true
  | _ -> false
;;

let xclip_working () = Sys.command "xclip -d :0 -out -sel c" = 0

let get_clipboard_contents () =
  if is_macos ()
  then Some (File.cap_cmd "pbpaste")
  else if xclip_working ()
  then Some (File.cap_cmd "xclip -d :0 -out -sel c")
  else None
;;
