let get_default_dir () = "test1.flap"

let get_flaps () =
  let partial_map f xs =
    let prepend x xs =
      match x with
      | None -> xs
      | Some x -> x :: xs
    in
    List.fold_left (fun acc x -> prepend (f x) acc) [] xs |> List.rev |> Array.of_list
  in
  File.to_lines (get_default_dir ())
  |> partial_map (fun s ->
    if String.length s != 0
    then (
      let nth p = List.nth (String.split_on_char '|' s) p in
      Some (nth 0, nth 1))
    else None)
;;

let flap args =
  let start_time = Sys.time () in
  let flaps = get_flaps () in
  Fuzzy.find_best_opt (List.nth (List.rev !args) 1) (Array.map (fun (x, _) -> x) flaps)
  |> function
  | None -> Printf.printf "No suitable match found.\n"
  | Some i ->
    (match Webbrowser.reload (flaps.(i) |> fun (_, y) -> y) with
     | Ok () -> Printf.printf "Match found in %fs\n" (Sys.time () -. start_time)
     | Error (`Msg msg) -> Printf.printf "Link opening failed with error: %s\n" msg)
;;

let serialize_mappings flaps =
  String.concat "\n" (List.map (fun (x, y) -> x ^ "|" ^ y) (Array.to_list flaps))
;;

let confirm_update force flaps i msg token link token_link_pair =
  let update_and_notify () =
    flaps.(i) <- token, link;
    serialize_mappings flaps |> File.write (get_default_dir ());
    Printf.printf
      "Updated (%s, %s) -> (%s, %s)\n"
      (fst token_link_pair)
      (snd token_link_pair)
      token
      link
  in
  if not force
  then (
    let () = Printf.printf "%s" msg in
    flush stdout;
    try
      match input_char stdin with
      | 'y' -> update_and_notify ()
      | _ -> Printf.printf "Did not update flap.\n"
    with
    | End_of_file -> Printf.printf "End of input reached, did not update.\n"
    | Sys_error msg -> Printf.printf "System error: %s\n" msg)
  else update_and_notify ()
;;

let do_roost args force =
  let flaps = get_flaps () in
  let token, link =
    let grab i l = List.nth l i in
    List.rev !args |> fun l -> grab 1 l, grab 2 l
  in
  Fuzzy.find_best_opt (List.nth (List.rev !args) 1) (Array.map (fun (x, _) -> x) flaps)
  |> function
  | None -> File.append_line (get_default_dir ()) (token ^ "|" ^ link)
  | Some i ->
    let token_link_pair = flaps.(i) in
    if fst token_link_pair = token
    then
      confirm_update
        !force
        flaps
        i
        "Found a matching token - would you like to update it? (y/n)\n"
        token
        link
        token_link_pair
    else
      confirm_update
        !force
        flaps
        i
        (Format.sprintf "Found %s, which was a near match. Update? (y/n)\n" token)
        token
        link
        token_link_pair
;;

let find_link_opt = function
  | None -> None
  | Some s ->
    let re = Re.Perl.re {|http(s?)://(\S+)|} |> Re.compile in
    (match Re.exec_opt re s with
     | None -> None
     | Some g -> Some (Re.Group.get g 0))
;;

let roost args force =
  if List.length !args < 3
  then (
    let append_clipboard_args () =
      match Sysinfo.get_clipboard_contents () |> find_link_opt with
      | None ->
        if Sysinfo.is_macos ()
        then Printf.printf "Clipboard contents do not contain link.\n"
        else if not (Sysinfo.xclip_working ())
        then
          Printf.printf
            "xclip is not installed/supported on the current system or is otherwise \
             non-functional.\n"
      | Some x -> args := x :: !args
    in
    let () =
      if !force
      then (
        let () = append_clipboard_args () in
        Printf.printf
          "Not enough arguments provided, attemtping to copy from clipboard.\n")
      else (
        Printf.printf
          "Not enough arguments provided, should we copy from clipboard? (y/n)\n";
        flush stdout;
        try
          match input_char stdin with
          | 'y' -> append_clipboard_args ()
          | _ -> Printf.printf "Did not update flap.\n"
        with
        | End_of_file -> Printf.printf "End of input reached, did not update.\n"
        | Sys_error msg -> Printf.printf "System error: %s\n" msg)
    in
    if List.length !args < 3
    then Printf.printf "Must specify a token AND a link (owl roost <token> <link>).\n"
    else do_roost args force)
  else do_roost args force
;;

let yarp args =
  if List.length !args < 2
  then Printf.printf "Must supply a token to yarp (owl yarp <token>).\n"
  else (
    let flaps = get_flaps () in
    let token = List.nth (List.rev !args) 1 in
    Fuzzy.find_best_opt (List.nth (List.rev !args) 1) (Array.map (fun (x, _) -> x) flaps)
    |> function
    | None -> Printf.printf "No match found, nothing yarped.\n"
    | Some i ->
      let token_link_pair = flaps.(i) in
      if fst token_link_pair = token
      then (
        Array.append
          (Array.sub flaps 0 i)
          (Array.sub flaps (i + 1) (Array.length flaps - i - 1))
        |> serialize_mappings
        |> File.write (get_default_dir ());
        Printf.printf "Yarped %s.\n" token)
      else
        Printf.printf
          "No suitable match found, but the closest match was %s -> %s"
          (fst token_link_pair)
          (snd token_link_pair))
;;
