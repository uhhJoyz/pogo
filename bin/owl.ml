let get_default_dir () = "test.flap"

let get_flaps () =
  let partial_map f xs =
    let prepend x xs =
      match x with
      | None -> xs
      | Some x -> x :: xs
    in
    List.fold_left (fun acc x -> prepend (f x) acc) [] xs |> List.rev
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
  Fuzzy.find_best_opt (List.nth (List.rev !args) 1) (List.map (fun (x, _) -> x) flaps)
  |> function
  | None -> Printf.printf "No suitable match found.\n"
  | Some i ->
    (match Webbrowser.reload (List.nth flaps i |> fun (_, y) -> y) with
     | Ok () -> Printf.printf "Match found in %fs\n" (Sys.time () -. start_time)
     | Error (`Msg msg) -> Printf.printf "Link opening failed with error: %s\n" msg)
;;

let roost args =
  if List.length !args < 3
  then Printf.printf "Must specify a token AND a link (owl roost <token> <link>).\n"
  else (
    let flaps = get_flaps () in
    let token, link =
      let grab i l = List.nth l i in
      List.rev !args |> fun l -> grab 1 l, grab 2 l
    in
    Fuzzy.find_best_opt (List.nth (List.rev !args) 1) (List.map (fun (x, _) -> x) flaps)
    |> function
    | None -> Printf.printf "great, now we store!"
    | Some i ->
      let token_link_pair = List.nth flaps i in
      if fst token_link_pair = token
      then (* TODO: figure out how to update file without rewriting the entire thing *)
        Printf.printf
          "Updated (%s, %s) -> (%s, %s)\n"
          (fst token_link_pair)
          (snd token_link_pair)
          token
          link
      else
        Printf.printf
          "found token %s which was a near but not exact match"
          (fst token_link_pair))
;;
