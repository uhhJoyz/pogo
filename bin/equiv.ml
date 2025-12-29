type lax_equivalence =
  | Equivalent
  | Congruent
  | Divergent

(* defines equivalence classes for congruence *)
let eq_map =
  let equivalences =
    [ 'a', [ '@'; '4' ]
    ; 's', [ '$'; '5' ]
    ; 'l', [ '!'; '1'; 'i'; '7'; '|'; '/'; '\\' ]
    ; 'v', [ '^' ]
    ; 'e', [ '3' ]
    ; 'b', [ '6'; '8' ]
    ; 'o', [ '0' ]
    ; '.', [ ',' ]
    ; '\'', [ '"'; '`' ]
    ; '_', [ '-'; '=' ]
    ; ':', [ ';' ]
    ; '(', [ '['; '{' ]
    ; ')', [ ']'; '}' ]
    ]
  in
  let eq =
    Hashtbl.create
      ~random:false
      (List.fold_left (fun acc (_, l) -> acc + List.length l) 0 equivalences)
  in
  let add_class b = List.iter (fun v -> Hashtbl.add eq v b) in
  let build_classmap = List.iter (fun (b, l) -> add_class b l) in
  let () = build_classmap equivalences in
  eq
;;

let congruent a b =
  if a = b
  then Equivalent
  else if
    let find_or_fallback v =
      Hashtbl.find_opt eq_map v
      |> function
      | None -> v
      | Some x -> x
    in
    find_or_fallback a = find_or_fallback b
  then Congruent
  else Divergent
;;

let to_int = function
    | Equivalent -> 2
    | Congruent -> 1
    | _ -> -1
