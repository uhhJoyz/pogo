open Minttea

type model = { cursor : int }

let initial_model = { cursor = 0 }
let init _model = Command.Noop

let update event model =
  match event with
  | Event.KeyDown (Key "q") -> model, Command.Quit
  | _ -> model, Command.Noop
;;

let view model =
  Format.sprintf
    {|
TODO: Implement real view function %d 

Press q to quit.
    |}
    model.cursor
;;

let app = Minttea.app ~init ~update ~view ()
let usage_msg = "owl | flap <token>\n    | roost <token> <link>"
let args = ref []
let speclist = []
let anon_fun identifier = args := identifier :: !args

let print_usage () =
  Printf.printf
    "Must specify whether to flap (find link access point) or roost (redirect or \
     otherwise store token): \n\
     %s"
    usage_msg
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  if List.length !args <= 0
  then print_usage ()
  else if List.length !args = 1
  then Minttea.start app ~initial_model
  else (
    match List.nth (List.rev !args) 0 with
    | "flap" -> Owl.flap args
    | "roost" -> Owl.roost args
    | _ -> print_usage ())
;;
