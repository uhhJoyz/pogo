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

let usage_msg =
  "owl [ flap <token> -- access link\n\
  \    | roost <token> <link> -- store link\n\
  \    | yarp <token> -- remove link]\n
  \    | preen <optional:token> -- show all links (or similar ones)]\n"
;;

let force = ref false
let interactive = ref false
let args = ref []

let speclist =
  [ "-f", Arg.Set force, "Skip any confirmation prompts."
  ; "-i", Arg.Set interactive, "Enter interactive mode."
  ]
;;

let anon_fun identifier = args := identifier :: !args

let print_usage () =
  Printf.printf
    "Must specify whether to \n\
     \t- flap (Find Link Access Point)\n\
     \t- roost (Redirect Or Otherwise Store Token)\n\
     \t- yarp (Yoke Adjacent bookmarks and Remove Point)
     \t- preen (preview related existing entries now) : \n\
     %s"
    usage_msg
;;

let () =
  Owl.ensure_default_dir ();
  Arg.parse speclist anon_fun usage_msg;
  if List.length !args <= 0
  then print_usage ()
  else if !interactive
  then Minttea.start app ~initial_model
  else (
    match List.nth (List.rev !args) 0 with
    | "flap" -> Owl.flap args
    | "roost" -> Owl.roost args force
    | "yarp" -> Owl.yarp args
    | "preen" -> Owl.preen args
    | _ -> print_usage ())
;;
