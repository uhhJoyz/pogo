open Minttea

type model = {
    cursor: int;
}

let initial_model = {
    cursor = 0;
}

let init _model = Command.Noop

let update event model =
    match event with
    | Event.KeyDown ((Key "q")) -> (model, Command.Quit)
    | _ -> (model, Command.Noop)

let view model =
    Format.sprintf 
    {|
TODO: Implement real view function %d 

Press q to quit.
    |} model.cursor

let app = Minttea.app ~init ~update ~view ()
let () = Minttea.start app ~initial_model
