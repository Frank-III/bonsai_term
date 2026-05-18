open! Core
open Bonsai

module For_dimensions = struct
  type 'incoming t =
    { var : Geom.Dimensions.t Bonsai.Expert.Var.t
    ; term : 'incoming Term.t
    }

  let create ~term =
    let var = Bonsai.Expert.Var.create (Term.dimensions term) in
    { var; term }
  ;;

  let set { var; term = _ } dimensions = Bonsai.Expert.Var.set var dimensions

  let update ({ term; var } as t) =
    let new_dimensions = Term.dimensions term in
    if not ([%equal: Geom.Dimensions.t] (Expert.Var.get var) new_dimensions)
    then set t new_dimensions
  ;;

  let value t = Bonsai.Expert.Var.value t.var
end

module For_clock = struct
  type t = Bonsai.Time_source.t

  let create (time_source : Async.Time_source.t) =
    Bonsai.Time_source.create ~start:(Async.Time_source.now time_source)
  ;;

  let advance_to clock to_ =
    Bonsai.Time_source.advance_clock clock ~to_;
    Bonsai.Time_source.Private.flush clock
  ;;
end

module For_exit = struct
  module Status = struct
    type 'exit t =
      | Not_yet_exited
      | Exited of 'exit
  end

  type 'exit t = { mutable status : 'exit Status.t }

  let create () = { status = Not_yet_exited }
  let exit_status t = t.status

  let exit t exit =
    match t.status with
    | Not_yet_exited -> t.status <- Exited exit
    | Exited _ ->
      eprint_s
        [%message "WARNING: [exit] was called twice! Ignoring second call to exit."]
  ;;

  let exit_effect t ret = Effect.of_sync_fun (fun () -> exit t ret) ()

  let warn_if_already_exited ~(here : [%call_pos]) t =
    match t.status with
    | Not_yet_exited -> ()
    | Exited _ ->
      eprint_s
        [%message
          "Warning! [Bonsai_term.Driver.compute_frame] was called even though the bonsai \
           term app has exited. This is a bug in [bonsai_term]"
            (here : Source_code_position.t)]
  ;;
end
