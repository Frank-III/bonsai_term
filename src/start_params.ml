open! Core
open Bonsai
open Async

type ('result, 'exit, 'incoming, 'runtime_start_params) t =
  { runtime_start_params : 'runtime_start_params
  ; optimize : bool
  ; target_frames_per_second : int
  ; time_source : Time_source.t
  ; get_view_and_handler : 'result -> View.With_handler.t
  ; handle_incoming : 'result -> 'incoming -> unit Effect.t
  ; app :
      exit:('exit -> unit Effect.t)
      -> dimensions:Geom.Dimensions.t Bonsai.t
      -> local_ Bonsai.graph
      -> 'result Bonsai.t
  }

let sanity_check_exn
  { runtime_start_params = _
  ; time_source = _
  ; optimize = _
  ; target_frames_per_second
  ; get_view_and_handler = _
  ; handle_incoming = _
  ; app = _
  }
  =
  if target_frames_per_second < 1
  then
    raise_s
      [%message
        "Assertion failure: [target_frames_per_second < 1]"
          (target_frames_per_second : int)
          "please pick a value >= 1"]
;;

let create_exn
  ~runtime_start_params
  ~time_source
  ~optimize
  ~target_frames_per_second
  ~get_view_and_handler
  ~handle_incoming
  ~app
  =
  let optimize = Option.value ~default:true optimize
  and target_frames_per_second = Option.value ~default:60 target_frames_per_second
  and time_source = Option.value_or_thunk ~default:Time_source.wall_clock time_source in
  let out =
    { runtime_start_params
    ; time_source
    ; optimize
    ; target_frames_per_second
    ; get_view_and_handler
    ; handle_incoming
    ; app
    }
  in
  sanity_check_exn out;
  out
;;
