open! Core

type 'exit t =
  | Exit of 'exit
  | Incoming_events_pipe_closed
  | Continue
[@@deriving sexp_of]
