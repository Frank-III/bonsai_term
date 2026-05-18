open! Core

(** [Frame_outcome.t] is the result of "what should happen next" after an [Event.t] is
    processed.

    Right now, the outcome of an event can be either the app shutting down (e.g. upon an
    [End] event being received/the user pressing [Ctrl+C] OR the outcome can be to
    [continue] which is either processing the next pending event or waiting for the next
    event).

    [Incoming_events_pipe_closed] is received when the input stdin that is used for
    interactivity is closed. This effectively kills interactivity of the app. In this
    case, we will return an error, but [bonsai_term] internals still need to do some
    cleanup so we edit it here. *)
type 'exit t =
  | Exit of 'exit
  | Incoming_events_pipe_closed
  | Continue
[@@deriving sexp_of]
