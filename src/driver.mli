open! Core
open Async

(** A [Driver.t] contains the "core" logic for driving a single cycle for the "bonsai
    loop."

    We have this intermediate abstraction so that we can "drive" bonsai term in:
    1. The "real way" when users call [Bonsai_term.start] (defined in [loop.ml])
    2. In "testing" when users use [bonsai_term_integration_test].

    importantly, because both [Bonsai_term.start] and the [bonsai_term_integration_test]
    handle use this to "drive", this allows for "integration tests" with minimal amounts
    of mocking. *)
type ('result, 'exit, 'incoming) t

val create
  :  ('result, 'exit, 'incoming, 'runtime_start_params) Start_params.t
  -> (module Runtime_intf.S with type Start_params.t = 'runtime_start_params)
  -> ('result, 'exit, 'incoming) t Deferred.t

(** [compute_first_frame] is a nuance. The "real" bonsai term calls it right after
    creation, but making it a separate function allows for the test handle to do things
    after [create], but before the first computation. *)
val compute_first_frame : _ t -> unit

(** [compute_frame] returns a "staged" deferred. You can think of this as a
    [Frame_outcome.t Deferred.t Deferred.t].

    [Deferred.bind]'ing on the outer effect will "complete" after the frame is "painted"
    (when Term.image is called). [bind]'ing again on the inner deferred will return after
    the frame is "finished" (after the next "event" (key, timer, win resize) is received
    and its events are processed by bonsai_term as specified in
    [docs/how_does_bonsai_term_work.md]). *)
val compute_frame
  :  ('result, 'exit, 'incoming) t
  -> [ `Frame_painted of [ `Frame_finished of 'exit Frame_outcome.t ] Deferred.t ]
       Deferred.t

(** Calls [Term.release] - needs to get called for cleanup purposes. *)
val release : _ t -> unit Deferred.t

(** [prev_view] returns the previous view that was drawn to the screen. Useful for expect
    test purposes. *)
val prev_view : _ t -> View.t option

val dimensions : _ t -> Geom.Dimensions.t

(** [finished t] will be determined after the driver finishes. *)
val finished
  :  ('result, 'exit, 'incoming) t
  -> ('exit, [ `Incoming_events_pipe_closed ]) Result.t Deferred.t

(** [send_event] lets you programmatically send an [Event.t] that is given to the handler
    of the bonsai term app. *)
val send_event : _ t -> Event.t -> unit

(** [send_incoming_event] lets you programmatically send a user-defined 'incoming event to
    the bonsai term app. *)
val send_incoming_event : (_, _, 'incoming) t -> 'incoming -> unit

(** [resize] lets you forcebly resize the dimensions of the bonsai term app. The intended
    use case of this is [bonsai_emacs] / [bonsai_vim] where otherwise there is no resize
    mechanism. *)
val resize : _ t -> Geom.Dimensions.t -> unit
