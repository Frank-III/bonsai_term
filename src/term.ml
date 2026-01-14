open! Core
open Async

type t =
  { term : Notty_async.Term.t
  ; bvar : (unit, read) Bvar.t
  ; pending_events : Event.Root_event.t Queue.t
  ; pipe_is_closed : unit Set_once.t
  ; time_source : Async.Time_source.t
  }

let size t =
  let%tydi { term; _ } = t in
  Notty_async.Term.size term
;;

let image t image =
  let%tydi { term; _ } = t in
  Notty_async.Term.image term image
;;

let dead t =
  let%tydi { term; _ } = t in
  Notty_async.Term.dead term
;;

let release t =
  let%tydi { term; _ } = t in
  let%bind () = Notty_async.Term.restore_title term in
  Notty_async.Term.release term
;;

let cursor t cursor =
  let%tydi { term; _ } = t in
  Notty_async.Term.cursor term cursor
;;

let set_title t title =
  let%tydi { term; _ } = t in
  Notty_async.Term.set_title term title
;;

let dimensions t =
  let width, height = size t in
  { Geom.Dimensions.width; height }
;;

module Queue = struct
  include Queue

  let dequeue_all_and_clear queue =
    match Queue.is_empty queue with
    | true -> []
    | false ->
      let list = Queue.to_list queue in
      Queue.clear queue;
      list
  ;;
end

let rec next_event_or_wait_delay t ~delay : Event.Root_event.t Nonempty_list.t Deferred.t =
  let%tydi { term = _; bvar; pending_events; pipe_is_closed = _; time_source } = t in
  match Queue.dequeue_all_and_clear pending_events with
  | hd :: tl ->
    let events = Nonempty_list.create hd tl in
    Deferred.return events
  | [] ->
    (match%bind
       Async.choose
         [ Async.choice (Bvar.wait bvar) (fun () -> `New_event)
         ; Async.choice (Time_source.after time_source delay) (fun () -> `Timer)
         ]
     with
     | `New_event -> next_event_or_wait_delay t ~delay
     | `Timer -> Deferred.return (Nonempty_list.singleton Event.Root_event.Timer))
;;

let create ?dispose ?nosig ?mouse ?bpaste ?reader ?writer ?for_mocking ~time_source () =
  let%bind term =
    Notty_async.Term.create ?mouse ?dispose ?nosig ?bpaste ?reader ?writer ?for_mocking ()
  in
  let%bind () = Notty_async.Term.save_title term in
  let pending_events = Queue.create () in
  let bvar = Bvar.create () in
  let notty_pipe = Notty_async.Term.events term in
  let pipe_is_closed = Set_once.create () in
  let[@inline always] enqueue_event event =
    Queue.enqueue pending_events event;
    Bvar.broadcast bvar ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback notty_pipe ~f:(fun event ->
       (* NOTE: We use [iter_without_pushback] here to immediately enqueue the event so
          that we do not risk accidentally dropping it. A first implementation of this
          function made use of [Pipe.read] inside of a call to [Async.choose], but this
          proved unreliable as if a different branch of the [choose] won, we could "drop"
          the event we saw.

          The alternate approach is to instead [Pipe.iter_without_pushback] to add the
          events to a queue, and then [Bvar.broadcast] to (optionally) notify the "loop"
          that there is a new event in the queue. *)
       enqueue_event (Event_conversion.notty_root_event_to_root_event event)));
  Deferred.upon (Pipe.closed notty_pipe) (fun () -> enqueue_event Stdin_closed);
  let t =
    { term; bvar :> (unit, read) Bvar.t; pending_events; pipe_is_closed; time_source }
  in
  Deferred.return t
;;
