open! Core
open Async
module Start_params = Term_start_params

type t = { term : Notty_async.Term.t }

let create ~event_queue (start_params : Start_params.t) =
  let%tydi { dispose; nosig; mouse; bpaste; reader; writer; for_mocking } =
    start_params
  in
  let ~mouse, ~hover =
    Option.value mouse ~default:Mouse_reporting_config.All_mouse_events_except_hover
    |> Mouse_reporting_config.to_notty_flags
  in
  let%bind term =
    Notty_async.Term.create
      ~mouse
      ~hover
      ?dispose
      ?nosig
      ?bpaste
      ?reader
      ?writer
      ?for_mocking
      ()
  in
  let%bind () = Notty_async.Term.save_title term in
  let notty_pipe = Notty_async.Term.events term in
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
       Event_queue.enqueue_event
         event_queue
         (Event_conversion.notty_root_event_to_root_event event)));
  Deferred.upon (Pipe.closed notty_pipe) (fun () ->
    Event_queue.enqueue_event event_queue Incoming_events_pipe_closed);
  return { term }
;;

let size t =
  let width, height = Notty_async.Term.size t.term in
  { Geom.Dimensions.width; height }
;;

let render t image = Notty_async.Term.image t.term image
let has_been_released t = Notty_async.Term.dead t.term

let release t =
  let%bind () = Notty_async.Term.restore_title t.term in
  Notty_async.Term.release t.term
;;

let set_cursor t cursor =
  let cursor = Option.map cursor ~f:Types.Cursor.to_notty in
  Notty_async.Term.cursor t.term cursor
;;

let set_title t title = Notty_async.Term.set_title t.term title

let set_mouse_enabled t enabled =
  let ~mouse, ~hover = Mouse_reporting_config.to_notty_flags enabled in
  let%bind () = Notty_async.Term.set_mouse t.term mouse in
  Notty_async.Term.set_hover t.term hover
;;

let write_to_string_tty t string =
  let writer = Notty_async.Term.writer t.term in
  Writer.write writer string;
  Writer.flushed writer
;;
