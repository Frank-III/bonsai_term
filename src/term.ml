open! Core
open Async

module Packed_runtime = struct
  type t =
    | T :
        { module_ : (module Runtime_intf.S with type t = 'a)
        ; runtime : 'a
        }
        -> t
end

type 'incoming t =
  { runtime : Packed_runtime.t
  ; event_queue : 'incoming Event_queue.t
  ; time_source : Async.Time_source.t
  }

let size t =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.size runtime
;;

let image t image =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.render runtime image
;;

let dead t =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.has_been_released runtime
;;

let release t =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.release runtime
;;

let cursor t cursor =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.set_cursor runtime cursor
;;

let set_title t title =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.set_title runtime title
;;

let set_mouse t enabled =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.set_mouse_enabled runtime enabled
;;

let dimensions t = size t

let rec next_event_or_wait_delay t ~delay
  : 'incoming Event.Root_event.t Nonempty_list.t Deferred.t
  =
  let%tydi { runtime = _; event_queue; time_source } = t in
  match Event_queue.dequeue_all_and_clear event_queue with
  | hd :: tl ->
    let events = Nonempty_list.create hd tl in
    Deferred.return events
  | [] ->
    (match%bind
       Async.choose
         [ Async.choice (Event_queue.wait_for_next_event event_queue) (fun () ->
             `New_event)
         ; Async.choice (Time_source.after time_source delay) (fun () -> `Timer)
         ]
     with
     | `New_event -> next_event_or_wait_delay t ~delay
     | `Timer -> Deferred.return (Nonempty_list.singleton Event.Root_event.Timer))
;;

let enqueue_event t event = Event_queue.enqueue_event t.event_queue event

let create
  (type runtime_start_params)
  start_params
  (module Runtime : Runtime_intf.S with type Start_params.t = runtime_start_params)
  ~time_source
  ()
  =
  let event_queue = Event_queue.create () in
  let%bind runtime = Runtime.create ~event_queue start_params in
  let t =
    { runtime = T { module_ = (module Runtime); runtime }; event_queue; time_source }
  in
  Deferred.return t
;;

let write_string_to_tty t string =
  let%tydi { runtime = T { module_ = (module Runtime); runtime }; _ } = t in
  Runtime.write_to_string_tty runtime string
;;
