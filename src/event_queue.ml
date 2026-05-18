open! Core
open! Async

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

type 'incoming t =
  { pending_events : 'incoming Event.Root_event.t Queue.t
  ; bvar : (unit, read_write) Bvar.t
  }

let create () =
  let pending_events = Queue.create () in
  let bvar = Bvar.create () in
  { pending_events; bvar }
;;

let enqueue_event t event =
  Queue.enqueue t.pending_events event;
  Bvar.broadcast t.bvar ()
;;

let dequeue_all_and_clear t = Queue.dequeue_all_and_clear t.pending_events
let wait_for_next_event t = Bvar.wait t.bvar
