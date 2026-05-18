open! Core
open Async

type 'incoming t

val create : unit -> 'incoming t
val enqueue_event : 'incoming t -> 'incoming Event.Root_event.t -> unit
val dequeue_all_and_clear : 'incoming t -> 'incoming Event.Root_event.t list
val wait_for_next_event : _ t -> unit Deferred.t
