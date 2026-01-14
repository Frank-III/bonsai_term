open! Core
open Async

type t

val create
  :  ?dispose:bool
  -> ?nosig:bool
  -> ?mouse:bool
  -> ?bpaste:bool
  -> ?reader:Reader.t
  -> ?writer:Writer.t
  -> ?for_mocking:Notty_async.For_mocking.t
  -> time_source:Time_source.t
  -> unit
  -> t Deferred.t

val dimensions : t -> Geom.Dimensions.t

val next_event_or_wait_delay
  :  t
  -> delay:Time_ns.Span.t
  -> Event.Root_event.t Nonempty_list.t Deferred.t

val image : t -> Notty.I.t -> unit Deferred.t
val dead : t -> bool
val release : t -> unit Deferred.t

type cursor :=
  [ `Default
  | `Bar
  | `Bar_blinking
  | `Block
  | `Block_blinking
  | `Underline
  | `Underline_blinking
  ]

val cursor : t -> (int * int * cursor) option -> unit Deferred.t
val set_title : t -> string -> unit Deferred.t
