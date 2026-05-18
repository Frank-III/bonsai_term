open! Core
open Async

type 'incoming t

val create
  :  'runtime_start_params
  -> (module Runtime_intf.S with type Start_params.t = 'runtime_start_params)
  -> time_source:Time_source.t
  -> unit
  -> 'incoming t Deferred.t

val dimensions : _ t -> Geom.Dimensions.t

val next_event_or_wait_delay
  :  'incoming t
  -> delay:Time_ns.Span.t
  -> 'incoming Event.Root_event.t Nonempty_list.t Deferred.t

val image : _ t -> Notty.I.t -> unit Deferred.t
val dead : _ t -> bool
val release : _ t -> unit Deferred.t
val cursor : _ t -> Types.Cursor.t option -> unit Deferred.t
val set_title : _ t -> string -> unit Deferred.t
val set_mouse : _ t -> Mouse_reporting_config.t -> unit Deferred.t
val write_string_to_tty : _ t -> string -> unit Deferred.t
val enqueue_event : 'incoming t -> 'incoming Event.Root_event.t -> unit
