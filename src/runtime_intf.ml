open! Core
open Async

(** [Runtime_intf.S] is a tiny module that provides the "backend" for [bonsai_term].

    The "implementation" that bonsai_term uses lives in the [term_runtime] module. This
    module will let us have alternate runtime implementations for bonsai_term, such as
    editor buffers, terminal emulators, and future UI hosts. *)

module type S = sig
  module Start_params : T

  type t

  val create : event_queue:'incoming Event_queue.t -> Start_params.t -> t Deferred.t
  val size : t -> Geom.Dimensions.t
  val render : t -> Rendered_view.t -> unit Deferred.t
  val has_been_released : t -> bool
  val release : t -> unit Deferred.t
  val set_cursor : t -> Types.Cursor.t option -> unit Deferred.t
  val set_title : t -> string -> unit Deferred.t
  val set_mouse_enabled : t -> Mouse_reporting_config.t -> unit Deferred.t
  val write_to_string_tty : t -> string -> unit Deferred.t
end
