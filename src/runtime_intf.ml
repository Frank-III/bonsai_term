open! Core
open Async

(** [Runtime_intf.S] is a tiny module that provides the "backend" for [bonsai_term].

    The "implementation" that bonsai_term uses lives in the [term_runtime] module. This
    module will let us have alternate runtime implementations for bonsai term, like an
    ansi code parser / serializer that is different from notty, and also new backends for
    bonsai_term (e.g. [bonsai_emacs], and [bonsai_vim] (and also maybe eventually
    [bonsai_vscode] and [bonsai_term_web])) *)

module type S = sig
  module Start_params : T

  type t

  val create : event_queue:'incoming Event_queue.t -> Start_params.t -> t Deferred.t
  val size : t -> Geom.Dimensions.t
  val render : t -> Notty.image -> unit Deferred.t
  val has_been_released : t -> bool
  val release : t -> unit Deferred.t
  val set_cursor : t -> Types.Cursor.t option -> unit Deferred.t
  val set_title : t -> string -> unit Deferred.t
  val set_mouse_enabled : t -> Mouse_reporting_config.t -> unit Deferred.t
  val write_to_string_tty : t -> string -> unit Deferred.t
end
