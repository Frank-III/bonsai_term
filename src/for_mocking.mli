open! Core
open Async

type t

(** [create] allows tests to mock terminal dimensions and tty behavior without depending
    on a specific terminal runtime implementation. *)
val create
  :  dimensions:(Core_unix.File_descr.t -> (int * int) option)
  -> wait_for_next_window_change:(unit -> unit Deferred.t)
  -> is_a_tty:(Fd.t -> bool Deferred.t)
  -> t

val dimensions : t -> Core_unix.File_descr.t -> (int * int) option
val wait_for_next_window_change : t -> unit -> unit Deferred.t
val is_a_tty : t -> Fd.t -> bool Deferred.t
