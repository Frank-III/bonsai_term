open! Core
open Async

type t =
  { dimensions : Core_unix.File_descr.t -> (int * int) option
  ; wait_for_next_window_change : unit -> unit Deferred.t
  ; is_a_tty : Fd.t -> bool Deferred.t
  }

let create ~dimensions ~wait_for_next_window_change ~is_a_tty =
  { dimensions; wait_for_next_window_change; is_a_tty }
;;

let dimensions t = t.dimensions
let wait_for_next_window_change t = t.wait_for_next_window_change
let is_a_tty t = t.is_a_tty
