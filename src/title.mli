open! Core

(** This is a tiny module for setting the window title.

    You can use this to change the "title" for the bonsai term app used by your terminal
    emulator / tmux. *)

(** [set_title] will attempt to set your window's title. Furthermore, when bonsai term
    apps exit, bonsai term apps will attempt to "restore" the original window title before
    the bonsai term app started, though this may not work reliably on all terminal
    emulators. Only works on the ones that support "save" and "restore" with
    ["\x1b\[22;0t"] and ["\x1b\[23;0t"]. *)
val set_title : local_ Bonsai.graph -> (string -> unit Effect.t) Bonsai.t

val register
  :  Term.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

module For_mock_tests : sig
  val register
    :  ?set_title:(string -> unit Effect.t)
    -> (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t
end
