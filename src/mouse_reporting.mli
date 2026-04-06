open! Core

(** Dynamically enable/disable terminal mouse reporting.

    When mouse reporting is enabled, the terminal will send mouse events on stdin.
    Disabling mouse reporting is important for allowing terminal-native text selection.

    This is exposed via dynamic scope, similar to {!Cursor.set_cursor_position}. *)

val set_mouse_reporting : local_ Bonsai.graph -> (bool -> unit Ui_effect.t) Bonsai.t

val register
  :  Term.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

module For_mock_tests : sig
  val register
    :  (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t
end
