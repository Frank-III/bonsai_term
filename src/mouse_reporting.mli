open! Core

(** Dynamically control terminal mouse reporting.

    [All_mouse_events] enables ordinary mouse events and hover/motion events with no
    button pressed. [All_mouse_events_except_hover] enables ordinary mouse events but
    disables hover events. [No_mouse_events] disables terminal mouse reporting, which is
    important for allowing terminal-native text selection.

    This is exposed via dynamic scope, similar to {!Cursor.set_cursor_position}. *)

val set_mouse_reporting
  :  local_ Bonsai.graph
  -> (Mouse_reporting_config.t -> unit Ui_effect.t) Bonsai.t

val register
  :  _ Term.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

module For_mock_tests : sig
  val register
    :  (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t
end
