open! Core

(** Controls how much terminal mouse reporting is enabled. *)
type t =
  | All_mouse_events
  (** Report ordinary mouse events and hover/motion events with no button pressed. *)
  | All_mouse_events_except_hover
  (** Report ordinary mouse events, but do not report hover/motion events with no button
      pressed. *)
  | No_mouse_events
  (** Disable terminal mouse reporting, allowing terminal-native mouse selection. *)
[@@deriving sexp, compare, equal]

val to_notty_flags : t -> mouse:bool * hover:bool
