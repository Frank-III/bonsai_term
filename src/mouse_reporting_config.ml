open! Core

type t =
  | All_mouse_events
  | All_mouse_events_except_hover
  | No_mouse_events
[@@deriving sexp, compare, equal]
