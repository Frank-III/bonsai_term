open! Core

type t =
  | All_mouse_events
  | All_mouse_events_except_hover
  | No_mouse_events
[@@deriving sexp, compare, equal]

let to_notty_flags t =
  match t with
  | All_mouse_events -> ~mouse:true, ~hover:true
  | All_mouse_events_except_hover -> ~mouse:true, ~hover:false
  | No_mouse_events -> ~mouse:false, ~hover:false
;;
