open! Core

module Modifier = struct
  type t =
    | Meta
    | Ctrl
    | Shift
  [@@deriving sexp_of, enumerate, equal]
end

module Key = struct
  type t =
    | Escape
    | Enter
    | Tab
    | Backspace
    | Insert
    | Delete
    | Home
    | End
    | Arrow of [ `Up | `Down | `Left | `Right ]
    | Page of [ `Up | `Down ]
    | Function of int
    | Uchar of Uchar.t
    | ASCII of char
  [@@deriving equal, sexp_of]
end

type mouse_kind =
  | Left
  | Middle
  | Right
  | Scroll of [ `Up | `Down ]
  | Drag
  | Hover
  | Release
[@@deriving sexp_of, enumerate]

type t =
  | Key_press of
      { key : Key.t
      ; mods : Modifier.t list [@sexp.list]
      }
  | Mouse of
      { kind : mouse_kind
      ; position : Geom.Position.t
      ; mods : Modifier.t list [@sexp.list]
      }
  | Paste of [ `Start | `End ]
[@@deriving sexp_of]

module Root_event = struct
  type nonrec 'incoming t =
    | Event of t
    | Incoming_event of 'incoming
    | Resize of Geom.Dimensions.t
    | Incoming_events_pipe_closed
    | Timer
  [@@deriving sexp_of]
end
