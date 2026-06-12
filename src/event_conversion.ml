open! Core

type terminal_event =
  | Key of
      { key : Event.Key.t
      ; mods : Event.Modifier.t list
      }
  | Mouse of
      { kind : Event.mouse_kind
      ; position : Geom.Position.t
      ; mods : Event.Modifier.t list
      }
  | Paste of [ `Start | `End ]
[@@deriving sexp_of]

type terminal_root_event =
  | Terminal_event of terminal_event
  | Resize of Geom.Dimensions.t
[@@deriving sexp_of]

let event_to_event = function
  | Key { key; mods } -> Event.Key_press { key; mods }
  | Mouse { kind; position; mods } -> Event.Mouse { kind; position; mods }
  | Paste paste -> Event.Paste paste
;;

let root_event_to_root_event = function
  | Resize dimensions -> Event.Root_event.Resize dimensions
  | Terminal_event event -> Event.Root_event.Event (event_to_event event)
;;
