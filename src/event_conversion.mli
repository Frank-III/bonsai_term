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

val event_to_event : terminal_event -> Event.t
val root_event_to_root_event : terminal_root_event -> _ Event.Root_event.t
