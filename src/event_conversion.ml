open! Core

let notty_mouse_press_to_mouse_kind : Notty.Unescape.button -> Event.mouse_kind =
  fun x ->
  match x with
  | `Right -> Right
  | `Left -> Left
  | `Middle -> Middle
  | `Scroll x -> Scroll x
;;

let notty_mouse_kind_to_mouse_kind
  : [ `Press of Notty.Unescape.button | `Drag | `Hover | `Release ] -> Event.mouse_kind
  = function
  | `Press button -> notty_mouse_press_to_mouse_kind button
  | `Drag -> Drag
  | `Hover -> Hover
  | `Release -> Release
;;

let notty_mods_to_mods =
  List.map ~f:(function
    | `Meta -> Event.Modifier.Meta
    | `Ctrl -> Ctrl
    | `Shift -> Shift)
;;

let notty_key_kind_to_key
  :  [ `ASCII of char
     | `Arrow of [ `Down | `Left | `Right | `Up ]
     | `Backspace
     | `Delete
     | `End
     | `Enter
     | `Escape
     | `Function of int
     | `Home
     | `Insert
     | `Page of [ `Down | `Up ]
     | `Tab
     | `Uchar of Uchar.t
     ]
  -> Event.Key.t
  =
  fun x ->
  match x with
  | `Tab -> Tab
  | `Arrow x -> Arrow x
  | `Backspace -> Backspace
  | `Enter -> Enter
  | `Function x -> Function x
  | `Page x -> Page x
  | `End -> End
  | `Uchar c -> Uchar c
  | `Insert -> Insert
  | `Delete -> Delete
  | `Escape -> Escape
  | `ASCII x -> ASCII x
  | `Home -> Home
;;

let notty_event_to_event
  :  [ `Key of Notty.Unescape.key
     | `Mouse of Notty.Unescape.mouse
     | `Paste of Notty.Unescape.paste
     ]
  -> Event.t
  = function
  | `Key (kind, mods) ->
    let key = notty_key_kind_to_key kind
    and mods = notty_mods_to_mods mods in
    Key_press { key; mods }
  | `Mouse (kind, (x, y), mods) ->
    let kind = notty_mouse_kind_to_mouse_kind kind
    and position = { Geom.Position.x; y }
    and mods = notty_mods_to_mods mods in
    Mouse { kind; position; mods }
  | `Paste paste -> Paste paste
;;

let notty_root_event_to_root_event
  :  [ `Key of Notty.Unescape.key
     | `Mouse of Notty.Unescape.mouse
     | `Paste of Notty.Unescape.paste
     | `Resize of int * int
     ]
  -> 'incoming Event.Root_event.t
  = function
  | `Resize (width, height) -> Resize { Geom.Dimensions.width; height }
  | (`Paste _ | `Key _ | `Mouse _) as event ->
    let event = notty_event_to_event event in
    Event event
;;
