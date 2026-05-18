open! Core

(** [notty_root_event_to_root_event] is our translation layer between "notty" events and
    our own [Event.t] types. *)
val notty_root_event_to_root_event
  :  [ `Key of Notty.Unescape.key
     | `Mouse of Notty.Unescape.mouse
     | `Paste of Notty.Unescape.paste
     | `Resize of int * int
     ]
  -> _ Event.Root_event.t
