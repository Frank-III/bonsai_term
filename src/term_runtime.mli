open! Core
include Runtime_intf.S with type Start_params.t = Term_start_params.t

module For_testing : sig
  val ghostty_snapshot : t -> (string, Ghostty_vt.error) result

  val ghostty_selection_to_string
    :  t
    -> start:Ghostty_vt.Selection.point
    -> end_:Ghostty_vt.Selection.point
    -> (string, Ghostty_vt.error) result

  val decode_string : string -> Event_conversion.terminal_event list Async.Deferred.t
end
