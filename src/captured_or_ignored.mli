open! Core

(** [Captured_or_ignored.t] is a tiny utility type meant to help libraries and apps
    represent "event capturing".

    - [ Ignored  ] represents a handler ignoring an event / it being a no-op.
    - [ Captured ] represents a handler that "used" the event.

    You can "propagate" by using the [Let_syntax] module that we have. e.g.

    {[
      fun (event : Event.t) ->
        let%bind.Captured_or_ignored () = child_handler event in
        match event with
        | Key_press { key = ASCII 'C'; mods = [ Ctrl ] } ->
          Captured_or_not.captured_effect exit
        | _ -> Captured_or_ignored.ignore_effect
    ]}

    In the above example, if [child_handler] if the above handler is handling [Ctrl-C], it
    will "win" and the parent handler will _not_ handle [ctrl-c], but if the child does
    not handle [Ctrl-C], it'll be handled by the parent.

    {[
      fun (event : Event.t) ->
        match event with
        | Key_press { key = ASCII 'D'; mods = [ Ctrl ] } ->
          Captured_or_not.capture handle_ctrl_d
        | _ ->
          let%bind.Captured_or_ignored () = child_handler event in
          (match event with
           | Key_press { key = ASCII 'C'; mods = [ Ctrl ] } ->
             Captured_or_not.capture exit
           | _ -> Captured_or_ignored.ignore)
    ]}

    In the above example, the [Ctrl-D] from the parent will be handled unconditionally.

    The semantics of the [let%bind.Captured_or_ignored] are that it will "stop" at the
    first handler that returns [Captured].

    NOTE: For debugging purposes, you can use the [sexp_of_t] which will let you know the
    location of where the event was "captured". *)
type t [@@deriving sexp_of]

(** [any ts] returns [Captured] if there is ~any [t] in [ts] that was not captured. *)
val any : t Nonempty_list.t -> t

(** NOTE: you can use [captured] and [ignore] to express the "ignored" vs. "captured"
    decisions. *)
val capture : here:[%call_pos] -> unit Effect.t -> t Effect.t

val ignore : t Effect.t
val captured : here:[%call_pos] -> unit -> t
val ignored : t

module Let_syntax : sig
  module Let_syntax : sig
    val bind : t Effect.t -> f:(unit -> t Effect.t) -> t Effect.t
    val map : t Effect.t -> f:(unit -> t) -> t Effect.t
  end
end
