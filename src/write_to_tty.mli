open! Core

(** [Write_to_tty.write_string_to_tty] lets you write strings to the same output
    [Writer.t] that the bonsai term app is currently using.

    You can use this to write arbitrary strings to the output. This is a bit of an expert
    level API as you can print unsanitized strings outward. This function is meant for
    "components" to provide support for more custom / arbitrary / obscure OSC codes
    without necessarily needing to add support to these on bonsai_term. (e.g. OSC 52 /
    adhoc protocols) *)

val write_string_to_tty : local_ Bonsai.graph -> (string -> unit Effect.t) Bonsai.t

val register
  :  _ Term.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

module For_mock_tests : sig
  val register
    :  ?write_string_to_tty:(string -> unit Effect.t)
    -> (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t
end
