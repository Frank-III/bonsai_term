open! Core

module Kind : sig
  type t = Types.Cursor.Kind.t =
    | Default
    | Bar
    | Bar_blinking
    | Block
    | Block_blinking
    | Underline
    | Underline_blinking
  [@@deriving sexp_of, equal]
end

type t = Types.Cursor.t =
  { position : Geom.Position.t
  ; kind : Kind.t
  }
[@@deriving sexp_of, equal]

val set_cursor_position : local_ Bonsai.graph -> (t option -> unit Ui_effect.t) Bonsai.t

val register
  :  _ Term.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

module For_mock_tests : sig
  val register
    :  (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t
end
