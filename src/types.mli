open! Core

(** This [Types] module contains the types that would otherwise cause cyclical
    dependencies. It does not contain ~all of the type definitions. *)

module Cursor : sig
  module Kind : sig
    type t =
      | Default
      | Bar
      | Bar_blinking
      | Block
      | Block_blinking
      | Underline
      | Underline_blinking
  end

  type t =
    { position : Geom.Position.t
    ; kind : Kind.t
    }

  val to_notty
    :  t
    -> int
       * int
       * [ `Default
         | `Bar
         | `Bar_blinking
         | `Block
         | `Block_blinking
         | `Underline
         | `Underline_blinking
         ]
end
