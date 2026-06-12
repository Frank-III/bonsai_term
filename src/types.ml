open! Core

module Cursor = struct
  module Kind = struct
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
end
