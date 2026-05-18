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

    let to_notty = function
      | Default -> `Default
      | Bar -> `Bar
      | Bar_blinking -> `Bar_blinking
      | Block -> `Block
      | Block_blinking -> `Block_blinking
      | Underline -> `Underline
      | Underline_blinking -> `Underline_blinking
    ;;
  end

  type t =
    { position : Geom.Position.t
    ; kind : Kind.t
    }

  let to_notty { position = { x; y }; kind } =
    let kind = Kind.to_notty kind in
    x, y, kind
  ;;
end
