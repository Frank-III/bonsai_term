open! Core

module Color = struct
  type t =
    | Default
    | Palette_index of int
    | Rgb of
        { r : int
        ; g : int
        ; b : int
        }
  [@@deriving equal, sexp_of]

  let rgb ~r ~g ~b =
    Rgb
      { r = Int.clamp_exn r ~min:0 ~max:255
      ; g = Int.clamp_exn g ~min:0 ~max:255
      ; b = Int.clamp_exn b ~min:0 ~max:255
      }
  ;;

  let xterm_256 index =
    if index < 0 || index > 255
    then invalid_arg [%string "Attr.Color.xterm_256: index out of range: %{index#Int}"];
    Palette_index index
  ;;

  module Expert = struct
    let black = Palette_index 0
    let red = Palette_index 1
    let green = Palette_index 2
    let yellow = Palette_index 3
    let blue = Palette_index 4
    let magenta = Palette_index 5
    let cyan = Palette_index 6
    let white = Palette_index 7
    let lightblack = Palette_index 8
    let lightred = Palette_index 9
    let lightgreen = Palette_index 10
    let lightyellow = Palette_index 11
    let lightblue = Palette_index 12
    let lightmagenta = Palette_index 13
    let lightcyan = Palette_index 14
    let lightwhite = Palette_index 15
    let default = Default
  end
end

type t =
  { fg : Color.t option
  ; bg : Color.t option
  ; bold : bool
  ; italic : bool
  ; underline : bool
  ; blink : bool
  ; invert : bool
  ; href : string option
  }
[@@deriving equal]

let empty =
  { fg = None
  ; bg = None
  ; bold = false
  ; italic = false
  ; underline = false
  ; blink = false
  ; invert = false
  ; href = None
  }
;;

let merge a b =
  { fg = Option.first_some b.fg a.fg
  ; bg = Option.first_some b.bg a.bg
  ; bold = a.bold || b.bold
  ; italic = a.italic || b.italic
  ; underline = a.underline || b.underline
  ; blink = a.blink || b.blink
  ; invert = a.invert || b.invert
  ; href = Option.first_some b.href a.href
  }
;;

let many attrs = List.fold attrs ~init:empty ~f:merge

let fg color = { empty with fg = Some color }
let bg color = { empty with bg = Some color }
let bold = { empty with bold = true }
let italic = { empty with italic = true }
let underline = { empty with underline = true }
let blink = { empty with blink = true }
let invert = { empty with invert = true }
let href url = { empty with href = Some url }

module Private = struct
  type color_repr =
    | Default
    | Palette_index of int
    | Rgb of
        { r : int
        ; g : int
        ; b : int
        }

  let color_to_repr = function
    | Color.Default -> Default
    | Palette_index index -> Palette_index index
    | Rgb { r; g; b } -> Rgb { r; g; b }
  ;;

  let fg t = t.fg
  let bg t = t.bg
  let bold t = t.bold
  let italic t = t.italic
  let underline t = t.underline
  let blink t = t.blink
  let invert t = t.invert

  let color t = t
end
