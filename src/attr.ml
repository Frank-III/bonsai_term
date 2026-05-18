open! Core

type t = Notty.A.t [@@deriving equal]

let many attrs = List.fold attrs ~init:Notty.A.empty ~f:Notty.A.( ++ )
let bold = Notty.A.st Notty.A.bold
let italic = Notty.A.st Notty.A.italic
let underline = Notty.A.st Notty.A.underline
let blink = Notty.A.st Notty.A.blink
let invert = Notty.A.st Notty.A.reverse
let empty = many []

module Color = struct
  type t = Notty.A.color

  let sexp_of_t t =
    Notty.A.Private.color_to_repr t
    |> [%sexp_of: [ `Default | `Palette_index of int | `Rgb_888 of int * int * int ]]
  ;;

  let equal =
    (* Notty.A.color is an int under the hood, so phys_equal is fast and correct *)
    phys_equal
  ;;

  let rgb ~r ~g ~b =
    let r = Int.clamp_exn r ~min:0 ~max:255 in
    let g = Int.clamp_exn g ~min:0 ~max:255 in
    let b = Int.clamp_exn b ~min:0 ~max:255 in
    Notty.A.rgb_888 ~r ~g ~b
  ;;

  let xterm_256 index =
    if index < 0 || index > 255
    then invalid_arg [%string "Attr.Color.xterm_256: index out of range: %{index#Int}"];
    if index < 8
    then (
      match index with
      | 0 -> Notty.A.black
      | 1 -> Notty.A.red
      | 2 -> Notty.A.green
      | 3 -> Notty.A.yellow
      | 4 -> Notty.A.blue
      | 5 -> Notty.A.magenta
      | 6 -> Notty.A.cyan
      | 7 -> Notty.A.white
      | _ -> assert false)
    else if index < 16
    then (
      match index with
      | 8 -> Notty.A.lightblack
      | 9 -> Notty.A.lightred
      | 10 -> Notty.A.lightgreen
      | 11 -> Notty.A.lightyellow
      | 12 -> Notty.A.lightblue
      | 13 -> Notty.A.lightmagenta
      | 14 -> Notty.A.lightcyan
      | 15 -> Notty.A.lightwhite
      | _ -> assert false)
    else if index >= 232
    then (
      let level = index - 232 in
      Notty.A.gray level)
    else (
      let cube_range = index - 16 in
      let r = cube_range / 36 in
      let gb_range = cube_range - (r * 36) in
      let g = gb_range / 6 in
      let b = gb_range - (g * 6) in
      Notty.A.rgb ~r ~g ~b)
  ;;

  module Expert = struct
    let black = Notty.A.black
    let red = Notty.A.red
    let green = Notty.A.green
    let yellow = Notty.A.yellow
    let blue = Notty.A.blue
    let magenta = Notty.A.magenta
    let cyan = Notty.A.cyan
    let white = Notty.A.white
    let lightblack = Notty.A.lightblack
    let lightred = Notty.A.lightred
    let lightgreen = Notty.A.lightgreen
    let lightyellow = Notty.A.lightyellow
    let lightblue = Notty.A.lightblue
    let lightmagenta = Notty.A.lightmagenta
    let lightcyan = Notty.A.lightcyan
    let lightwhite = Notty.A.lightwhite
    let default = Notty.A.default
  end
end

let fg = Notty.A.fg
let bg = Notty.A.bg
let href url = Notty.A.href ~url

module Private = struct
  let type_equal : (t, Notty.A.t) Type_equal.t = T
end
