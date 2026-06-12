open! Core

type t

type color =
  | Default
  | Palette_index of int
  | Rgb of
      { r : int
      ; g : int
      ; b : int
      }
[@@deriving equal, sexp_of]

type style =
  { fg : color option
  ; bg : color option
  ; bold : bool
  ; italic : bool
  ; underline : bool
  ; blink : bool
  ; reverse : bool
  }
[@@deriving equal, sexp_of]

type cell =
  | Empty
  | Text of
      { text : string
      ; style : style
      }
[@@deriving equal, sexp_of]

type frame =
  { width : int
  ; height : int
  ; cells : cell array
  }
[@@deriving equal, sexp_of]

val frame : t -> frame
val width : t -> int
val height : t -> int
val text : attrs:Attr.t list -> string -> t
val transparent_rectangle : width:int -> height:int -> t
val rectangle : width:int -> height:int -> fill:char -> attrs:Attr.t list -> t
val hcat : t list -> t
val vcat : t list -> t
val zcat : t list -> t
val pad : ?r:int -> ?l:int -> ?t:int -> ?b:int -> t -> t
val crop : ?r:int -> ?l:int -> ?t:int -> ?b:int -> t -> t
val render_ansi : ?cols:int -> ?rows:int -> t -> string
