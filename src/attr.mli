open! Core

type t [@@deriving equal]

val empty : t
val many : t list -> t

module Color : sig
  type t [@@deriving equal, sexp_of]

  (** NOTE: The below colors are "ANSI" "true" 24-bit colors.

      https://en.wikipedia.org/wiki/ANSI_escape_code#Colors *)
  val rgb : r:int -> g:int -> b:int -> t

  (** [xterm_256 index] is the xterm 256-color palette entry with index [0..255]. This
      uses the terminal's palette (i.e. [38;5;<index>] / [48;5;<index>] escape sequences),
      matching palette-based UIs like jane_curses.

      @raise Invalid_argument if [index] is outside [0..255]. *)
  val xterm_256 : int -> t

  module Expert : sig
    (* NOTE: These colors are the "user-default" colors and using these can result in
       unreadable / hard-to-read colors, please prefer to use [rgb] isntead. Only use
       these if you absolutely must. (e.g. if you are embedding other terminal UIs that do
       in fact embed these colors) *)
    val black : t
    val red : t
    val green : t
    val yellow : t
    val blue : t
    val magenta : t
    val cyan : t
    val white : t
    val lightblack : t
    val lightred : t
    val lightgreen : t
    val lightyellow : t
    val lightblue : t
    val lightmagenta : t
    val lightcyan : t
    val lightwhite : t
    val default : t
  end
end

(** [fg color] sets the "foreground color" (i.e. the color of the letters) to [color]. *)
val fg : Color.t -> t

(** [bg color] sets the "background color" to [color]. *)
val bg : Color.t -> t

val bold : t
val italic : t
val underline : t
val blink : t
val invert : t

(** [href url] creates a clickable hyperlink to [url]. *)
val href : string -> t

module Private : sig
  val type_equal : (t, Notty.A.t) Type_equal.t
end
