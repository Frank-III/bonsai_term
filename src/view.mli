open! Core

type t

(** An empty view *)
val none : t

(** A view containing the provided text, potentially with some additional styling
    attributes for things like text/background color. *)
val text : ?attrs:Attr.t list -> string -> t

(** Checks to see if a string is accepted by Notty as UTF-8 text (i.e. it is valid UTF-8
    and contains no Unicode control characters).

    Note: [View.text] will sanitize/escape problematic input, so a [false] result here is
    mainly useful for diagnostics. *)
val is_valid_utf8 : string -> bool

(** Build a view with the given sexp as the text content. *)
val sexp_for_debugging : ?attrs:Attr.t list -> Sexp.t -> t

(** Builds a rectangle [width x height] in size, filled with the [fill] character
    (defaults to ' ') and with the given [attrs] applied. This can be useful to set the
    background color for a large area of the screen. *)
val rectangle : ?attrs:Attr.t list -> ?fill:char -> width:int -> height:int -> unit -> t

(** Builds a transparent rectangle with the given size. Transparent rectangles can be
    useful to add elements that take up space but don't impact rendering; for example, to
    add padding between items in a [vcat] *)
val transparent_rectangle : width:int -> height:int -> t

(** Vertically concatenate a list of views

    [ vcat [ a; b ] ] will put [ a ] above [ b ]. *)
val vcat : t list -> t

(** Horizontally concatenate a list of views

    [ hcat [ a; b ] ] will put [ a ] to the left of [ b ]. *)
val hcat : t list -> t

(** Stack views on top of one another.

    [ zcat [ a; b ] ] will put [ a ] on top of [ b ]. *)
val zcat : t list -> t

(** [center t ~within] will center the [ t ] view within the given box

    The dimensions of [center t ~within] will be at least as big as [within]. If
    [dimensions t] is bigger than [within], then [center ~within t] will be as big as [t] *)
val center : t -> within:Geom.Dimensions.t -> t

(** Adds padding to the edges of a view. *)
val pad : ?r:int -> ?l:int -> ?t:int -> ?b:int -> t -> t

(** Removes content from the inside of a view *)
val crop : ?r:int -> ?l:int -> ?t:int -> ?b:int -> t -> t

(** info about views *)

val dimensions : t -> Geom.Dimensions.t
val height : t -> int
val width : t -> int

(* Set all unspecified foreground and background colors in this image. If [fill_backdrop]
   is true (defaults to false), then a rectangle of solid [bg] is inserted behind the
   image. *)
val with_colors : ?fill_backdrop:bool -> t -> fg:Attr.Color.t -> bg:Attr.Color.t -> t

(* [with_colors'] is like [with_colors], but the foreground and background colors are
   optional. If either isn't passed, then they'll be unset, and could be influenced by a
   further call to [with_colors]. *)
val with_colors' : ?fill_backdrop:bool -> ?fg:Attr.Color.t -> ?bg:Attr.Color.t -> t -> t

(** [uchar_tty_width] lets you know how "wide" a unicode character is in a terminal. e.g.
    alphanumeric characters have a width of 1, but emojis (e.g. 🐹) have a tty width of 2.

    This utilitiy funtion may be useful to you if you are implementing functionality that
    needs to know how "wide" some character looks visually. (e.g. when implementing a
    "line-wrapping" algorithm) *)
val uchar_tty_width : Uchar.t -> int

(** The [Tag] API allows you to associate arbitrary information with a [View.t] and look
    it up later. For more information, see the doc comments in [Tag.For_view]. *)
module Tag : Tag.For_view with type view := t

module With_handler : sig
  type nonrec t = view:t * handler:(Event.t -> unit Effect.t)
end

module Private : sig
  val notty_image : t -> Notty.I.t
end
