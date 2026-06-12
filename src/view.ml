open! Core
open Unboxed_datatypes

module Single_memo = struct
  type ('a, 'b) t =
    { mutable prev : (#('a * 'b) Option_u.t[@kind value_or_null & value_or_null])
    ; gen : 'a -> 'b
    ; equal : 'a -> 'a -> bool
    }

  let memo ~equal gen =
    { prev = (Option_u.none [@kind value_or_null & value_or_null]) (); gen; equal }
  ;;

  let get (type a b) t input =
    let compute_and_store () =
      let output = t.gen input in
      t.prev <- (Option_u.some [@kind value_or_null & value_or_null]) #(input, output);
      output
    in
    match (t.prev : (#(a * b) Option_u.t[@kind value_or_null & value_or_null])) with
    | T #(Some, #(prev_input, prev_output)) ->
      if t.equal prev_input input then prev_output else compute_and_store ()
    | T #(None, _) -> compute_and_store ()
  ;;

  let prev_or_compute (type a b) t input =
    match (t.prev : (#(a * b) Option_u.t[@kind value_or_null & value_or_null])) with
    | T #(Some, #(_, prev_output)) -> prev_output
    | T #(None, _) -> get t input
  ;;
end

let dummy_fg_bg = Attr.empty
let memo gen = Single_memo.memo ~equal:[%equal: Attr.t] gen

type t =
  { tags : Tag.t
  ; rendered : (Attr.t, Rendered_view.t) Single_memo.t
  }

let char_code_to_safe_string_mapping =
  lazy
    (Iarray.init 256 ~f:(fun i ->
       match i with
       | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> This [%string "\\00%{i#Int}"]
       | 8 -> This "\\b"
       | 9 -> This "\\t"
       | 10 -> This "\\n"
       | 11 | 12 -> This [%string "\\0%{i#Int}"]
       | 13 -> This "\\r"
       | 14
       | 15
       | 16
       | 17
       | 18
       | 19
       | 20
       | 21
       | 22
       | 23
       | 24
       | 25
       | 26
       | 27
       | 28
       | 29
       | 30
       | 31 -> This [%string "\\0%{i#Int}"]
       | 127 -> This [%string "\\127"]
       | _ when i >= 128 && i < 160 -> This [%string "\\%{i#Int}"]
       | _ -> Null))
;;

let replace_invalid_characters string =
  (* Terminal text cannot safely contain Unicode control characters (category [Cc]), which
     includes not only ASCII controls (U+0000..U+001F and U+007F), but also the C1 control
     range U+0080..U+009F.

     Note that U+0080..U+009F are *not* single bytes in UTF-8; e.g. U+0080 encodes as
     [0xC2 0x80]. So we must sanitize at the Unicode codepoint level rather than doing a
     byte-based scan.
  *)
  let buffer = Buffer.create (String.length string) in
  let (lazy mapping) = char_code_to_safe_string_mapping in
  let string = String.Utf8.of_string_unchecked string in
  String.Utf8.to_sequence string
  |> Sequence.iter ~f:(fun uchar ->
    let scalar = Uchar.to_scalar uchar in
    match
      if scalar >= 0 && scalar < Iarray.length mapping
      then Iarray.get mapping scalar
      else Null
    with
    | This replacement -> Buffer.add_string buffer replacement
    | Null -> Buffer.add_string buffer (Uchar.Utf8.to_string uchar));
  Buffer.contents buffer
;;

let has_characters_that_need_to_be_replaced string =
  let (lazy mapping) = char_code_to_safe_string_mapping in
  let string = String.Utf8.of_string_unchecked string in
  String.Utf8.exists string ~f:(fun uchar ->
    let scalar = Uchar.to_scalar uchar in
    scalar >= 0
    && scalar < Iarray.length mapping
    && Or_null.is_this (Iarray.get mapping scalar))
;;

let text ?(attrs = []) string =
  let string =
    match has_characters_that_need_to_be_replaced string with
    | false -> string
    | true -> replace_invalid_characters string
  in
  { tags = Tag.empty
  ; rendered = memo (fun fg_bg -> Rendered_view.text ~attrs:(fg_bg :: attrs) string)
  }
;;

let text ?attrs string =
  match String.Utf8.is_valid string with
  | true -> text ?attrs string
  | false -> text ?attrs (String.Utf8.to_string (String.Utf8.sanitize string))
;;

let make_cat ~rendered_cat ~measure ~update_location ts =
  let ~rendered, ~tags, ~offset:_ =
    List.fold
      ts
      ~init:(~rendered:[], ~tags:Tag.empty, ~offset:0)
      ~f:(fun (~rendered, ~tags, ~offset) t ->
        let rendered = t.rendered :: rendered in
        let tags =
          let with_offset =
            Tag.transform_regions t.tags ~f:(fun location ->
              update_location location ~offset)
          in
          Tag.merge tags with_offset
        in
        let offset = offset + measure (Single_memo.prev_or_compute t.rendered dummy_fg_bg) in
        ~rendered, ~tags, ~offset)
  in
  { tags
  ; rendered =
      memo (fun fg_bg ->
        rendered_cat (List.rev_map rendered ~f:(fun mem -> Single_memo.get mem fg_bg)))
  }
;;

let vcat =
  make_cat
    ~rendered_cat:Rendered_view.vcat
    ~measure:Rendered_view.height
    ~update_location:(fun location ~offset -> { location with y = location.y + offset })
;;

let hcat =
  make_cat
    ~rendered_cat:Rendered_view.hcat
    ~measure:Rendered_view.width
    ~update_location:(fun location ~offset -> { location with x = location.x + offset })
;;

let zcat ts =
  (* This function doesn't use [make_cat] for two reasons:
     1. with [zcat] you don't need to update the locations
     2. we're merging the tags in the reverse order, allowing tags earlier in the list to
        take priority over tags later in the list. Maybe this is a mistake. *)
  let ~rendered, ~tags =
    List.fold
      ts
      ~init:(~rendered:[], ~tags:Tag.empty)
      ~f:(fun (~rendered, ~tags) t ->
        ~rendered:(t.rendered :: rendered), ~tags:(Tag.merge t.tags tags))
  in
  { tags
  ; rendered =
      memo (fun fg_bg ->
        Rendered_view.zcat (List.rev_map rendered ~f:(fun mem -> Single_memo.get mem fg_bg)))
  }
;;

let sexp_for_debugging ?attrs sexp =
  vcat
    (List.map ~f:(fun s -> text ?attrs s) (String.split_lines (Sexp.to_string_hum sexp)))
;;

let dimensions t =
  let rendered = Single_memo.prev_or_compute t.rendered dummy_fg_bg in
  let width = Rendered_view.width rendered
  and height = Rendered_view.height rendered in
  { Geom.Dimensions.width; height }
;;

let pad ?(r = 0) ?(l = 0) ?(t = 0) ?(b = 0) view =
  let tags =
    if l = 0 && t = 0
    then view.tags
    else
      Tag.transform_regions view.tags ~f:(fun location ->
        { location with x = location.x + l; y = location.y + t })
  in
  let rendered =
    memo (fun fg_bg -> Rendered_view.pad ~r ~l ~t ~b (Single_memo.get view.rendered fg_bg))
  in
  { tags; rendered }
;;

let transparent_rectangle ~width ~height =
  { tags = Tag.empty
  ; rendered = memo (fun _ -> Rendered_view.transparent_rectangle ~width ~height)
  }
;;

let rectangle ?(attrs = []) ?(fill = ' ') ~width ~height () =
  let rendered =
    memo (fun fg_bg ->
      Rendered_view.rectangle ~width ~height ~fill ~attrs:(fg_bg :: attrs))
  in
  { tags = Tag.empty; rendered }
;;

let center t ~within:{ Geom.Dimensions.width; height } =
  let%tydi { width = w; height = h } = dimensions t in
  let ( - ) a b = Int.max 0 (a - b) in
  let left = (width - w) / 2 in
  let right = left + ((width - w) % 2) in
  let top = (height - h) / 2 in
  let bottom = top + ((height - h) % 2) in
  pad ~r:right ~l:left ~t:top ~b:bottom t
;;

let crop ?(r = 0) ?(l = 0) ?(t = 0) ?(b = 0) view =
  let rendered =
    memo (fun fg_bg -> Rendered_view.crop ~r ~l ~t ~b (Single_memo.get view.rendered fg_bg))
  in
  let tags =
    if l = 0 && t = 0
    then view.tags
    else
      Tag.transform_regions view.tags ~f:(fun location ->
        { location with x = location.x - l; y = location.y - t })
  in
  { tags; rendered }
;;

let none =
  { tags = Tag.empty
  ; rendered = memo (fun _ -> Rendered_view.transparent_rectangle ~width:0 ~height:0)
  }
;;

let height { rendered; tags = _ } =
  Rendered_view.height (Single_memo.prev_or_compute rendered dummy_fg_bg)
;;

let width { rendered; tags = _ } =
  Rendered_view.width (Single_memo.prev_or_compute rendered dummy_fg_bg)
;;

let with_colors' ?(fill_backdrop = false) ?fg ?bg { tags; rendered = child_rendered } =
  let fg_bg =
    [ Option.map fg ~f:Attr.fg; Option.map bg ~f:Attr.bg ] |> List.filter_opt |> Attr.many
  in
  let rendered =
    memo (fun upper_fg_bg ->
      match fill_backdrop, bg with
      | true, Some _ ->
        let rendered = Single_memo.get child_rendered fg_bg in
        let width = Rendered_view.width rendered in
        let height = Rendered_view.height rendered in
        Rendered_view.zcat
          [ rendered
          ; Rendered_view.rectangle
              ~width
              ~height
              ~fill:' '
              ~attrs:[ upper_fg_bg; fg_bg ]
          ]
      | false, _ | true, None ->
        Single_memo.get child_rendered (Attr.many [ upper_fg_bg; fg_bg ]))
  in
  { tags; rendered }
;;

let with_colors ?fill_backdrop t ~fg ~bg = with_colors' ?fill_backdrop ~fg ~bg t
let uchar_tty_width = Uucp.Break.tty_width_hint

let is_valid_utf8 s =
  String.Utf8.is_valid s && not (has_characters_that_need_to_be_replaced s)
;;

module Tag = struct
  type ('key, 'data) t = ('key, 'data) Tag.Id.t

  let sexp_of_key = Tag.Id.sexp_of_key

  let create ~(here : [%call_pos]) modul ~transform_regions ~reduce =
    Tag.Id.create ~here ~transform_regions modul ~reduce
  ;;

  let mark t ~id ~key ~f =
    let region = { Geom.Region.x = 0; y = 0; width = width t; height = height t } in
    let data = f region in
    let tags = Tag.set t.tags id ~key ~data in
    { t with tags }
  ;;

  let find t ~id key = Tag.get t.tags id key
  let mem t ~id key = Tag.mem t.tags id key
  let remove t ~id ~key = { t with tags = Tag.remove t.tags id key }
  let remove_all t ~id = { t with tags = Tag.remove_all t.tags id }
  let keys t ~id = Tag.keys t.tags id
end

module With_handler = struct
  type nonrec t = view:t * handler:(Event.t -> unit Effect.t)
end

module Private = struct
  let rendered_view { rendered; tags = _ } = Single_memo.get rendered dummy_fg_bg
end
