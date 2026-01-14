open! Core
open Unboxed_datatypes

module Single_memo = struct
  type ('a, 'b) t =
    | Constant of 'b
    | Varying of
        { mutable prev : (#('a * 'b) Option_u.t[@kind value_or_null & value_or_null])
        ; gen : 'a -> 'b
        ; equal : 'a -> 'a -> bool
        }

  let memo ~equal gen =
    Varying
      { prev = (Option_u.none [@kind value_or_null & value_or_null]) (); gen; equal }
  ;;

  let constant a = Constant a

  let get (type a b) t input =
    match t with
    | Constant t -> t
    | Varying t ->
      let compute_and_store () =
        let output = t.gen input in
        t.prev <- (Option_u.some [@kind value_or_null & value_or_null]) #(input, output);
        output
      in
      (match (t.prev : (#(a * b) Option_u.t[@kind value_or_null & value_or_null])) with
       | T #(Some, #(prev_input, prev_output)) ->
         if t.equal prev_input input then prev_output else compute_and_store ()
       | T #(None, _) -> compute_and_store ())
  ;;

  let prev_or_compute (type a b) t input =
    match t with
    | Constant t -> t
    | Varying { prev; _ } ->
      (match (prev : (#(a * b) Option_u.t[@kind value_or_null & value_or_null])) with
       | T #(Some, #(_, prev_output)) -> prev_output
       | T #(None, _) -> get t input)
  ;;
end

let dummy_fg_bg = Attr.empty
let memo = Single_memo.memo ~equal:[%equal: Attr.t]
let constant = Single_memo.constant

type t =
  { tags : Tag.t
  ; image : (Attr.t, Notty.I.t) Single_memo.t
  }

let char_code_to_safe_string_mapping =
  lazy
    (Iarray.init 256 ~f:(fun i ->
       match i with
       | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> Uopt.some [%string "\\00%{i#Int}"]
       | 8 -> Uopt.some "\\b"
       | 9 -> Uopt.some "\\t"
       | 10 -> Uopt.some "\\n"
       | 11 | 12 -> Uopt.some [%string "\\0%{i#Int}"]
       | 13 -> Uopt.some "\\r"
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
       | 31 -> Uopt.some [%string "\\0%{i#Int}"]
       | 127 -> Uopt.some [%string "\\127"]
       | _ -> Uopt.none))
;;

let replace_invalid_characters string =
  String.concat_map string ~f:(fun char ->
    let c = Char.to_int char in
    let (lazy mapping) = char_code_to_safe_string_mapping in
    match%optional.Uopt
      if c >= 0 || c < Iarray.length mapping then Iarray.get mapping c else Uopt.none
    with
    | Some string -> string
    | None -> Char.to_string char)
;;

let has_characters_that_need_to_be_replaced string =
  String.exists string ~f:(fun c ->
    let c = Char.to_int c in
    (c >= 0 && c < 32) || c = 127)
;;

let text ?(attrs = []) string =
  let T = Attr.Private.type_equal in
  let string =
    match has_characters_that_need_to_be_replaced string with
    | false -> string
    | true -> replace_invalid_characters string
  in
  { tags = Tag.empty
  ; image = memo (fun fg_bg -> Notty.I.string (Attr.many (fg_bg :: attrs)) string)
  }
;;

let text ?attrs string =
  match String.Utf8.is_valid string with
  | true -> text ?attrs string
  | false -> text ?attrs (String.Utf8.to_string (String.Utf8.sanitize string))
;;

let make_cat ~cat ~measure ~update_location ts =
  let ~images, ~tags, ~offset:_ =
    List.fold
      ts
      ~init:(~images:[], ~tags:Tag.empty, ~offset:0)
      ~f:(fun (~images, ~tags, ~offset) t ->
        let images = t.image :: images in
        let tags =
          let with_offset =
            Tag.transform_regions t.tags ~f:(fun location ->
              update_location location ~offset)
          in
          Tag.merge tags with_offset
        in
        let offset = offset + measure (Single_memo.prev_or_compute t.image dummy_fg_bg) in
        ~images, ~tags, ~offset)
  in
  { tags
  ; image =
      memo (fun fg_bg ->
        cat (List.rev_map images ~f:(fun mem -> Single_memo.get mem fg_bg)))
  }
;;

let vcat =
  make_cat
    ~cat:Notty.I.vcat
    ~measure:Notty.I.height
    ~update_location:(fun location ~offset -> { location with y = location.y + offset })
;;

let hcat =
  make_cat
    ~cat:Notty.I.hcat
    ~measure:Notty.I.width
    ~update_location:(fun location ~offset -> { location with x = location.x + offset })
;;

let zcat ts =
  (* This function doesn't use [make_cat] for two reasons:
     1. with [zcat] you don't need to update the locations
     2. we're merging the tags in the reverse order, allowing tags earlier in the list to
        take priority over tags later in the list. Maybe this is a mistake. *)
  let ~images, ~tags =
    List.fold ts ~init:(~images:[], ~tags:Tag.empty) ~f:(fun (~images, ~tags) t ->
      ~images:(t.image :: images), ~tags:(Tag.merge t.tags tags))
  in
  { tags
  ; image =
      memo (fun fg_bg ->
        Notty.I.zcat (List.rev_map images ~f:(fun mem -> Single_memo.get mem fg_bg)))
  }
;;

let sexp_for_debugging ?attrs sexp =
  vcat
    (List.map ~f:(fun s -> text ?attrs s) (String.split_lines (Sexp.to_string_hum sexp)))
;;

let dimensions t =
  let image = t.image in
  let width = Notty.I.width (Single_memo.prev_or_compute image dummy_fg_bg)
  and height = Notty.I.height (Single_memo.prev_or_compute image dummy_fg_bg) in
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
  let image =
    memo (fun fg_bg -> Notty.I.pad ~r ~l ~t ~b (Single_memo.get view.image fg_bg))
  in
  { tags; image }
;;

let transparent_rectangle ~width ~height =
  { tags = Tag.empty; image = memo (fun _ -> Notty.I.void width height) }
;;

let rectangle_impl ~width ~height ~fill ~attrs =
  let T = Attr.Private.type_equal in
  let row = Notty.I.string (Attr.many attrs) (String.init width ~f:(fun _ -> fill)) in
  Notty.I.vcat (List.init height ~f:(fun _ -> row))
;;

let rectangle ?(attrs = []) ?(fill = ' ') ~width ~height () =
  let T = Attr.Private.type_equal in
  let image =
    memo (fun fg_bg -> rectangle_impl ~width ~height ~fill ~attrs:(fg_bg :: attrs))
  in
  { tags = Tag.empty; image }
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
  let image =
    memo (fun fg_bg -> Notty.I.crop ~r ~l ~t ~b (Single_memo.get view.image fg_bg))
  in
  let tags =
    if l = 0 && t = 0
    then view.tags
    else
      Tag.transform_regions view.tags ~f:(fun location ->
        { location with x = location.x - l; y = location.y - t })
  in
  { tags; image }
;;

let none = { tags = Tag.empty; image = memo (fun _ -> Notty.I.empty) }

let height { image; tags = _ } =
  Notty.I.height (Single_memo.prev_or_compute image dummy_fg_bg)
;;

let width { image; tags = _ } =
  Notty.I.width (Single_memo.prev_or_compute image dummy_fg_bg)
;;

let with_colors' ?(fill_backdrop = false) ?fg ?bg { image; tags } =
  let fg_bg =
    [ Option.map fg ~f:Attr.fg; Option.map bg ~f:Attr.bg ] |> List.filter_opt |> Attr.many
  in
  let build_image upper_fg_bg =
    match fill_backdrop, bg with
    | true, Some _ ->
      let image = Single_memo.get image fg_bg in
      let width = Notty.I.width image in
      let height = Notty.I.height image in
      Notty.I.zcat
        [ image; rectangle_impl ~width ~height ~fill:' ' ~attrs:[ upper_fg_bg; fg_bg ] ]
    | false, _ | true, None -> Single_memo.get image (Attr.many [ upper_fg_bg; fg_bg ])
  in
  let image =
    match fg, bg with
    | Some _, Some _ ->
      (* if both foreground and background are set in this call, then the resulting image
         is constant *)
      constant (build_image Attr.empty)
    | _ -> memo build_image
  in
  { tags; image }
;;

let with_colors ?fill_backdrop t ~fg ~bg = with_colors' ?fill_backdrop ~fg ~bg t
let uchar_tty_width = Notty.Tty_width_hint.tty_width_hint

let is_valid_utf8 s =
  try
    ignore (Notty.I.string Notty.A.empty s : Notty.I.t);
    true
  with
  | _ -> false
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
  let notty_image { image; tags = _ } = Single_memo.get image dummy_fg_bg
end
