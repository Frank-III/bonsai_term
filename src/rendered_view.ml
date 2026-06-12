open! Core

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

type t = { frame : frame }

let color_of_attr color =
  match Attr.Private.color_to_repr color with
  | Default -> Default
  | Palette_index index -> Palette_index index
  | Rgb { r; g; b } -> Rgb { r; g; b }
;;

let style_of_attr attr =
  { fg = Option.map (Attr.Private.fg attr) ~f:color_of_attr
  ; bg = Option.map (Attr.Private.bg attr) ~f:color_of_attr
  ; bold = Attr.Private.bold attr
  ; italic = Attr.Private.italic attr
  ; underline = Attr.Private.underline attr
  ; blink = Attr.Private.blink attr
  ; reverse = Attr.Private.invert attr
  }
;;

let empty_frame ~width ~height =
  { width; height; cells = Array.create ~len:(width * height) Empty }
;;

let cell_index frame ~x ~y = (y * frame.width) + x
let width t = t.frame.width
let height t = t.frame.height

let set_cell frame ~x ~y cell =
  if x >= 0 && y >= 0 && x < frame.width && y < frame.height
  then frame.cells.(cell_index frame ~x ~y) <- cell
;;

let get_cell frame ~x ~y =
  if x >= 0 && y >= 0 && x < frame.width && y < frame.height
  then frame.cells.(cell_index frame ~x ~y)
  else Empty
;;

let copy_cell ~src ~dst ~src_x ~src_y ~dst_x ~dst_y =
  match get_cell src ~x:src_x ~y:src_y with
  | Empty -> ()
  | Text _ as cell -> set_cell dst ~x:dst_x ~y:dst_y cell
;;

let uchar_tty_width uchar = Int.max 0 (Uucp.Break.tty_width_hint uchar)

let string_uchars string =
  String.Utf8.of_string_unchecked string
  |> String.Utf8.to_sequence
  |> Sequence.to_list
;;

let grapheme_width text =
  let widths = List.map (string_uchars text) ~f:uchar_tty_width in
  if List.exists widths ~f:(fun width -> width = 2)
  then 2
  else List.sum (module Int) widths ~f:Fn.id
;;

let text_width text =
  Uuseg_string.fold_utf_8
    `Grapheme_cluster
    (fun width grapheme -> width + grapheme_width grapheme)
    0
    text
;;

let grapheme_slices text =
  let offset = ref 0 in
  Uuseg_string.fold_utf_8
    `Grapheme_cluster
    (fun acc grapheme ->
      let width = grapheme_width grapheme in
      let slice = Option.some_if (width > 0) (!offset, grapheme) in
      offset := !offset + width;
      match slice with
      | None -> acc
      | Some slice -> slice :: acc)
    []
    text
  |> List.rev
;;

let frame t = t.frame

let text ~attrs string =
  let attr = Attr.many attrs in
  let frame = empty_frame ~width:(text_width string) ~height:1 in
  let style = style_of_attr attr in
  List.iter (grapheme_slices string) ~f:(fun (x, text) ->
    set_cell frame ~x ~y:0 (Text { text; style }));
  { frame }
;;

let transparent_rectangle ~width ~height =
  { frame = empty_frame ~width ~height }
;;

let rectangle ~width ~height ~fill ~attrs =
  let attr = Attr.many attrs in
  let frame = empty_frame ~width ~height in
  let style = style_of_attr attr in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      set_cell frame ~x ~y (Text { text = String.of_char fill; style })
    done
  done;
  { frame }
;;

let hcat ts =
  let width = List.sum (module Int) ts ~f:width in
  let height = List.map ts ~f:height |> List.max_elt ~compare:Int.compare |> Option.value ~default:0 in
  let frame = empty_frame ~width ~height in
  let offset = ref 0 in
  List.iter ts ~f:(fun t ->
    for y = 0 to t.frame.height - 1 do
      for x = 0 to t.frame.width - 1 do
        copy_cell ~src:t.frame ~dst:frame ~src_x:x ~src_y:y ~dst_x:(!offset + x) ~dst_y:y
      done
    done;
    offset := !offset + t.frame.width);
  { frame }
;;

let vcat ts =
  let width = List.map ts ~f:width |> List.max_elt ~compare:Int.compare |> Option.value ~default:0 in
  let height = List.sum (module Int) ts ~f:height in
  let frame = empty_frame ~width ~height in
  let offset = ref 0 in
  List.iter ts ~f:(fun t ->
    for y = 0 to t.frame.height - 1 do
      for x = 0 to t.frame.width - 1 do
        copy_cell ~src:t.frame ~dst:frame ~src_x:x ~src_y:y ~dst_x:x ~dst_y:(!offset + y)
      done
    done;
    offset := !offset + t.frame.height);
  { frame }
;;

let zcat ts =
  let width = List.map ts ~f:width |> List.max_elt ~compare:Int.compare |> Option.value ~default:0 in
  let height = List.map ts ~f:height |> List.max_elt ~compare:Int.compare |> Option.value ~default:0 in
  let frame = empty_frame ~width ~height in
  List.rev ts
  |> List.iter ~f:(fun t ->
    for y = 0 to t.frame.height - 1 do
      for x = 0 to t.frame.width - 1 do
        copy_cell ~src:t.frame ~dst:frame ~src_x:x ~src_y:y ~dst_x:x ~dst_y:y
      done
    done);
  { frame }
;;

let pad ?(r = 0) ?(l = 0) ?(t = 0) ?(b = 0) view =
  let width = l + view.frame.width + r in
  let height = t + view.frame.height + b in
  let frame = empty_frame ~width ~height in
  for y = 0 to view.frame.height - 1 do
    for x = 0 to view.frame.width - 1 do
      copy_cell ~src:view.frame ~dst:frame ~src_x:x ~src_y:y ~dst_x:(l + x) ~dst_y:(t + y)
    done
  done;
  { frame }
;;

let crop ?(r = 0) ?(l = 0) ?(t = 0) ?(b = 0) view =
  let width = Int.max 0 (view.frame.width - l - r) in
  let height = Int.max 0 (view.frame.height - t - b) in
  let frame = empty_frame ~width ~height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      copy_cell ~src:view.frame ~dst:frame ~src_x:(l + x) ~src_y:(t + y) ~dst_x:x ~dst_y:y
    done
  done;
  { frame }
;;

let sgr_color ~is_fg = function
  | Default -> if is_fg then "39" else "49"
  | Palette_index index -> sprintf "%d;5;%d" (if is_fg then 38 else 48) index
  | Rgb { r; g; b } -> sprintf "%d;2;%d;%d;%d" (if is_fg then 38 else 48) r g b
;;

let sgr_params style =
  let base =
    [ Option.map style.fg ~f:(sgr_color ~is_fg:true)
    ; Option.map style.bg ~f:(sgr_color ~is_fg:false)
    ; Option.some_if style.bold "1"
    ; Option.some_if style.italic "3"
    ; Option.some_if style.underline "4"
    ; Option.some_if style.blink "5"
    ; Option.some_if style.reverse "7"
    ]
    |> List.filter_opt
  in
  if List.is_empty base then [ "0" ] else "0" :: base
;;

let add_styled_text buffer style text =
  Buffer.add_string buffer "\027[";
  Buffer.add_string buffer (String.concat (sgr_params style) ~sep:";");
  Buffer.add_string buffer "m";
  Buffer.add_string buffer text
;;

let render_frame_ansi ?cols ?rows frame =
  let cols = Option.value cols ~default:frame.width in
  let rows = Option.value rows ~default:frame.height in
  let buffer = Buffer.create (cols * Int.max rows 1) in
  for y = 0 to rows - 1 do
    if y > 0 then Buffer.add_string buffer "\r\n";
    for x = 0 to cols - 1 do
      match
        if x < frame.width && y < frame.height
        then frame.cells.(cell_index frame ~x ~y)
        else Empty
      with
      | Empty -> Buffer.add_char buffer ' '
      | Text { text; style } -> add_styled_text buffer style text
    done
  done;
  Buffer.add_string buffer "\027[0m";
  Buffer.contents buffer
;;

let render_ansi ?cols ?rows t = render_frame_ansi ?cols ?rows t.frame
