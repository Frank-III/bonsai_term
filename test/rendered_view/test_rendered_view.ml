open! Core

module Attr = Bonsai_term.Attr
module Rendered_view = Bonsai_term.Expert.Rendered_view
module View = Bonsai_term.View

let show_frame frame =
  printf "%dx%d\n" frame.Rendered_view.width frame.height;
  Array.iteri frame.cells ~f:(fun index cell ->
    match cell with
    | Empty -> ()
    | Text { text; style } ->
      let x = index % frame.width in
      let y = index / frame.width in
      printf
        "%d,%d %S bold=%b underline=%b fg=%s\n"
        x
        y
        text
        style.bold
        style.underline
        (match style.fg with
         | None -> "default"
         | Some Default -> "default"
         | Some (Palette_index index) -> sprintf "palette:%d" index
         | Some (Rgb { r; g; b }) -> sprintf "rgb:%d,%d,%d" r g b))
;;

let%expect_test "rendered view exposes an owned cell frame" =
  let view =
    View.hcat
      [ View.text ~attrs:[ Attr.bold; Attr.fg (Attr.Color.rgb ~r:10 ~g:20 ~b:30) ] "A"
      ; View.transparent_rectangle ~width:1 ~height:1
      ; View.text ~attrs:[ Attr.underline ] "B"
      ]
  in
  let rendered = View.Private.rendered_view view in
  show_frame (Rendered_view.frame rendered);
  [%expect
    {|
    3x1
    0,0 "A" bold=true underline=false fg=rgb:10,20,30
    2,0 "B" bold=false underline=true fg=default
    |}]
;;

let%expect_test "rendered view can round trip through ghostty plain text" =
  let rendered =
    View.vcat [ View.text "hello"; View.text ~attrs:[ Attr.bold ] "world" ]
    |> View.Private.rendered_view
  in
  let ansi = Rendered_view.render_ansi rendered in
  (match Bonsai_pi_ghostty_vt.render_plain ~cols:5 ~rows:2 ansi with
   | Ok text -> printf "%S\n" text
   | Error err -> printf "ghostty-error %s\n" (Bonsai_pi_ghostty_vt.error_to_string err));
  [%expect {| "hello\nworld" |}]
;;

let%expect_test "ansi output is generated from owned frame" =
  let rendered = Rendered_view.text ~attrs:[] "old" in
  let frame = Rendered_view.frame rendered in
  let style =
    match frame.cells.(0) with
    | Empty -> raise_s [%message "missing first cell"]
    | Text { style; text = _ } -> style
  in
  frame.cells.(0) <- Text { text = "n"; style };
  let ansi = Rendered_view.render_ansi ~cols:3 ~rows:1 rendered in
  (match Bonsai_pi_ghostty_vt.render_plain ~cols:3 ~rows:1 ansi with
   | Ok text -> printf "%S\n" text
   | Error err -> printf "ghostty-error %s\n" (Bonsai_pi_ghostty_vt.error_to_string err));
  [%expect {| "nld" |}]
;;

let%expect_test "rendered view frame follows pad crop and zcat" =
  let base = View.text "abc" |> View.pad ~l:1 ~t:1 ~r:1 ~b:1 |> View.crop ~l:1 ~t:1 ~r:1 ~b:1 in
  let overlay =
    View.zcat
      [ View.text ~attrs:[ Attr.bold ] "X"
      ; View.transparent_rectangle ~width:3 ~height:1
      ; base
      ]
  in
  show_frame (View.Private.rendered_view overlay |> Rendered_view.frame);
  [%expect
    {|
    3x1
    0,0 "X" bold=true underline=false fg=default
    1,0 "b" bold=false underline=false fg=default
    2,0 "c" bold=false underline=false fg=default
    |}]
;;

let%expect_test "rendered view with backdrop colors exposes styled spaces" =
  let view =
    View.text "A"
    |> View.with_colors
         ~fill_backdrop:true
         ~fg:(Attr.Color.rgb ~r:200 ~g:201 ~b:202)
         ~bg:(Attr.Color.rgb ~r:1 ~g:2 ~b:3)
  in
  show_frame (View.Private.rendered_view view |> Rendered_view.frame);
  [%expect
    {|
    1x1
    0,0 "A" bold=false underline=false fg=rgb:200,201,202
    |}]
;;
