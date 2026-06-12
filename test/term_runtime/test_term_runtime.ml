open! Core
open Async

module Event_conversion = Bonsai_term.Private.Event_conversion
module Event_queue = Bonsai_term.Private.Event_queue
module For_mocking = Bonsai_term.For_mocking
module Mouse_reporting_config = Bonsai_term.Private.Mouse_reporting_config
module Term_runtime = Bonsai_term.Private.Term_runtime
module View = Bonsai_term.View

let print_decoded string =
  let%bind events = Term_runtime.For_testing.decode_string string in
  print_s [%sexp (events : Event_conversion.terminal_event list)];
  return ()
;;

let%expect_test "ascii ctrl alt and utf8 keys" =
  let%bind () = print_decoded "a\001\027b\195\169" in
  [%expect
    {|
    ((Key (key (ASCII a)) (mods ())) (Key (key (ASCII A)) (mods (Ctrl)))
     (Key (key (ASCII b)) (mods (Meta))) (Key (key (Uchar U+00E9)) (mods ())))
    |}];
  return ()
;;

let%expect_test "standalone escape does not wait for another byte" =
  let%bind () = print_decoded "\027" in
  [%expect {| ((Key (key Escape) (mods ()))) |}];
  return ()
;;

let%expect_test "csi arrows and navigation" =
  let%bind () = print_decoded "\027[A\027[1;5B\027[3~\027[5~\027[6~\027OP" in
  [%expect
    {|
    ((Key (key (Arrow Up)) (mods ())) (Key (key (Arrow Down)) (mods (Ctrl)))
     (Key (key Delete) (mods ())) (Key (key (Page Up)) (mods ()))
     (Key (key (Page Down)) (mods ())) (Key (key (Function 1)) (mods ())))
    |}];
  return ()
;;

let%expect_test "bracketed paste boundaries" =
  let%bind () = print_decoded "\027[200~pasted\027[201~" in
  [%expect
    {|
    ((Paste Start) (Key (key (ASCII p)) (mods ()))
     (Key (key (ASCII a)) (mods ())) (Key (key (ASCII s)) (mods ()))
     (Key (key (ASCII t)) (mods ())) (Key (key (ASCII e)) (mods ()))
     (Key (key (ASCII d)) (mods ())) (Paste End)) |}];
  return ()
;;

let%expect_test "sgr mouse press release drag and scroll" =
  let%bind () =
    print_decoded "\027[<0;10;5M\027[<0;10;5m\027[<32;11;6M\027[<64;12;7M"
  in
  [%expect
    {|
    ((Mouse (kind Left) (position ((x 9) (y 4))) (mods ()))
     (Mouse (kind Release) (position ((x 9) (y 4))) (mods ()))
     (Mouse (kind Drag) (position ((x 10) (y 5))) (mods ()))
     (Mouse (kind (Scroll Up)) (position ((x 11) (y 6))) (mods ()))) |}];
  return ()
;;

let with_temp_runtime ?(input = "") ?for_mocking f =
  let path = Filename_unix.temp_file "bonsai-term-runtime" ".out" in
  let%bind reader = Reader.For_testing.of_string input in
  let%bind writer = Writer.open_file path in
  let event_queue = Event_queue.create () in
  let%bind runtime =
    Term_runtime.create
      ~event_queue
      { dispose = None
      ; nosig = None
      ; mouse = None
      ; bpaste = None
      ; reader = Some reader
      ; writer = Some writer
      ; for_mocking
      }
  in
  let%bind result = f runtime event_queue in
  let%bind () = Term_runtime.release runtime in
  let%bind () = Writer.close writer in
  let output = In_channel.read_all path in
  Core_unix.unlink path;
  return (result, output)
;;

let visible_escapes string =
  string
  |> String.substr_replace_all ~pattern:"\027" ~with_:"<ESC>"
  |> String.substr_replace_all ~pattern:"\007" ~with_:"<BEL>"
;;

let%expect_test "runtime writes setup render cursor title and release sequences" =
  let for_mocking =
    For_mocking.create
      ~dimensions:(fun _ -> Some (2, 1))
      ~wait_for_next_window_change:(fun () -> Deferred.never ())
      ~is_a_tty:(fun _ -> return false)
  in
  let%bind (), output =
    with_temp_runtime ~for_mocking (fun runtime _event_queue ->
      let%bind () = Term_runtime.render runtime (View.text "ok" |> View.Private.rendered_view) in
      let%bind () =
        Term_runtime.set_cursor
          runtime
          (Some
             { position = { x = 2; y = 3 }
             ; kind = Bonsai_term.Cursor.Kind.Bar
             })
      in
      let%bind () = Term_runtime.set_title runtime "hello\027bad\007" in
      Term_runtime.set_mouse_enabled runtime Mouse_reporting_config.No_mouse_events)
  in
  print_endline (visible_escapes output);
  [%expect
    {| <ESC>[?1000h<ESC>[?1002h<ESC>[?1006h<ESC>[?2004h<ESC>[?25l<ESC>[H<ESC>[0mo<ESC>[0mk<ESC>[0m<ESC>[?25h<ESC>[4;3H<ESC>[6 q<ESC>]0;hellobad<BEL><ESC>[?1003l<ESC>[?1002l<ESC>[?1000l<ESC>[?1006l<ESC>[?1003l<ESC>[?1002l<ESC>[?1000l<ESC>[?1006l<ESC>[?2004l<ESC>[?25h<ESC>[0m |}];
  return ()
;;

let%expect_test "runtime keeps ghostty adapter in sync with rendered frames" =
  let for_mocking =
    For_mocking.create
      ~dimensions:(fun _ -> Some (8, 2))
      ~wait_for_next_window_change:(fun () -> Deferred.never ())
      ~is_a_tty:(fun _ -> return false)
  in
  let%bind (), _output =
    with_temp_runtime ~for_mocking (fun runtime _event_queue ->
      let%bind () =
        Term_runtime.render runtime (View.text "ghostty" |> View.Private.rendered_view)
      in
      (match Term_runtime.For_testing.ghostty_snapshot runtime with
       | Ok snapshot -> printf "%S\n" snapshot
       | Error err -> printf "ghostty unavailable: %s\n" (Ghostty_vt.error_to_string err));
      return ())
  in
  [%expect {| "ghostty\n" |}];
  return ()
;;

let%expect_test "runtime exposes ghostty selection strings" =
  let for_mocking =
    For_mocking.create
      ~dimensions:(fun _ -> Some (8, 2))
      ~wait_for_next_window_change:(fun () -> Deferred.never ())
      ~is_a_tty:(fun _ -> return false)
  in
  let%bind (), _output =
    with_temp_runtime ~for_mocking (fun runtime _event_queue ->
      let%bind () =
        Term_runtime.render runtime (View.text "ghostty" |> View.Private.rendered_view)
      in
      (match
         Term_runtime.For_testing.ghostty_selection_to_string
           runtime
           ~start:{ tag = Ghostty_vt.Grid.Active; x = 0; y = 0 }
           ~end_:{ tag = Ghostty_vt.Grid.Active; x = 4; y = 0 }
       with
       | Ok text -> printf "%S\n" text
       | Error err -> printf "ghostty unavailable: %s\n" (Ghostty_vt.error_to_string err));
      return ())
  in
  [%expect {| "ghost" |}];
  return ()
;;

let%expect_test "mock dimensions and resize events" =
  let dimensions = ref (10, 4) in
  let resize_reader, resize_writer = Pipe.create () in
  let for_mocking =
    For_mocking.create
      ~dimensions:(fun _ -> Some !dimensions)
      ~wait_for_next_window_change:(fun () ->
        match%map Pipe.read resize_reader with
        | `Ok () -> ()
        | `Eof -> ())
      ~is_a_tty:(fun _ -> return false)
  in
  let%bind (), _output =
    with_temp_runtime ~for_mocking (fun runtime event_queue ->
      print_s [%sexp (Term_runtime.size runtime : Bonsai_term.Dimensions.t)];
      dimensions := 12, 5;
      Pipe.write_without_pushback resize_writer ();
      let%bind () = Scheduler.yield_until_no_jobs_remain () in
      print_s
        [%sexp
          (Event_queue.dequeue_all_and_clear event_queue
           : Nothing.t Bonsai_term.Event.Root_event.t list)];
      return ())
  in
  Pipe.close resize_writer;
  [%expect
    {|
    ((height 4) (width 10))
    ((Resize ((height 5) (width 12))))
    |}];
  return ()
;;
