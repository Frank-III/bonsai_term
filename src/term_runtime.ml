open! Core
open Async

module Start_params = Term_start_params
module Ghostty = Ghostty_vt

type t =
  { reader : Reader.t
  ; writer : Writer.t
  ; enqueue_terminal_event : Event_conversion.terminal_event -> unit
  ; enqueue_resize : Geom.Dimensions.t -> unit
  ; enqueue_closed : unit -> unit
  ; mutable dimensions : Geom.Dimensions.t
  ; mutable ghostty : Ghostty.Adapter.t option
  ; mutable released : bool
  ; original_termios : Core_unix.Terminal_io.t option
  }

let default_dimensions = { Geom.Dimensions.width = 80; height = 24 }

let dimensions_from_env () =
  match Sys.getenv "COLUMNS", Sys.getenv "LINES" with
  | Some columns, Some lines ->
    Option.try_with (fun () ->
      { Geom.Dimensions.width = Int.of_string columns; height = Int.of_string lines })
    |> Option.filter ~f:(fun { width; height } -> width > 0 && height > 0)
  | _, _ -> None
;;

let dimensions_from_stty () =
  Option.try_with (fun () ->
    let in_channel = Core_unix.open_process_in "stty size 2>/dev/null </dev/tty" in
    let output = In_channel.input_all in_channel in
    match Core_unix.close_process_in in_channel with
    | Ok () ->
      (match String.split (String.strip output) ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) with
       | [ rows; columns ] ->
         let height = Int.of_string rows in
         let width = Int.of_string columns in
         Option.some_if (width > 0 && height > 0) { Geom.Dimensions.width; height }
       | _ -> None)
    | Error _ -> None)
  |> Option.join
;;

let fd_file_descr fd = Fd.file_descr_exn fd

let dimensions_from_mocking for_mocking writer =
  let file_descr = Writer.fd writer |> fd_file_descr in
  Option.bind for_mocking ~f:(fun for_mocking -> For_mocking.dimensions for_mocking file_descr)
  |> Option.map ~f:(fun (width, height) -> { Geom.Dimensions.width; height })
;;

let current_dimensions ?for_mocking writer =
  match dimensions_from_mocking for_mocking writer with
  | Some dimensions -> dimensions
  | None ->
    (match dimensions_from_stty () with
     | Some dimensions -> dimensions
     | None ->
       (match dimensions_from_env () with
        | Some dimensions -> dimensions
        | None -> default_dimensions))
;;

let write_and_flush writer string =
  Writer.write writer string;
  Writer.flushed writer
;;

let create_ghostty dimensions =
  match
    Ghostty.Adapter.create ~cols:dimensions.Geom.Dimensions.width ~rows:dimensions.height ()
  with
  | Ok ghostty -> Some ghostty
  | Error _ -> None
;;

let resize_ghostty t =
  match t.ghostty with
  | None -> ()
  | Some ghostty ->
    (match
       Ghostty.Adapter.resize
         ghostty
         ~cols:t.dimensions.Geom.Dimensions.width
         ~rows:t.dimensions.height
     with
     | Ok () -> ()
     | Error _ ->
       Ghostty.Adapter.free ghostty;
       t.ghostty <- create_ghostty t.dimensions)
;;

let feed_ghostty_frame t frame =
  match t.ghostty with
  | None -> ()
  | Some ghostty ->
    Ghostty.Adapter.write ghostty frame
;;

let set_raw_mode ?(nosig = false) reader ~for_mocking =
  let fd = Reader.fd reader in
  let%bind is_tty =
    match for_mocking with
    | Some for_mocking -> For_mocking.is_a_tty for_mocking fd
    | None -> return (Core_unix.isatty (fd_file_descr fd))
  in
  match is_tty with
  | false -> return None
  | true ->
    let original = Core_unix.Terminal_io.tcgetattr (fd_file_descr fd) in
    let raw =
      { original with
        c_icanon = false
      ; c_echo = false
      ; c_echoe = false
      ; c_echok = false
      ; c_echonl = false
      ; c_icrnl = false
      ; c_ixon = false
      ; c_isig = not nosig
      ; c_vmin = 1
      ; c_vtime = 0
      }
    in
    Core_unix.Terminal_io.tcsetattr raw (fd_file_descr fd) ~mode:TCSANOW;
    let%map () = Deferred.unit in
    Some original
;;

let restore_termios reader = function
  | None -> Deferred.unit
  | Some termios ->
    Core_unix.Terminal_io.tcsetattr termios (Reader.fd reader |> fd_file_descr) ~mode:TCSANOW;
    Deferred.unit
;;

let mouse_enable_sequences = function
  | Mouse_reporting_config.No_mouse_events -> ""
  | All_mouse_events_except_hover -> "\027[?1000h\027[?1002h\027[?1006h"
  | All_mouse_events -> "\027[?1000h\027[?1002h\027[?1003h\027[?1006h"
;;

let mouse_disable_sequences = "\027[?1003l\027[?1002l\027[?1000l\027[?1006l"

let setup_sequences ~mouse ~bpaste =
  mouse_enable_sequences mouse ^ if bpaste then "\027[?2004h\027[?25l" else "\027[?25l"
;;

let release_sequences = mouse_disable_sequences ^ "\027[?2004l\027[?25h\027[0m"

let key ?(mods = []) key = Event_conversion.Key { key; mods }
let event_of_ascii = function
  | '\027' -> Some (key Escape)
  | '\r' | '\n' -> Some (key Enter)
  | '\t' -> Some (key Tab)
  | '\b' | '\127' -> Some (key Backspace)
  | c when Char.to_int c >= 1 && Char.to_int c <= 26 ->
    let ascii = Char.of_int_exn (Char.to_int c + Char.to_int 'A' - 1) in
    Some (key ~mods:[ Ctrl ] (ASCII ascii))
  | c when Char.to_int c >= 32 && Char.to_int c < 127 -> Some (key (ASCII c))
  | _ -> None
;;

let read_char_option ?timeout reader =
  let normalize = function
    | `Eof -> None
    | `Ok c -> Some c
  in
  match timeout with
  | None -> Reader.read_char reader >>| normalize
  | Some timeout ->
    choose
      [ choice (Reader.read_char reader) normalize
      ; choice (Clock.after timeout) (fun () -> None)
      ]
;;

let utf8_expected_length first =
  let byte = Char.to_int first in
  if byte land 0b1000_0000 = 0
  then 1
  else if byte land 0b1110_0000 = 0b1100_0000
  then 2
  else if byte land 0b1111_0000 = 0b1110_0000
  then 3
  else if byte land 0b1111_1000 = 0b1111_0000
  then 4
  else 1
;;

let read_utf8_event reader first =
  let length = utf8_expected_length first in
  let buffer = Buffer.create length in
  Buffer.add_char buffer first;
  let rec loop remaining =
    if remaining = 0
    then (
      let string = Buffer.contents buffer in
      match String.Utf8.is_valid string with
      | false -> return None
      | true ->
        String.Utf8.to_sequence (String.Utf8.of_string_unchecked string)
        |> Sequence.hd
        |> Option.map ~f:(fun uchar -> key (Uchar uchar))
        |> return)
    else (
      let%bind next = read_char_option reader in
      match next with
      | None -> return None
      | Some c ->
        Buffer.add_char buffer c;
        loop (remaining - 1))
  in
  loop (length - 1)
;;

let mods_of_csi_param params =
  match List.last params with
  | None | Some 0 | Some 1 -> []
  | Some n ->
    let mask = n - 1 in
    [ Option.some_if (mask land 1 <> 0) Event.Modifier.Shift
    ; Option.some_if (mask land 2 <> 0) Event.Modifier.Meta
    ; Option.some_if (mask land 4 <> 0) Event.Modifier.Ctrl
    ]
    |> List.filter_opt
;;

let key_of_csi final params =
  let mods = mods_of_csi_param params in
  match final, params with
  | 'A', _ -> Some (key ~mods (Arrow `Up))
  | 'B', _ -> Some (key ~mods (Arrow `Down))
  | 'C', _ -> Some (key ~mods (Arrow `Right))
  | 'D', _ -> Some (key ~mods (Arrow `Left))
  | 'H', _ -> Some (key ~mods Home)
  | 'F', _ -> Some (key ~mods End)
  | '~', [ 1 ] | '~', [ 7 ] -> Some (key Home)
  | '~', [ 4 ] | '~', [ 8 ] -> Some (key End)
  | '~', [ 2 ] -> Some (key Insert)
  | '~', [ 3 ] -> Some (key Delete)
  | '~', [ 5 ] -> Some (key (Page `Up))
  | '~', [ 6 ] -> Some (key (Page `Down))
  | '~', [ n ] when n >= 11 && n <= 15 -> Some (key (Function (n - 10)))
  | '~', [ n ] when n >= 17 && n <= 21 -> Some (key (Function (n - 11)))
  | '~', [ n ] when n >= 23 && n <= 24 -> Some (key (Function (n - 12)))
  | _ -> None
;;

let mouse_of_sgr params final =
  match params with
  | button_code :: x :: y :: _ ->
    let mods =
      [ Option.some_if (button_code land 4 <> 0) Event.Modifier.Shift
      ; Option.some_if (button_code land 8 <> 0) Event.Modifier.Meta
      ; Option.some_if (button_code land 16 <> 0) Event.Modifier.Ctrl
      ]
      |> List.filter_opt
    in
    let button = button_code land 3 in
    let kind =
      if Char.equal final 'm'
      then Event.Release
      else if button_code land 64 <> 0
      then Scroll (if button = 0 then `Up else `Down)
      else if button_code land 32 <> 0
      then Drag
      else (
        match button with
        | 0 -> Left
        | 1 -> Middle
        | 2 -> Right
        | _ -> Release)
    in
    Some
      (Event_conversion.Mouse
         { kind
         ; position = { Geom.Position.x = Int.max 0 (x - 1); y = Int.max 0 (y - 1) }
         ; mods
         })
  | _ -> None
;;

let parse_int_default_zero s =
  if String.is_empty s then 0 else Option.value (Int.of_string_opt s) ~default:0
;;

let parse_csi_payload payload =
  String.split payload ~on:';' |> List.map ~f:parse_int_default_zero
;;

let read_csi_event reader =
  let buffer = Buffer.create 16 in
  let rec loop () =
    let%bind next = read_char_option reader in
    match next with
    | None -> return None
    | Some final when Char.(final >= '@' && final <= '~') ->
      let payload = Buffer.contents buffer in
      if (not (String.is_empty payload)) && Char.equal (String.get payload 0) '<'
      then (
        let params = String.drop_prefix payload 1 |> parse_csi_payload in
        return (mouse_of_sgr params final))
      else (
        let params = parse_csi_payload payload in
        (match final, params with
         | '~', [ 200 ] -> return (Some (Event_conversion.Paste `Start))
         | '~', [ 201 ] -> return (Some (Event_conversion.Paste `End))
         | _ -> return (key_of_csi final params)))
    | Some c ->
      Buffer.add_char buffer c;
      loop ()
  in
  loop ()
;;

let escape_timeout = Time_float.Span.of_ms 25.

let read_escape_event reader =
  let%bind next = read_char_option ~timeout:escape_timeout reader in
  match next with
  | None -> return (Some (key Escape))
  | Some '[' -> read_csi_event reader
  | Some 'O' ->
    let%map final = read_char_option reader in
    (match final with
     | Some 'P' -> Some (key (Function 1))
     | Some 'Q' -> Some (key (Function 2))
     | Some 'R' -> Some (key (Function 3))
     | Some 'S' -> Some (key (Function 4))
     | _ -> Some (key Escape))
  | Some c ->
    (match event_of_ascii c with
     | None -> return (Some (key Escape))
     | Some (Event_conversion.Key { key; mods }) ->
       return (Some (Event_conversion.Key { key; mods = Event.Modifier.Meta :: mods }))
     | Some event -> return (Some event))
;;

let read_terminal_event reader first =
  match first with
  | '\027' -> read_escape_event reader
  | c when Char.to_int c < 128 -> return (event_of_ascii c)
  | c -> read_utf8_event reader c
;;

let start_input_loop t =
  don't_wait_for
    (let rec loop () =
       if t.released
       then Deferred.unit
       else (
         let%bind result = Reader.read_char t.reader in
         match result with
         | `Eof ->
           t.enqueue_closed ();
           Deferred.unit
         | `Ok first ->
           let%bind event = read_terminal_event t.reader first in
           Option.iter event ~f:t.enqueue_terminal_event;
           loop ())
     in
     loop ())
;;

let start_mock_resize_loop t for_mocking =
  don't_wait_for
    (let rec loop () =
       if t.released
       then Deferred.unit
       else (
         let%bind () = For_mocking.wait_for_next_window_change for_mocking () in
         if t.released
         then Deferred.unit
         else (
           let dimensions = current_dimensions ?for_mocking:(Some for_mocking) t.writer in
           t.dimensions <- dimensions;
           resize_ghostty t;
           t.enqueue_resize dimensions;
           loop ()))
     in
     loop ())
;;

let create ~event_queue (start_params : Start_params.t) =
  let%tydi { dispose = _; nosig; mouse; bpaste; reader; writer; for_mocking } =
    start_params
  in
  let reader = Option.value reader ~default:(Lazy.force Reader.stdin) in
  let writer = Option.value writer ~default:(Lazy.force Writer.stdout) in
  let mouse = Option.value mouse ~default:Mouse_reporting_config.All_mouse_events_except_hover in
  let bpaste = Option.value bpaste ~default:true in
  let nosig = Option.value nosig ~default:false in
  let%bind original_termios = set_raw_mode ~nosig reader ~for_mocking in
  let dimensions = current_dimensions ?for_mocking writer in
  let t =
    { reader
    ; writer
    ; enqueue_terminal_event =
        (fun event ->
          Event_queue.enqueue_event
            event_queue
            (Event_conversion.root_event_to_root_event (Terminal_event event)))
    ; enqueue_resize =
        (fun dimensions -> Event_queue.enqueue_event event_queue (Resize dimensions))
    ; enqueue_closed =
        (fun () -> Event_queue.enqueue_event event_queue Incoming_events_pipe_closed)
    ; dimensions
    ; ghostty = create_ghostty dimensions
    ; released = false
    ; original_termios
    }
  in
  let%bind () = write_and_flush writer (setup_sequences ~mouse ~bpaste) in
  start_input_loop t;
  Option.iter for_mocking ~f:(start_mock_resize_loop t);
  return t
;;

let size t = t.dimensions

let render t view =
  let frame =
    "\027[H" ^ Rendered_view.render_ansi ~cols:t.dimensions.width ~rows:t.dimensions.height view
  in
  feed_ghostty_frame t frame;
  Writer.write t.writer frame;
  Writer.flushed t.writer
;;

let has_been_released t = t.released

let release t =
  if t.released
  then Deferred.unit
  else (
    t.released <- true;
    Option.iter t.ghostty ~f:Ghostty.Adapter.free;
    t.ghostty <- None;
    let%bind () = write_and_flush t.writer release_sequences in
    restore_termios t.reader t.original_termios)
;;

let cursor_shape_code = function
  | Types.Cursor.Kind.Default -> 0
  | Bar -> 6
  | Bar_blinking -> 5
  | Block -> 2
  | Block_blinking -> 1
  | Underline -> 4
  | Underline_blinking -> 3
;;

let set_cursor t (cursor : Types.Cursor.t option) =
  (match cursor with
   | None -> Writer.write t.writer "\027[?25l"
   | Some { position = { x; y }; kind } ->
     Writer.write t.writer "\027[?25h";
     Writer.writef t.writer "\027[%d;%dH" (y + 1) (x + 1);
     Writer.writef t.writer "\027[%d q" (cursor_shape_code kind));
  Writer.flushed t.writer
;;

let sanitize_title title =
  String.filter title ~f:(fun c -> not (Char.equal c '\027' || Char.equal c '\007'))
;;

let set_title t title =
  Writer.writef t.writer "\027]0;%s\007" (sanitize_title title);
  Writer.flushed t.writer
;;

let set_mouse_enabled t enabled =
  Writer.write t.writer mouse_disable_sequences;
  Writer.write t.writer (mouse_enable_sequences enabled);
  Writer.flushed t.writer
;;

let write_to_string_tty t string = write_and_flush t.writer string

module For_testing = struct
  let ghostty_snapshot t =
    match t.ghostty with
    | None -> Error (Ghostty.Ghostty_error ("ghostty_adapter_unavailable", -4))
    | Some ghostty -> Ghostty.Adapter.render ghostty
  ;;

  let ghostty_selection_to_string t ~start ~end_ =
    match t.ghostty with
    | None -> Error (Ghostty.Ghostty_error ("ghostty_adapter_unavailable", -4))
    | Some ghostty -> Ghostty.Adapter.selection_to_string ghostty ~start ~end_
  ;;

  let decode_string string =
    let%bind reader = Reader.For_testing.of_string string in
    let rec loop acc =
      let%bind next = read_char_option reader in
      match next with
      | None -> return (List.rev acc)
      | Some first ->
        let%bind event = read_terminal_event reader first in
        loop
          (match event with
           | None -> acc
           | Some event -> event :: acc)
    in
    loop []
  ;;
end
