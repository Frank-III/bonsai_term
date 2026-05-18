open! Core
open Bonsai
open Async

let rec run_driver_until_exit driver =
  let%bind (`Frame_painted finish_frame) = Driver.compute_frame driver in
  match%bind finish_frame with
  | `Frame_finished (Exit exit) -> Deferred.Or_error.return exit
  | `Frame_finished Incoming_events_pipe_closed ->
    Deferred.Or_error.error_s
      [%message
        "Bonsai Term app was closed early. The stdin to the app was closed. If this is \
         in prod, and you expect your TUI's stdin's to be closed in practice, please \
         reach out to bonsai-term devs about your use case. If you are writing a test, \
         consider using the bonsai_term_test or bonsai_integration_test libraries \
         instead or alternatively consider mocking out the ?reader, ?writer and \
         ?for_mocking paramters to [Bonsai_term.start]."]
  | `Frame_finished Continue -> run_driver_until_exit driver [@tail]
;;

let start_driver_with_exit driver =
  Driver.compute_first_frame driver;
  run_driver_until_exit driver
;;

let stitch (~view, ~handler) =
  let%arr.Bonsai view and handler in
  ~view, ~handler
;;

let with_driver
  ~dispose
  ~nosig
  ~mouse
  ~bpaste
  ~reader
  ~writer
  ~time_source
  ~for_mocking
  ~optimize
  ~target_frames_per_second
  ~get_view_and_handler
  ~handle_incoming
  app
  f
  =
  Deferred.Or_error.try_with_join (fun () ->
    let start_params =
      Start_params.create_exn
        ~runtime_start_params:
          { Term_start_params.dispose; nosig; mouse; bpaste; reader; writer; for_mocking }
        ~time_source
        ~optimize
        ~target_frames_per_second
        ~get_view_and_handler
        ~handle_incoming
        ~app
    in
    let%bind driver = Driver.create start_params (module Term_runtime) in
    let finally () = Driver.release driver in
    Monitor.protect (fun () -> f driver) ~finally)
;;

let start_with_custom_runtime
  ~runtime_start_params
  runtime
  ?time_source
  ?optimize
  ?target_frames_per_second
  ~get_view_and_handler
  ~handle_incoming
  app
  =
  Deferred.Or_error.try_with_join (fun () ->
    let start_params =
      Start_params.create_exn
        ~runtime_start_params
        ~time_source
        ~optimize
        ~target_frames_per_second
        ~get_view_and_handler
        ~handle_incoming
        ~app
    in
    let%bind.Deferred driver = Driver.create start_params runtime in
    Driver.compute_first_frame driver;
    let _finished : 'exit Deferred.Or_error.t =
      (* NOTE: We ignored the finished ['exit] here, when using `start_with_driver` the
         intent is to isntead use [Driver.finished] instead. *)
      Monitor.protect
        (fun () -> run_driver_until_exit driver)
        ~finally:(fun () -> Driver.release driver)
    in
    Deferred.Or_error.return driver)
;;

let start_with_driver
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?reader
  ?writer
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?for_mocking
  ~get_view_and_handler
  ~handle_incoming
  app
  =
  let runtime_start_params =
    { Term_start_params.dispose; nosig; mouse; bpaste; reader; writer; for_mocking }
  in
  start_with_custom_runtime
    ~runtime_start_params
    (module Term_runtime)
    ?time_source
    ?optimize
    ?target_frames_per_second
    ~get_view_and_handler
    ~handle_incoming
    app
;;

let start_with_exit
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?reader
  ?writer
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?for_mocking
  app
  =
  with_driver
    ~dispose
    ~nosig
    ~mouse
    ~bpaste
    ~reader
    ~writer
    ~time_source
    ~optimize
    ~target_frames_per_second
    ~for_mocking
    ~get_view_and_handler:Fn.id
    ~handle_incoming:(fun _ incoming -> Nothing.unreachable_code incoming)
    (fun ~exit ~dimensions graph -> stitch (app ~exit ~dimensions graph))
    start_driver_with_exit
;;

let make_app_exit_on_ctrlc app =
  let app ~exit ~dimensions (local_ graph) =
    let ~view, ~handler = app ~dimensions graph in
    let handler =
      let%arr.Bonsai handler in
      fun (event : Event.t) ->
        match event with
        | Key_press { key = ASCII ('C' | 'c'); mods = [ Ctrl ] } -> exit ()
        | Key_press { key = Uchar uchar; mods = [ Ctrl ] }
          when Uchar.equal (Uchar.of_char 'C') uchar
               || Uchar.equal (Uchar.of_char 'c') uchar -> exit ()
        | event -> handler event
    in
    ~view, ~handler
  in
  app
;;

let start
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?reader
  ?writer
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?for_mocking
  app
  =
  start_with_exit
    ?dispose
    ?nosig
    ?mouse
    ?bpaste
    ?reader
    ?writer
    ?time_source
    ?optimize
    ?target_frames_per_second
    ?for_mocking
    (make_app_exit_on_ctrlc app)
;;

module For_testing = struct
  let make_app_exit_on_ctrlc = make_app_exit_on_ctrlc
  let with_driver = with_driver
end

module For_other_bonsais = struct
  module Event_queue = Event_queue

  module Runtime = struct
    module type S = Runtime_intf.S
  end

  let start_with_custom_runtime = start_with_custom_runtime
end
