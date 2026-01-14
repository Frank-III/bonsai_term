open! Core
open Bonsai
open Async

type 'exit t =
  { clock : Bonsai.Time_source.t
  ; target_delay : Time_ns.Span.t
  ; term : Term.t
  ; dimensions_manager : State_management.For_dimensions.t
  ; exit_manager : 'exit State_management.For_exit.t
  ; driver : View.With_handler.t Bonsai_driver.t
  ; time_source : Async.Time_source.t
  ; mutable prev_view : View.t option
  }

let create
  { Start_params.dispose
  ; nosig
  ; mouse
  ; bpaste
  ; reader
  ; writer
  ; time_source
  ; for_mocking
  ; optimize
  ; target_frames_per_second
  ; app
  }
  =
  let clock = State_management.For_clock.create time_source
  and target_delay = Time_ns.Span.of_sec (1.0 /. Float.of_int target_frames_per_second) in
  let%bind term =
    Term.create
      ?mouse
      ?dispose
      ?nosig
      ?bpaste
      ?reader
      ?writer
      ~time_source
      ?for_mocking
      ()
  in
  let dimensions_manager = State_management.For_dimensions.create ~term
  and exit_manager = State_management.For_exit.create () in
  let driver =
    (fun (local_ graph) ->
      let ~view, ~handler =
        app
          ~exit:(fun exit -> State_management.For_exit.exit_effect exit_manager exit)
          ~dimensions:(State_management.For_dimensions.value dimensions_manager)
          graph
      in
      let%arr.Bonsai view and handler in
      ~view, ~handler)
    |> Cursor.register term
    |> Title.register term
    |> Bonsai_driver.create
         ~optimize
         ~time_source:clock
         ~instrumentation:(Bonsai_driver.Instrumentation.default_for_test_handles ())
  in
  let driver =
    { clock
    ; target_delay
    ; term
    ; dimensions_manager
    ; exit_manager
    ; driver
    ; time_source
    ; prev_view = None
    }
  in
  return driver
;;

let compute_first_frame t =
  Bonsai_driver.flush t.driver;
  Bonsai_driver.trigger_lifecycles t.driver
;;

let handle_root_event
  :  handle_event:(Event.t -> unit)
  -> dimensions_manager:State_management.For_dimensions.t
  -> exit_manager:'exit State_management.For_exit.t -> Event.Root_event.t
  -> 'exit Frame_outcome.t
  =
  fun ~handle_event ~dimensions_manager ~exit_manager event ->
  match event with
  | Stdin_closed -> Stdin_closed
  | Resize dimensions ->
    State_management.For_dimensions.set dimensions_manager dimensions;
    Continue
  | Event ((Paste _ | Mouse _ | Key_press _) as event) ->
    handle_event event;
    (match State_management.For_exit.exit_status exit_manager with
     | Not_yet_exited -> Continue
     | Exited exit -> Exit exit)
  | Timer -> Continue
;;

let[@inline always] if_frame_has_not_yet_exited exit_manager f =
  match State_management.For_exit.exit_status exit_manager with
  | Exited exit ->
    Deferred.return
      (`Frame_painted (Deferred.return (`Frame_finished (Frame_outcome.Exit exit))))
  | Not_yet_exited -> f ()
;;

let compute_frame
  ({ clock
   ; target_delay
   ; term
   ; dimensions_manager
   ; exit_manager
   ; driver
   ; time_source
   ; prev_view
   } as t)
  =
  if_frame_has_not_yet_exited exit_manager
  @@ fun () ->
  let frame_start_time = Time_source.now time_source in
  let () = State_management.For_clock.advance_to clock frame_start_time
  and () = State_management.For_dimensions.update dimensions_manager in
  Bonsai_driver.flush driver;
  let ~view, ~handler = Bonsai_driver.result driver in
  let view_changed =
    match prev_view with
    | None -> true
    | Some prev_view -> not (phys_equal view prev_view)
  in
  let%bind () =
    if view_changed
    then
      if Term.dead term
      then Deferred.return ()
      else (
        let%map () = Term.image term (View.Private.notty_image view) in
        t.prev_view <- Some view)
    else Deferred.return ()
  in
  Bonsai_driver.trigger_lifecycles driver;
  let time_taken = Time_ns.diff (Time_source.now time_source) frame_start_time in
  let delay = Time_ns.Span.(max zero (target_delay - time_taken)) in
  let handle_event event =
    Effect.Expert.handle (handler event) ~on_exn:(fun exn ->
      Exn.reraise exn "Unhandled exception raised in effect")
  in
  let shutdown_or_continue =
    let%map events = Term.next_event_or_wait_delay ~delay term in
    Nonempty_list.fold_until
      events
      ~init:()
      ~finish:(fun () -> Continue)
      ~f:(fun () event ->
        match handle_root_event ~handle_event ~dimensions_manager ~exit_manager event with
        | Continue -> Continue ()
        | Stdin_closed -> Stop Frame_outcome.Stdin_closed
        | Exit exit -> Stop (Frame_outcome.Exit exit))
  in
  let frame_finished =
    let%map shutdown_or_continue in
    `Frame_finished shutdown_or_continue
  in
  return (`Frame_painted frame_finished)
;;

let release t = Term.release t.term
let prev_view t = t.prev_view
let dimensions t = Term.dimensions t.term
