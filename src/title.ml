open! Core

let sanitize_title title =
  (* Terminal title setting happens via an OSC escape sequence. Avoid passing through
     control characters that could break the sequence or cause surprising behavior. *)
  String.map title ~f:(fun c ->
    match Char.to_int c with
    | n when n < 0x20 || n = 0x7f -> ' '
    | _ -> c)
  |> String.strip
;;

let variable : (string -> unit Ui_effect.t) Bonsai.Dynamic_scope.t =
  Bonsai.Dynamic_scope.create
    ~name:"set_title"
    ~fallback:(fun title ->
      let _ : _ =
        raise_s
          [%message
            "Bug in bonsai_term! Set title handler not registered! [set_title] won't \
             occur"
              (title : string)]
      in
      Effect.Ignore)
    ()
;;

let register term inside =
  let value =
    Bonsai.return
      (Ui_effect_of_deferred.of_deferred_fun (fun title ->
         Term.set_title term (sanitize_title title)))
  in
  Bonsai.Dynamic_scope.set variable value ~inside
;;

let set_title = Bonsai.Dynamic_scope.lookup variable

module For_mock_tests = struct
  let register
    ?(set_title =
      fun title ->
        Ui_effect.of_thunk (fun () -> print_s [%message "[set_title]" (title : string)]))
    inside
    =
    let value = Bonsai.return set_title in
    Bonsai.Dynamic_scope.set variable value ~inside
  ;;
end
