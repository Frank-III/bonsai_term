open! Core

let variable : (string -> unit Ui_effect.t) Bonsai.Dynamic_scope.t =
  Bonsai.Dynamic_scope.create
    ~name:"write_string_to_tty"
    ~fallback:(fun string ->
      let _ : _ =
        raise_s
          [%message
            "Bug in bonsai_term! write_string_to_tty handler not registered! \
             [write_string_to_tty] won't occur"
              (string : string)]
      in
      Effect.Ignore)
    ()
;;

let register term inside =
  let value =
    Bonsai.return
      (Ui_effect_of_deferred.of_deferred_fun (fun string ->
         Term.write_string_to_tty term string))
  in
  Bonsai.Dynamic_scope.set variable value ~inside
;;

let write_string_to_tty (local_ graph) = Bonsai.Dynamic_scope.lookup variable graph

module For_mock_tests = struct
  let register
    ?(write_string_to_tty =
      fun string ->
        Ui_effect.of_thunk (fun () ->
          print_s [%message "[write_string_to_tty]" (string : string)]))
    inside
    =
    let value = Bonsai.return write_string_to_tty in
    Bonsai.Dynamic_scope.set variable value ~inside
  ;;
end
