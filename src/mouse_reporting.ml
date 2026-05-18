open! Core

let variable : (Mouse_reporting_config.t -> unit Ui_effect.t) Bonsai.Dynamic_scope.t =
  Bonsai.Dynamic_scope.create
    ~name:"set_mouse_reporting"
    ~fallback:(fun enabled ->
      let _ : _ =
        raise_s
          [%message
            "Bug in bonsai_term! Mouse reporting handler not registered! \
             [set_mouse_reporting] won't occur"
              (enabled : Mouse_reporting_config.t)]
      in
      Ui_effect.Ignore)
    ()
;;

let set_mouse_reporting = Bonsai.Dynamic_scope.lookup variable

let register term inside =
  let value =
    Bonsai.return
      (Ui_effect_of_deferred.of_deferred_fun (fun enabled -> Term.set_mouse term enabled))
  in
  Bonsai.Dynamic_scope.set variable value ~inside
;;

module For_mock_tests = struct
  let register inside =
    let value =
      Bonsai.return (fun enabled ->
        Ui_effect.of_sync_fun
          (fun () ->
            print_s
              [%message "[set_mouse_reporting]" (enabled : Mouse_reporting_config.t)])
          ())
    in
    Bonsai.Dynamic_scope.set variable value ~inside
  ;;
end
