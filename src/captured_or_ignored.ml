open! Core

type t =
  | Captured of { here : Source_code_position.t Nonempty_list.t }
  | Ignored
[@@deriving sexp_of]

let any ts =
  let l =
    List.concat_map (Nonempty_list.to_list ts) ~f:(function
      | Captured { here } -> Nonempty_list.to_list here
      | Ignored -> [])
  in
  match l with
  | [] -> Ignored
  | here :: rem -> Captured { here = Nonempty_list.create here rem }
;;

let captured ~(here : [%call_pos]) () = Captured { here = [ here ] }
let ignored = Ignored

let capture ~(here : [%call_pos]) effect =
  let%bind.Effect () = effect in
  Effect.return (captured ~here ())
;;

let ignore = Effect.return Ignored

module Let_syntax = struct
  module Let_syntax = struct
    let bind effect ~f =
      match%bind.Effect effect with
      | Captured _ as ret -> Effect.return ret
      | Ignored -> f ()
    ;;

    let map effect ~f =
      match%map.Effect effect with
      | Captured _ as ret -> ret
      | Ignored -> f ()
    ;;
  end
end
