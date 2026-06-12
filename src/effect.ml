open! Core
include Bonsai.Effect
include Ui_effect_of_deferred

type _ t += Show_cursor : unit t | Hide_cursor : unit t

let write_ansi_to_stdout string =
  Out_channel.output_string stdout string;
  Out_channel.flush stdout
;;

let () =
  Core.Hashtbl.add_exn
    Expert.handlers
    ~key:Stdlib.Obj.Extension_constructor.(id (of_val Show_cursor))
    ~data:(fun _ -> write_ansi_to_stdout "\027[?25h")
;;

let () =
  Core.Hashtbl.add_exn
    Expert.handlers
    ~key:Stdlib.Obj.Extension_constructor.(id (of_val Hide_cursor))
    ~data:(fun _ -> write_ansi_to_stdout "\027[?25l")
;;

let set_cursor = Cursor.set_cursor_position
let show_cursor = Show_cursor
let hide_cursor = Hide_cursor
let eprint_s sexp = of_sync_fun Core.eprint_s sexp
