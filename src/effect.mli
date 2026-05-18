open! Core

include module type of struct
  include Bonsai.Effect
end

val show_cursor : unit t
val hide_cursor : unit t
val of_deferred_fun : ('query -> 'response Async.Deferred.t) -> 'query -> 'response t
val of_deferred_thunk : (unit -> 'response Async.Deferred.t) -> 'response t

val of_deferred_fun'
  :  ('query -> on_exn:(Exn.t -> unit) -> 'response Async.Deferred.t)
  -> 'query
  -> 'response t

val of_deferred_thunk'
  :  (unit -> on_exn:(Exn.t -> unit) -> 'response Async.Deferred.t)
  -> 'response t

val set_cursor : local_ Bonsai.graph -> (Cursor.t option -> unit t) Bonsai.t
val eprint_s : Sexp.t -> unit t
