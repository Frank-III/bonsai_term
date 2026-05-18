open! Core
open Async

type t =
  { dispose : bool option
  ; nosig : bool option
  ; mouse : Mouse_reporting_config.t option
  ; bpaste : bool option
  ; reader : Reader.t option
  ; writer : Writer.t option
  ; for_mocking : Notty_async.For_mocking.t option
  }
