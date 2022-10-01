exception CustomParsingError of string * Loc.t
exception CustomLexingError of string

(* TODO: Create a sum type instead of strings? *)

type error_loc = Loc.t

type error = string * error_loc

let show_error (msg, loc) =
  Printf.sprintf "%s: %s" (Loc.show loc) msg

exception Parse_error of error
