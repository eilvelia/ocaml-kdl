exception Custom_parsing_error of string * Loc.t
exception Custom_lexing_error of string * Loc.t

type error_loc = Loc.t

type error = string * error_loc

let pp_error fmt (msg, loc : error) =
  Format.fprintf fmt "%s: %s" (Loc.to_string loc) msg

exception Parse_error of error
