include Ast
include Err
include Pretty
include Typeannots
module L = Lens
open Printf

let document_revised =
  MenhirLib.Convert.Simplified.traditional2revised Parser.document

let parse ?(fname = "") lexbuf : (t, error) result =
  Sedlexing.set_filename lexbuf fname;
  Sedlexing.set_position lexbuf Loc.zero_pos;
  try Ok (document_revised @@ Lexer.main_tokenizer lexbuf) with
  | Sedlexing.InvalidCodepoint code ->
    Error (sprintf "Invalid code point %d" code, Sedlexing.lexing_bytes_positions lexbuf)
  | Sedlexing.MalFormed ->
    Error ("Malformed UTF-8", Sedlexing.lexing_bytes_positions lexbuf)
  | Custom_lexing_error msg ->
    Error (msg, Sedlexing.lexing_bytes_positions lexbuf)
  | Custom_parsing_error (msg, loc) ->
    Error (msg, loc)
  | Parser.Error ->
    (* This does not seem to print correct locations with lookahead tokens *)
    Error ("Syntax error", Sedlexing.lexing_bytes_positions lexbuf)

let from_string ?fname input = parse ?fname (Sedlexing.Utf8.from_string input)

let from_string_exn ?fname input =
  match from_string ?fname input with
  | Ok x -> x
  | Error err -> raise @@ Parse_error err

let from_channel ?fname input = parse ?fname (Sedlexing.Utf8.from_channel input)

let from_channel_exn ?fname input =
  match from_channel ?fname input with
  | Ok x -> x
  | Error err -> raise @@ Parse_error err

let of_string = from_string
let of_string_exn = from_string_exn
