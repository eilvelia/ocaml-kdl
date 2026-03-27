include Ast
include Err
include Pretty
include Typeannots
module L = Lens

let document_revised =
  MenhirLib.Convert.Simplified.traditional2revised Parser.document

let parse state : (t, error) result =
  try Ok (document_revised @@ Lexer.main_tokenizer state) with
  | Custom_lexing_error (msg, loc) -> Error (msg, loc)
  | Custom_parsing_error (msg, loc) -> Error (msg, loc)
  | Parser.Error ->
    (* note: this doesn't seem to print correct locations with lookahead tokens *)
    Error ("Syntax error", Lexer.get_location state)

let of_string ?compat ?fname input =
  parse (Lexer.tokenizer_state_of_string ?compat ?fname input)

let of_string_exn ?compat ?fname input =
  match of_string ?compat ?fname input with
  | Ok x -> x
  | Error err -> raise @@ Parse_error err

let of_channel ?compat ?fname input =
  parse (Lexer.tokenizer_state_of_channel ?compat ?fname input)

let of_channel_exn ?compat ?fname input =
  match of_channel ?compat ?fname input with
  | Ok x -> x
  | Error err -> raise @@ Parse_error err

let of_chunk_gen ?compat ?fname f =
  parse (Lexer.tokenizer_state_of_fun ?compat ?fname f)

let of_chunk_gen_exn ?compat ?fname f =
  match of_chunk_gen ?compat ?fname f with
  | Ok x -> x
  | Error err -> raise @@ Parse_error err
