include Ast
include Err
include Pretty
include Typeannots
module L = Lens

let document_revised =
  MenhirLib.Convert.Simplified.traditional2revised Parser.document
let parse yyrecord : (t, error) result =
  try Ok (document_revised @@ Lexer.main_tokenizer yyrecord) with
  | Custom_lexing_error msg ->
    Error (msg, Lexer.get_location yyrecord)
  | Custom_parsing_error (msg, loc) ->
    Error (msg, loc)
  | Parser.Error ->
    (* note: this doesn't seem to print correct locations with lookahead tokens *)
    Error ("Syntax error", Lexer.get_location yyrecord)

let from_string ?fname input =
  parse (Lexer.make_tokenizer_yyrecord ?fname input)

let from_string_exn ?fname input =
  match from_string ?fname input with
  | Ok x -> x
  | Error err -> raise @@ Parse_error err

let from_channel ?fname input =
  (* TODO: Implement buffer refiling *)
  from_string ?fname (In_channel.input_all input)

let from_channel_exn ?fname input =
  match from_channel ?fname input with
  | Ok x -> x
  | Error err -> raise @@ Parse_error err

let of_string = from_string
let of_string_exn = from_string_exn
