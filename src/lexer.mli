val resolve_escapes : string -> string

val skip_whitespace_line : ?offset:int -> string -> int

val is_fully_whitespace : string -> bool

val is_valid_ident : string -> bool

val escape_string : string -> string

type tokenizer_yyrecord

val make_tokenizer_yyrecord : ?fname:string -> string -> tokenizer_yyrecord

val main_tokenizer : tokenizer_yyrecord -> unit -> Parser.token * Lexing.position * Lexing.position

val get_location : tokenizer_yyrecord -> Lexing.position * Lexing.position
