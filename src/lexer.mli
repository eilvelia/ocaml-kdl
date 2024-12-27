val is_valid_ident : string -> bool

val escape_string : string -> string

type tokenizer_state

val tokenizer_state_of_string : ?fname:string -> string -> tokenizer_state

val tokenizer_state_of_channel : ?fname:string -> in_channel -> tokenizer_state

val tokenizer_state_of_fun : ?fname:string -> (bytes -> offset:int -> len:int -> int) -> tokenizer_state

val main_tokenizer : tokenizer_state -> unit -> Parser.token * Lexing.position * Lexing.position

val get_location : tokenizer_state -> Lexing.position * Lexing.position
(** Get token location *)
