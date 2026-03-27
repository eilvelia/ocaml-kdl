val is_valid_ident : string -> bool

val escape_string : string -> string

type tokenizer_state

val tokenizer_state_of_string : ?compat:[ `V1 ] -> ?fname:string -> string -> tokenizer_state

val tokenizer_state_of_channel : ?compat:[ `V1 ] -> ?fname:string -> in_channel -> tokenizer_state

val tokenizer_state_of_fun : ?compat:[ `V1 ] -> ?fname:string -> (bytes -> offset:int -> len:int -> int) -> tokenizer_state

val main_tokenizer : tokenizer_state -> unit -> Parser.token * Lexing.position * Lexing.position

val get_location : tokenizer_state -> Lexing.position * Lexing.position
(** Get token location *)
