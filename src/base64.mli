val decode : string -> bytes
(** Decode a base64-encoded string. This can decode base64 both with and
    without padding. The string can end with arbitrary number of [=] characters.
    @raise Failure in case of invalid base64 *)
