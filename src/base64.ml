let decode_table = lazy (
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let table = Array.make 256 0xff in
  String.iteri (fun i ch -> table.(Char.code ch) <- i) alphabet;
  table
)

let (.%[]) = String.unsafe_get
let (.%()) = Array.unsafe_get

external bytes_unsafe_set_int8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"

(* Returns length of the string ignoring arbitrary number of '=' characters
   at the end of the string *)
let[@inline] count_len str =
  (* More efficient than the tail-recursive version *)
  let i = ref (String.length str - 1) in
  while !i >= 0 && str.%[!i] = '=' do decr i done;
  !i + 1

exception Error of string

let[@inline] error descr = raise (Error descr)

let decode input =
  let table = Lazy.force decode_table in
  let real_len = count_len input in
  let padding = (4 - real_len land 3) land 3 in
  if padding = 3 then
    error "Invalid base64: a padding of 3 sextets is invalid";
  (* padding can be either 0, 1, or 2 *)
  let out_len = (real_len + padding) / 4 * 3 - padding in
  let out = Bytes.create out_len in
  let i = ref 0 and j = ref 0 in
  while !i < real_len do
    (* Transform 4 sextets into 3 octets *)
    let ch1 = input.%[!i] in
    let ch2 = input.%[!i + 1] in
    let sextet1 = table.%(Char.code ch1) in
    let sextet2 = table.%(Char.code ch2) in
    if sextet1 = 0xff || sextet2 = 0xff then error "Invalid base64 character";
    (* 6 + 2 bits *)
    let octet1 = sextet1 lsl 2
             lor sextet2 lsr 4 in
    bytes_unsafe_set_int8 out !j octet1;
    incr j;
    if !i + 2 < real_len then begin
      let ch3 = input.%[!i + 2] in
      let sextet3 = table.%(Char.code ch3) in
      if sextet3 = 0xff then error "Invalid base64 character";
      (* 4 + 4 bits *)
      let octet2 = (sextet2 land 0b1111) lsl 4
               lor sextet3 lsr 2 in
      bytes_unsafe_set_int8 out !j octet2;
      incr j;
      if !i + 3 < real_len then begin
        let ch4 = input.%[!i + 3] in
        let sextet4 = table.%(Char.code ch4) in
        if sextet4 = 0xff then error "Invalid base64 character";
        (* 2 + 6 bits *)
        let octet3 = (sextet3 land 0b11) lsl 6
                 lor sextet4 in
        bytes_unsafe_set_int8 out !j octet3;
        incr j
      end
    end;
    i := !i + 4
  done;
  assert (!j = Bytes.length out);
  out
