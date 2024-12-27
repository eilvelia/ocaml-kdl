let read_file filename =
  In_channel.with_open_bin filename In_channel.input_all

let normalize_numbers =
  let normalize_float str =
    let strlen = String.length str in
    let buf = Buffer.create strlen in
    String.iteri (fun i -> function
      | 'e' | 'E' when i + 1 < strlen && String.contains "0123456789" str.[i + 1] ->
        Buffer.add_string buf "E+"
      | 'e' -> Buffer.add_char buf 'E'
      | '_' -> ()
      | ch -> Buffer.add_char buf ch
    ) str;
    Buffer.contents buf
  in
  let rec map_value (num : [> Kdl.number ]) =
    match num with
    | `Int x -> `Int x
    | `Int_raw str -> `Int_raw (Z.to_string (Z.of_string str))
    | `Float_raw str -> `Float_raw (normalize_float str)
    | x -> x
  and map_nodes nodes = List.map map_node nodes
  and map_node (node : Kdl.node) : Kdl.node =
    { node with
      args = List.map (fun (a, v) -> a, map_value v) node.args;
      props = List.map (fun (k, (a, v)) -> k, (a, map_value v)) node.props;
      children = map_nodes node.children;
    }
  in
  map_nodes

let (let@) f x = f x

let run test =
  let failing_test = Filename.check_suffix test "_fail.kdl" in
  let input_file = Filename.concat "test_cases/input" test in
  let expect_file = Filename.concat "test_cases/expected_kdl" test in
  let expected =
    if failing_test then
      "[failing]"
    else String.trim (read_file expect_file)
  in
  let@ ic = In_channel.with_open_bin input_file in
  match Kdl.of_channel ic with
  | Error _ when failing_test -> `Success
  | Error err ->
    Format.printf "Test %s failed:\n---(error)---\n%a\n---(expected)---\n%s\n\n"
      test Kdl.pp_error err expected;
    `Fail
  | exception exn ->
    let err = Printexc.to_string exn in
    let backtrace = String.trim (Printexc.get_backtrace ()) in
    Format.printf
      "Test %s failed:\n---(error)---\n[exception]\n%s\n%s\n---(expected)---\n%s\n\n"
      test err backtrace expected;
    `Fail
  | Ok result ->
    let result = normalize_numbers result in
    let actual = String.trim (Kdl.to_string result) in
    if String.equal actual expected then
      `Success
    else begin
      Printf.printf "Test %s produces different output:\n" test;
      Printf.printf "---(actual)---\n%s\n---(expected)---\n%s\n\n" actual expected;
      `Fail
    end

let kdl_suite_runner () =
  Printexc.record_backtrace true;
  Kdl.indent := 4;
  let tests = Sys.readdir "test_cases/input" in
  let total = ref 0 and successful = ref 0 in
  Array.iter (fun test ->
    if Filename.extension test = ".kdl" then begin
      incr total;
      match run test with
      | `Fail -> ()
      | `Success -> incr successful
    end
  ) tests;
  let failed = !total - !successful in
  Printf.printf "--- KDL test suite: %d/%d (failed: %d)%s ---\n"
    !successful !total failed (if failed = 0 then " 🎉" else "");
  assert (!total > 10);
  if failed > 0 then exit 1

let () = kdl_suite_runner ()
