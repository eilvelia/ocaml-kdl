open Ast

(* note: we can possibly replace option with result for more detailed errors *)

type ('s, 'a) lens = {
  get : 's -> 'a option;
  set : 'a -> 's -> 's option;
}

let get a lens = lens.get a

let set a v lens = lens.set v a

let get_exn a lens =
  match lens.get a with
  | Some v -> v
  | None -> failwith "Lens failed to match"

let set_exn a v lens =
  match lens.set v a with
  | Some v -> v
  | None -> failwith "Lens failed to match"

(* note: update can possibly be added to the definition of [lens] to increase
   performance with more specialized implementations *)

let update f a lens =
  match lens.get a with
  | None -> None
  | Some value -> match f value with
    | None -> None
    | Some value' -> lens.set value' a

let compose l1 l2 = {
  get = (fun x ->
    match l2.get x with
    | Some x' -> l1.get x'
    | None -> None
  );
  set = (fun v a -> update (l1.set v) a l2)
}

let (//) l1 l2 = compose l2 l1

let (|--) = (//)

let (.@()) = get
let (.@()<-) a l v = set a v l

let (.@!()) = get_exn
let (.@!()<-) a l v = set_exn a v l

let node_name = {
  get = (fun node -> Some node.name);
  set = (fun name node -> Some { node with name })
}

let node_annot = {
  get = (fun node -> node.annot);
  set = (fun annot node -> Some { node with annot = Some annot })
}

(* Unset the annotation by passing None *)
let node_annot_opt = {
  get = (fun node -> Some node.annot);
  set = (fun annot node -> Some { node with annot })
}

let args = {
  get = (fun node -> Some node.args);
  set = (fun args node -> Some { node with args })
}

let props = {
  get = (fun node -> Some node.props);
  set = (fun props node -> Some { node with props })
}

let children = {
  get = (fun node -> Some node.children);
  set = (fun children node -> Some { node with children })
}

let top = {
  get = (function node :: _ -> Some node | [] -> None);
  set = (fun node -> function _ :: xs -> Some (node :: xs) | [] -> None)
}

open struct
  let nth_and_replace n x' list =
    let found = ref false in
    (* Note: Unlike List.mapi, this stops iterating when we've found the element *)
    let[@tail_mod_cons] rec go i = function
      | [] -> []
      | _ :: xs when i = n -> found := true; x' :: xs
      | x :: xs -> x :: go (i + 1) xs
    in
    let result = go 0 list in
    if !found then Some result else None

  let filter_and_replace f replace_list list =
    let found = ref false in
    let f (replace, result) x =
      if f x then begin
        found := true;
        match replace with
        | x' :: xs -> xs, x' :: result
        | [] -> [], x :: result
      end else
        replace, x :: result
    in
    let _, list = List.fold_left f (replace_list, []) list in
    if !found then Some (List.rev list) else None

  let[@inline] matches_node ?annot name node =
    String.equal node.name name && (match annot with
      | Some a -> (match node.annot with
        | Some a' -> String.equal a a'
        | None -> false)
      | None -> true)

  let rec find_node n annot name = function
    | [] -> None
    | x :: xs when matches_node ?annot name x ->
      if n <= 0 then Some x else find_node (n - 1) annot name xs
    | _ :: xs -> find_node n annot name xs

  let find_and_replace_node nth annot name x' list =
    let found = ref false in
    let[@tail_mod_cons] rec go n = function
      | [] -> []
      | x :: xs when matches_node ?annot name x ->
        if n <= 0 then (found := true; x' :: xs) else x :: go (n - 1) xs
      | x :: xs -> x :: go n xs
    in
    let result = go nth list in
    if !found then Some result else None
end

let nth n = {
  get = (fun list -> List.nth_opt list n);
  set = (fun x' list -> nth_and_replace n x' list)
}

(* these operations are O(n), and update is quite inefficient *)
let arg n = {
  (* Inlined [nth] instead of [args // nth n] *)
  get = (fun node -> List.nth_opt node.args n);
  set = (fun arg' node -> match nth_and_replace n arg' node.args with
    | Some args -> Some { node with args }
    | None -> None)
}

let first_arg = arg 0

let prop key = {
  get = (fun node -> List.assoc_opt key node.props);
  set = (fun v' node ->
    let found = ref false in
    let f (k, v) = if k = key then (found := true; k, v') else k, v in
    let props = List.map f node.props in
    if !found then Some { node with props } else None
  )
}

let node ?(nth = 0) ?annot (name : string) =
  {
    get = (fun nodes -> find_node nth annot name nodes);
    set = (fun node' nodes -> find_and_replace_node nth annot name node' nodes)
  }

let node_many ?annot (name : string) =
  let matches = matches_node ?annot name in
  {
    get = (fun nodes ->
      match List.filter matches nodes with [] -> None | xs -> Some xs);
    set = (fun nodes' nodes -> filter_and_replace matches nodes' nodes)
  }

let node_nth : int -> (node list, node) lens = nth

(* TODO: get node by annot only? *)

let child ?nth ?annot name = children // node ?nth ?annot name
let child_many ?annot name = children // node_many ?annot name
let child_nth n = children // node_nth n

let value : (annot_value, value) lens = {
  get = (fun (_, v) -> Some v);
  set = (fun v' (a, _) -> Some (a, v'))
}

let annot : (annot_value, string) lens = {
  get = (fun (a, _) -> a);
  set = (fun a' (_, v) -> Some (Some a', v))
}

let annot_opt : (annot_value, string option) lens = {
  get = (fun (a, _) -> Some a);
  set = (fun a' (_, v) -> Some (a', v))
}

let string = {
  get = (function `String str -> Some str | _ -> None);
  set = (fun value' _value -> Some (`String value'))
}

let number : (value, number) lens = {
  get = (function #number as num -> Some num | _ -> None);
  set = (fun num _ -> Some (num :> value))
}

let string_number : (value, string) lens = {
  get = (function #number as num -> Some (Num.to_string num) | _ -> None);
  set = (fun x _ -> Num.of_string x)
}

let float_number : (value, float) lens = {
  get = (function #number as num -> Some (Num.to_float num) | _ -> None);
  set = (fun x _ -> Some (Num.of_float x))
}

let int_number : (value, int) lens = {
  get = (function #number as num -> Num.to_int num | _ -> None);
  set = (fun x _ -> Some (Num.of_int x))
}

let int32_number : (value, int32) lens = {
  get = (function #number as num -> Num.to_int32 num | _ -> None);
  set = (fun x _ -> Some (Num.of_int32 x))
}

let int64_number : (value, int64) lens = {
  get = (function #number as num -> Num.to_int64 num | _ -> None);
  set = (fun x _ -> Some (Num.of_int64 x))
}

let nativeint_number : (value, nativeint) lens = {
  get = (function #number as num -> Num.to_nativeint num | _ -> None);
  set = (fun x _ -> Some (Num.of_nativeint x))
}

let bool = {
  get = (function `Bool b -> Some b | _ -> None);
  set = (fun value' _value -> Some (`Bool value'))
}

let null = {
  get = (function `Null -> Some () | _ -> None);
  set = (fun _ _ -> Some `Null)
}

let string_value : (annot_value, string) lens = value // string
let number_value : (annot_value, number) lens = value // number
let string_number_value : (annot_value, string) lens = value // string_number
let float_number_value : (annot_value, float) lens = value // float_number
let int_number_value : (annot_value, int) lens = value // int_number
let int32_number_value : (annot_value, int32) lens = value // int32_number
let int64_number_value : (annot_value, int64) lens = value // int64_number
let nativeint_number_value : (annot_value, nativeint) lens =
  value // nativeint_number
let bool_value : (annot_value, bool) lens = value // bool
let null_value : (annot_value, unit) lens = value // null

let filter f = {
  get = (fun list -> Some (List.filter f list));
  set = (fun replace list -> filter_and_replace f replace list)
}

open struct
  exception Short_circuit

  let mapm_option f list =
    let g a = match f a with Some x -> x | None -> raise_notrace Short_circuit in
    try Some (List.map g list)
    with Short_circuit -> None
end

let each l = {
  get = (fun list -> mapm_option l.get list);
  set = (fun replace_list list ->
    let f (replace, result) v =
      match replace with
      | v' :: replace_rest -> (match l.set v' v with
        | Some x -> replace_rest, x :: result
        | None -> raise_notrace Short_circuit)
      | [] -> [], v :: result
    in
    try
      let _, list = List.fold_left f (replace_list, []) list in
      Some (List.rev list)
    with Short_circuit -> None
  )
}
