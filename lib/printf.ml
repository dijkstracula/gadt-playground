(* Type-safe printf *)

(* given a `('a, 'b) fmt`:
    'a is the final result type (string for sprintf,
    unit for printf, some sort of continuation for
    kprintf???
    'b is "what's needed to provide the final result type"
*)
type (_, _) fmt =
  (* Lit doesn't consume any arguments, so the input
   * and output types are the same. *)
  | Lit : string -> ('a, 'a) fmt
  (* FInt / FString / FBool consume one argument, so
   * the format function gains an int / string / bool
   * argument. *)
  | FInt : ('a, int -> 'a) fmt
  | FString : ('a, string -> 'a) fmt
  | FBool : ('a, bool -> 'a) fmt
  | Cat : ('b, 'c) fmt * ('a, 'b) fmt -> ('a, 'c) fmt
  | Pad : int -> ('a, string -> 'a) fmt

let lit s = Lit s
let ( ^^ ) a b = Cat (a, b)

let sprintf =
  let rec go : type a b. Buffer.t -> (a, b) fmt -> (unit -> a) -> b =
   fun buf fmt k ->
    match fmt with
    (* b = a *)
    | Lit s ->
        Buffer.add_string buf s;
        k ()
    (* (int/string/bool -> a) = b *)
    | FInt ->
        fun n ->
          Buffer.add_string buf (Int.to_string n);
          k ()
    | FString ->
        fun s ->
          Buffer.add_string buf s;
          k ()
    | FBool ->
        fun b ->
          Buffer.add_string buf (Bool.to_string b);
          k ()
    (* bc : *)
    | Cat (bc, ab) ->
        let k () = go buf ab k in
        go buf bc k
    | Pad n ->
        fun s ->
          let rm = n - String.length s in
          if rm > 0 then Buffer.add_string buf (String.make rm ' ');
          Buffer.add_string buf s;
          k ()
  in
  fun fmt ->
    let buf = Buffer.create 16 in
    go buf fmt (fun () -> Buffer.contents buf)
