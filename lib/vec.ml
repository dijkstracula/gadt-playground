(* length-indexed vectors *)

module Nat = struct
  (* explicitly not making zero and succ abstract, so ocaml knows these are
     distinct types; typechecking `Cons` requires injectivity so it can
     compute the "path" to 'n.
  *)
  type zero = Z
  type 'n succ = S

  type (_, _, _) add =
    | AddZ : (zero, 'n, 'n) add
    | AddS : ('m, 'n, 'p) add -> ('m succ, 'n, 'p succ) add

  (* forall m n : Nat, (m + 1) = n -> m = (n + 1) *)
  let rec plus_m1_n : type m n p . (m succ, n, p) add -> (m, n succ, p) add = function
    | AddS h (* h: (m, n, p) add *) -> match h with
      | AddZ -> AddZ
      | AddS _ ->
        let ih (* (m-1, n, p) add *) = plus_m1_n h in
        AddS ih

  (* forall m n : Nat, m = (n+1) -> (m+1 = n *)
  let rec plus_m_n1 : type m n p . (m, n succ, p) add -> (m succ, n, p) add = function
    | AddZ -> AddS AddZ
    | AddS h -> AddS (plus_m_n1 h)
end

type (_, _) vec =
  | Nil : ('a, Nat.zero) vec
  | Cons : 'a * ('a, 'n) vec -> ('a, 'n Nat.succ) vec

let head : ('a, _ Nat.succ) vec -> 'a = function Cons (x, _) -> x
let tail : ('a, 'n Nat.succ) vec -> ('a, 'n) vec = function Cons (_, xs) -> xs

(* NB: argh, don't forget that if you match more than one constructor
   you need to introduce locally-abstract types with `type n`. *)

let rec length : type n. (_, n) vec -> int = function
  | Nil -> 0
  | Cons (_, xs) -> 1 + length xs

let rec map : type n. ('a -> 'b) -> ('a, n) vec -> ('b, n) vec = fun f xs ->
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

let rec map2 : type n. ('a -> 'b -> 'c) -> 
                      ('a, n) vec -> 
                      ('b, n) vec -> 
                      ('c, n) vec = fun f xs ys ->
  match (xs, ys) with
  | (Nil, Nil) -> Nil
  | (Cons (x, xs), Cons (y, ys)) -> Cons (f x y, map2 f xs ys)

let zip : type n . ('a, n) vec -> ('b, n) vec ->  (('a * 'b), n) vec = fun xs ys ->
    let f x y = (x, y) in
    map2 f xs ys

(* TODO: How can I constrain an 'a to be [deriving show]? *)
let pp_int : type n . (int, n) vec Fmt.t = fun ppf xs ->
  let rec go : type n . ('a, n) vec -> unit = function
    | Nil -> ()
    | Cons (x, Nil) -> 
      Fmt.pf ppf "%d" x
    | Cons (x, xs) -> 
      Fmt.pf ppf "%d; "x;
      go xs
  in
  Fmt.pf ppf "["; go xs; Fmt.pf ppf "]"

let rec eq : type n . ('a, n) vec -> ('b, n) vec -> bool = fun xs ys ->
  match (xs, ys) with
  | (Nil, Nil) -> true
  | (Cons (x, xs), Cons (y, ys)) -> x = y && eq xs ys

let rec append : type m n p . (m, n, p) Nat.add -> ('a, m) vec -> ('a, n) vec -> ('a, p) vec =
  fun h xs ys -> match (h, xs) with
  | AddZ, Nil -> ys
  | AddS h', Cons (x, xs) -> Cons (x, append h' xs ys)

let reverse : type n . ('a, n) vec -> ('a, n) vec = fun xs ->
  let rec vec_n_add : type n . ('a, n) vec -> (n, Nat.zero, n) Nat.add = function
    | Nil -> AddZ
    | Cons (_, xs) -> Nat.AddS (vec_n_add xs) in

  let rec go : type xs_len acc_len . 
    ('a, xs_len) vec -> 
    ('a, acc_len) vec ->
    (xs_len, acc_len, n) Nat.add -> 
    ('a, n) vec = fun xs acc h -> match (xs, h) with
        | Nil, AddZ -> acc
        | Cons (x, xs), _ -> 
          let l = Nat.plus_m1_n h in  
          go xs (Cons (x, acc)) l
  in
  go xs Nil (vec_n_add xs)


let rec to_list : type n . ('a, n) vec -> 'a list = function
  | Nil -> []
  | Cons (x, xs) -> x :: to_list xs

module Operators = struct
  let ( @:: ) x xs = Cons (x, xs)
end
