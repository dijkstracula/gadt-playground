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


module Operators = struct
  let ( @:: ) x xs = Cons (x, xs)
end
