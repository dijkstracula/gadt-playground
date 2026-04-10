(* A small expression language for a total [eval]. *)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

let rec eval : type a. a expr -> a = function
  | Int i -> i
  | Bool b -> b
  | Add (a, b) -> eval a + eval b
  | If (i, t, e) -> if eval i then eval t else eval e
