(* A small expression language for a total [eval]. *)

let spf = Stdlib.Printf.sprintf

type _ expr = 
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Eq : 'a expr * 'a expr -> bool expr

let rec eval : type a . a expr -> a = function
  | Int i -> i
  | Bool b -> b
  | Add (a, b) -> eval a + eval b
  | If (i, t, e) -> if (eval i) then (eval t) else (eval e)
  | Eq (a, b) -> (eval a) = (eval b)

let rec to_string : type a . a expr -> string = function
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | Add (a, b) -> spf ("(%s + %s)") (to_string a) (to_string b)
  | If (i, t, e) -> spf "(if %s then %s else %s)" (to_string i) (to_string t) (to_string e)
  | Eq (a, b) -> spf "%s = %s" (to_string a) (to_string b)

(* parser and typechecker? *)

type binOp = BAdd | BEq

type untyped = 
  | UInt of int
  | UBool of bool
  | UBin of untyped * binOp * untyped
  | UIte of untyped * untyped * untyped

(* ty is a witness for the type of an expr... *)
type _ ty = 
  | TInt : int ty
  | TBool : bool ty

let pp_ty : type a . a ty -> string = function
  | TInt -> "int"
  | TBool -> "bool"

type (_, _) eq = Refl : ('a, 'a) eq

let eq_ty : type a b . a ty -> b ty -> (a, b) eq option = fun a b ->
  match (a, b) with
  | (TInt, TInt) -> Some Refl
  | (TBool, TBool) -> Some Refl
  | _ -> None

type error = TypeError of string

let type_error : type a b . a ty -> b ty -> error = fun ta tb ->
  TypeError (spf "Can't unify %s and %s" (pp_ty ta) (pp_ty tb))

(* I can't write typecheck : untyped -> 'a expr *)

let (let*) = Result.bind

type any_expr = Any : ('a ty * 'a expr) -> any_expr

let rec typecheck : untyped -> (any_expr, error) result = function
  | UInt i -> Ok (Any (TInt, Int i))
  | UBool b -> Ok (Any (TBool, Bool b))
  | UBin (a, op, b) -> 
    let* a = typecheck a in
    let* b = typecheck b in
    (match (a, op, b) with 
    | (Any (TInt, a),    BAdd,  Any (TInt, b)) -> 
            Ok (Any (TInt, Add (a, b)))
    | (Any (TBool, a),   BEq,  Any (TBool, b)) -> 
            Ok (Any (TBool, Eq (a, b)))
    (* XXX: technically the error is incorrect if lhs typechecks w/ rhs
     * but the binop's expected type is wrong. *)
    | (Any (tl, _),      _,    Any (tr, _)) -> Error (type_error tl tr))
  | UIte (i, t, e) ->
    let* i = typecheck i in
    let* t = typecheck t in
    let* e = typecheck e in
    match (i, t, e) with
    | (Any (TBool, i), Any (tt, t), Any (tf, e)) ->
        (match eq_ty tt tf with
        | None -> Error (type_error tt tf)
        | Some Refl -> Ok (Any (tt, If (i, t, e))))
    | (Any (ti, _), _, _) -> Error (type_error TBool ti)
