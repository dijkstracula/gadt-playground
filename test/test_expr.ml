open Gadt_playground.Expr

(* -- Unit tests ---------------------------------------------------------- *)

let test_int () = Alcotest.(check int) "literal" 42 (eval (Int 42))
let test_add () = Alcotest.(check int) "add" 3 (eval (Add (Int 1, Int 2)))

let test_if_true () =
  Alcotest.(check int) "if true" 1 (eval (If (Bool true, Int 1, Int 0)))

(* -- QCheck generators --------------------------------------------------- *)

let rec gen_int_expr depth : int expr QCheck.Gen.t =
  let open QCheck.Gen in
  if depth <= 0 then map (fun i -> Int i) int
  else
    oneof
      [
        map (fun i -> Int i) int;
        map2
          (fun a b -> Add (a, b))
          (gen_int_expr (depth - 1))
          (gen_int_expr (depth - 1));
        map3
          (fun c t e -> If (c, t, e))
          (gen_bool_expr (depth - 1))
          (gen_int_expr (depth - 1))
          (gen_int_expr (depth - 1));
      ]

and gen_bool_expr depth : bool expr QCheck.Gen.t =
  let open QCheck.Gen in
  if depth <= 0 then map (fun b -> Bool b) bool
  else
    oneof
      [
        map (fun b -> Bool b) bool;
        map3
          (fun c t e -> If (c, t, e))
          (gen_bool_expr (depth - 1))
          (gen_bool_expr (depth - 1))
          (gen_bool_expr (depth - 1));
      ]

(* -- Property tests ------------------------------------------------------ *)

let eval_total =
  QCheck.Test.make ~name:"eval never raises" ~count:1000
    (QCheck.make (gen_int_expr 5))
    (fun e ->
      ignore (eval e);
      true)

let add_commutative =
  QCheck.Test.make ~name:"add is commutative" ~count:1000
    (QCheck.make (QCheck.Gen.pair (gen_int_expr 3) (gen_int_expr 3)))
    (fun (a, b) -> eval (Add (a, b)) = eval (Add (b, a)))

let if_same_branches =
  QCheck.Test.make ~name:"if with equal branches" ~count:1000
    (QCheck.make (QCheck.Gen.pair (gen_bool_expr 2) (gen_int_expr 3)))
    (fun (c, e) -> eval (If (c, e, e)) = eval e)

let add_identity =
  QCheck.Test.make ~name:"add zero is identity" ~count:1000
    (QCheck.make (gen_int_expr 4))
    (fun e -> eval (Add (e, Int 0)) = eval e)

(* -- Test runner --------------------------------------------------------- *)

let () =
  Alcotest.run "expr"
    [
      ( "unit",
        [
          Alcotest.test_case "int literal" `Quick test_int;
          Alcotest.test_case "addition" `Quick test_add;
          Alcotest.test_case "if true" `Quick test_if_true;
        ] );
      ( "properties",
        List.map QCheck_alcotest.to_alcotest
          [ eval_total; add_commutative; if_same_branches; add_identity ] );
    ]
