open Gadt_playground.Vec
open Gadt_playground.Vec.Operators

type three = Nat.(zero succ succ succ)

let int_vec : (int, 'n) vec Alcotest.testable =
  Alcotest.testable pp_int eq

let v1 : (int, three) vec = 1 @:: 2 @:: 3 @:: Nil
let v2 : (int, three) vec = 2 @:: 3 @:: 4 @:: Nil
let v3 : (string, three) vec = "a" @:: "b" @:: "c" @:: Nil

let test_ht () = 
  Alcotest.(check int) "first v1" 1 (head v1);
  Alcotest.(check int) "second v1" 2 (tail v1 |> head)

let test_len () = 
  Alcotest.(check int) "length v1" 3 (length v1)

let test_pp () = 
  Alcotest.(check string) "pp v1" "[1; 2; 3]" (Fmt.str "%a" pp_int v1)

let test_eq () =
  Alcotest.(check bool) "eq v1 v1" true (eq v1 v1);
  Alcotest.(check bool) "eq v2 v2" true (eq v2 v2);
  Alcotest.(check bool) "eq v1 v2" false (eq v1 v2)

let test_map () =
  Alcotest.(check int_vec) "map id" v1 (map Fun.id v1);
  Alcotest.(check int_vec) "map add1" v2 (map ((+) 1) v1);
  Alcotest.(check int_vec) "map2 plus" (3 @:: 5 @:: 7 @:: Nil) (map2 (+) v1 v2)

let test_zip () =
  (* TODO: too lazy to write a testable *)
  Alcotest.(check (pair int string)) "zip head" (1, "a") (head (zip v1 v3))

let () =
  Alcotest.run "vec" [
    "unit", [
      Alcotest.test_case "length" `Quick test_len;
      Alcotest.test_case "head/tail" `Quick test_ht;
      Alcotest.test_case "pp" `Quick test_pp;
      Alcotest.test_case "eq" `Quick test_eq; 
      Alcotest.test_case "map" `Quick test_map; 
      Alcotest.test_case "zip" `Quick test_zip; 
    ]
  ]
  
