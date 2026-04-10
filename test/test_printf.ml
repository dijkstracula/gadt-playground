open Gadt_playground.Printf
open QCheck

let test_simple () =
  Alcotest.(check string) "lit" "hello" (sprintf (lit "hello"));
  Alcotest.(check string) "fint" "42" (sprintf FInt 42);
  Alcotest.(check string) "cat" "hello 42" (sprintf (lit "hello " ^^ FInt) 42);
  Alcotest.(check string)
    "cat" "hello 42"
    (sprintf (FString ^^ FInt) "hello " 42)

let test_pad () = Alcotest.(check string) "len 5" "   hi" (sprintf (Pad 5) "hi")

(* property tests *)

let lits =
  Test.make ~name:"literals are unchanged" ~count:1000 (make Gen.string)
    (fun s -> sprintf (lit s) = s)

let concats =
  Test.make ~name:"cat of two literals is concat" ~count:1000
    (make (Gen.pair Gen.string Gen.string))
    (fun (a, b) -> sprintf (lit a ^^ lit b) = a ^ b)

(* TODO:
     sprintf (fmt1 ^^ fmt2) ...args... = sprintf fmt1 ... ^ sprintf fmt2 ... —
*)

let pad_length =
  Test.make ~name:"pad is at least n chars" ~count:1000
    (make (Gen.pair Gen.nat_small Gen.string_small))
    (fun (n, s) -> String.length (sprintf (Pad n) s) >= max n (String.length s))

let pad_noop_when_long =
  Test.make ~name:"pad is a noop for long strings"
    (make (Gen.pair Gen.nat_small Gen.string))
    (fun (n, s) -> String.length s >= n ==> (sprintf (Pad n) s = s))

let () =
  Alcotest.run "printf"
    [
      ( "unit",
        [
          Alcotest.test_case "simple" `Quick test_simple;
          Alcotest.test_case "pad" `Quick test_pad;
        ] );
      ( "general properties",
        List.map QCheck_alcotest.to_alcotest [ lits; concats ] );
      ( "pad",
        List.map QCheck_alcotest.to_alcotest [ pad_length; pad_noop_when_long ]
      );
    ]
