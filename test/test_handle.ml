open Gadt_playground.Handle.MockHandle

let test_simple () =
  let h = open_read "foo" in
  let l1, h = read_line h in
  let l2, h = read_line h in
  let _ = close h in
  (* uh oh: this compiles: let (l3, h) = read_line h *)
  Alcotest.(check (option string)) "l1" l1 (Some "line1");
  Alcotest.(check (option string)) "l2" l2 (Some "line2")

let test_rw () =
  let h = open_rw "foo" in
  let l1, h = read_line h in
  let l2, h = read_line h in
  let h = write_line "line4" h in
  let l3, h = read_line h in
  let l4, h = read_line h in
  let _h = close h in
  Alcotest.(check (option string)) "l1" l1 (Some "line1");
  Alcotest.(check (option string)) "l2" l2 (Some "line2");
  Alcotest.(check (option string)) "l3" l3 (Some "line3");
  Alcotest.(check (option string)) "l4" l4 (Some "line4")

(* proptests *)

type cmd = Read | Write of string

let pp_cmd fmt = function
  | Read -> Format.fprintf fmt "Read"
  | Write s -> Format.fprintf fmt "Write %s" s

let gen_cmd =
  QCheck.Gen.(
    oneof_weighted
      [
        (2, return Read);
        (1, map (fun s -> Write s) (string_size ~gen:printable (int_range 0 20)));
      ])

let cmds = QCheck.Gen.(list_size (int_range 0 20) gen_cmd)

let run_cmds cmds =
  let rec go h = function
    | [] -> []
    | Read :: xs ->
        let v, h = read_line h in
        v :: go h xs
    | Write s :: xs ->
        let h = write_line s h in
        go h xs
  in
  let h = open_rw "ignored" in
  let init_lines = [ "line1"; "line2"; "line3" ] in
  (init_lines, go h cmds)

let pp_cmds = QCheck.Print.(list (fun c -> Format.asprintf "%a" pp_cmd c))

let test_rw_model =
  let step (queue : string list) (cmd : cmd) =
    match cmd with
    | Write s -> (queue @ [ s ], None) (* Append to back, no read result *)
    | Read -> (
        match queue with
        | [] -> ([], Some None) (* empty queue: read returns None *)
        | x :: xs -> (xs, Some (Some x)))
    (* pop front, read returns Some x *)
  in
  let test cmds =
    let init_lines, actual_reads = run_cmds cmds in
    let expected_reads =
      let _final_q, reads =
        List.fold_left
          (fun (q, acc) cmd ->
            match step q cmd with
            | q', Some v -> (q', acc @ [ v ])
            | q', None -> (q', acc))
          (init_lines, []) cmds
      in
      reads
    in
    actual_reads = expected_reads
  in
  QCheck.Test.make ~count:200 ~name:"rw handle behaves like a queue"
    (QCheck.make ~print:pp_cmds cmds)
    test

let () =
  Alcotest.run "handle"
    [
      ( "unit",
        [
          Alcotest.test_case "simple" `Quick test_simple;
          Alcotest.test_case "rw" `Quick test_rw;
        ] );
      ("property", List.map QCheck_alcotest.to_alcotest [ test_rw_model ]);
    ]
