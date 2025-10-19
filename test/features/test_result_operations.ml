(* Test suite for Result module complexity analysis *)

(* Helper to check complexity *)
let check_and_print code =
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  match inferred with
  | [(_, cost)] -> print_endline (Gauge.Cost_model.to_string cost)
  | _ -> print_endline "Error: expected single function"

(* Basic Result operations - O(1) *)

let%expect_test "Result.ok is O(1)" =
  check_and_print "let f x = Result.ok x";
  [%expect {| O(1) |}]

let%expect_test "Result.error is O(1)" =
  check_and_print "let f e = Result.error e";
  [%expect {| O(1) |}]

let%expect_test "Result.is_ok is O(1)" =
  check_and_print "let f res = Result.is_ok res";
  [%expect {| O(1) |}]

let%expect_test "Result.is_error is O(1)" =
  check_and_print "let f res = Result.is_error res";
  [%expect {| O(1) |}]

let%expect_test "Result.value with default is O(1)" =
  check_and_print "let f res default = Result.value res ~default";
  [%expect {| O(1) |}]

let%expect_test "Result.get_ok is O(1)" =
  check_and_print "let f res = Result.get_ok res";
  [%expect {| O(1) |}]

let%expect_test "Result.get_error is O(1)" =
  check_and_print "let f res = Result.get_error res";
  [%expect {| O(1) |}]

(* Result transformations - O(1) wrapper *)

let%expect_test "Result.map is O(1) for O(1) function" =
  check_and_print "let f res = Result.map (fun x -> x + 1) res";
  [%expect {| O(1) |}]

let%expect_test "Result.map_error is O(1) for O(1) function" =
  check_and_print {|let f res = Result.map_error (fun e -> e + 1) res|};
  [%expect {| O(1) |}]

let%expect_test "Result.bind is O(1) for O(1) function" =
  check_and_print "let f res = Result.bind res (fun x -> Ok (x + 1))";
  [%expect {| O(1) |}]

let%expect_test "Result.join flattens nested result in O(1)" =
  check_and_print "let f res = Result.join res";
  [%expect {| O(1) |}]

let%expect_test "Result.iter is O(1) for O(1) function" =
  check_and_print "let f res = Result.iter (fun x -> ignore x) res";
  [%expect {| O(1) |}]

let%expect_test "Result.iter_error is O(1) for O(1) function" =
  check_and_print "let f res = Result.iter_error (fun e -> ignore e) res";
  [%expect {| O(1) |}]

let%expect_test "Result.fold is O(1) for O(1) functions" =
  check_and_print "let f res = Result.fold ~ok:(fun x -> x + 1) ~error:(fun _ -> 0) res";
  [%expect {| O(1) |}]

(* Result conversions - O(1) *)

let%expect_test "Result.to_option is O(1)" =
  check_and_print "let f res = Result.to_option res";
  [%expect {| O(1) |}]

let%expect_test "Result.to_list is O(1)" =
  check_and_print "let f res = Result.to_list res";
  [%expect {| O(1) |}]

let%expect_test "Result.to_seq is O(1)" =
  check_and_print "let f res = Result.to_seq res";
  [%expect {| O(1) |}]

(* Result comparison - O(1) *)

let%expect_test "Result.equal is O(1) for O(1) equality" =
  check_and_print "let f res1 res2 = Result.equal (=) (=) res1 res2";
  [%expect {| O(1) |}]

let%expect_test "Result.compare is O(1) for O(1) comparison" =
  check_and_print "let f res1 res2 = Result.compare compare compare res1 res2";
  [%expect {| O(1) |}]

(* Real-world Result usage patterns *)

let%expect_test "chaining Result.bind is O(1) for O(1) functions" =
  check_and_print {|
    let f res =
      Result.bind res (fun x ->
        Result.bind (Ok (x + 1)) (fun y ->
          Ok (y * 2)))
  |};
  [%expect {| O(1) |}]

let%expect_test "Result.map over list iteration" =
  check_and_print {|
    let f lst =
      List.map (fun x -> Result.map (fun y -> y + 1) x) lst
  |};
  [%expect {| O(n) |}]

let%expect_test "converting list of Results to Result of list" =
  check_and_print {|
    let f lst =
      List.fold_left (fun acc res ->
        Result.bind acc (fun acc_list ->
          Result.map (fun x -> x :: acc_list) res)
      ) (Ok []) lst
  |};
  [%expect {| O(n) |}]

let%expect_test "Result with expensive error handling" =
  check_and_print {|
    let f res =
      let expensive_handler e = List.fold_left (+) 0 [1;2;3;4;5] in
      Result.map_error expensive_handler res
  |};
  [%expect {| O(n) |}]

let%expect_test "nested Result operations" =
  check_and_print {|
    let f res =
      Result.map (fun x ->
        Result.fold ~ok:(fun y -> x + y) ~error:(fun _ -> x) (Ok 10)
      ) res
  |};
  [%expect {| O(1) |}]

(* Railway-oriented programming patterns *)

let%expect_test "multiple Result.bind chains" =
  check_and_print {|
    let f s =
      let parse_int s = if s = "" then Error "empty" else Ok 42 in
      let validate n = if n > 0 then Ok n else Error "negative" in
      let double n = Ok (n * 2) in
      parse_int s
      |> Result.bind validate
      |> Result.bind double
  |};
  [%expect {| O(1) |}]

let%expect_test "Result with pattern matching fallback" =
  check_and_print {|
    let f res =
      match res with
      | Ok x -> x + 1
      | Error _ -> 0
  |};
  [%expect {| O(1) |}]

let%expect_test "collecting Results from list" =
  check_and_print {|
    let f results =
      List.fold_left (fun acc res ->
        match acc, res with
        | Ok vals, Ok v -> Ok (v :: vals)
        | Error e, _ | _, Error e -> Error e
      ) (Ok []) results
  |};
  [%expect {| O(n) |}]

(* Edge cases *)

let%expect_test "Result.map with List operation" =
  check_and_print {|
    let f res =
      Result.map (fun lst -> List.length lst) res
  |};
  [%expect {| O(n) |}]

let%expect_test "multiple Result checks" =
  check_and_print {|
    let f res1 res2 res3 =
      Result.is_ok res1 && Result.is_ok res2 && Result.is_ok res3
  |};
  [%expect {| O(1) |}]

let%expect_test "Result with both map and map_error" =
  check_and_print {|
    let f res =
      res
      |> Result.map (fun x -> x + 1)
      |> Result.map_error (fun e -> e + 1)
  |};
  [%expect {| O(1) |}]
