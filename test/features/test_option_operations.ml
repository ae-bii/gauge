(* Test suite for Option module complexity analysis *)

(* Helper to check complexity *)
let check_and_print code =
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  match inferred with
  | [(_, cost)] -> print_endline (Gauge.Cost_model.to_string cost)
  | _ -> print_endline "Error: expected single function"

(* Basic Option operations - O(1) *)

let%expect_test "Option.none is O(1)" =
  check_and_print "let f () = Option.none";
  [%expect {| O(1) |}]

let%expect_test "Option.some is O(1)" =
  check_and_print "let f x = Option.some x";
  [%expect {| O(1) |}]

let%expect_test "Option.is_none is O(1)" =
  check_and_print "let f opt = Option.is_none opt";
  [%expect {| O(1) |}]

let%expect_test "Option.is_some is O(1)" =
  check_and_print "let f opt = Option.is_some opt";
  [%expect {| O(1) |}]

let%expect_test "Option.value with default is O(1)" =
  check_and_print "let f opt default = Option.value opt ~default";
  [%expect {| O(1) |}]

let%expect_test "Option.get is O(1)" =
  check_and_print "let f opt = Option.get opt";
  [%expect {| O(1) |}]

(* Option transformations - O(1) wrapper *)

let%expect_test "Option.map is O(1) for O(1) function" =
  check_and_print "let f opt = Option.map (fun x -> x + 1) opt";
  [%expect {| O(1) |}]

let%expect_test "Option.bind is O(1) for O(1) function" =
  check_and_print "let f opt = Option.bind opt (fun x -> Some (x + 1))";
  [%expect {| O(1) |}]

let%expect_test "Option.iter is O(1) for O(1) function" =
  check_and_print "let f opt = Option.iter (fun x -> ignore x) opt";
  [%expect {| O(1) |}]

let%expect_test "Option.fold is O(1) for O(1) function" =
  check_and_print "let f opt = Option.fold ~none:0 ~some:(fun x -> x + 1) opt";
  [%expect {| O(1) |}]

let%expect_test "Option.join flattens nested option in O(1)" =
  check_and_print "let f opt = Option.join opt";
  [%expect {| O(1) |}]

(* Option conversions - O(1) *)

let%expect_test "Option.to_list is O(1)" =
  check_and_print "let f opt = Option.to_list opt";
  [%expect {| O(1) |}]

let%expect_test "Option.to_seq is O(1)" =
  check_and_print "let f opt = Option.to_seq opt";
  [%expect {| O(1) |}]

let%expect_test "Option.to_result with error is O(1)" =
  check_and_print {|let f opt = Option.to_result ~none:"error" opt|};
  [%expect {| O(1) |}]

(* Option comparison - O(1) *)

let%expect_test "Option.equal is O(1) for O(1) equality" =
  check_and_print "let f opt1 opt2 = Option.equal (=) opt1 opt2";
  [%expect {| O(1) |}]

let%expect_test "Option.compare is O(1) for O(1) comparison" =
  check_and_print "let f opt1 opt2 = Option.compare compare opt1 opt2";
  [%expect {| O(1) |}]

(* Real-world Option usage patterns *)

let%expect_test "chaining Option.bind is O(1) for O(1) functions" =
  check_and_print {|
    let f opt =
      Option.bind opt (fun x ->
        Option.bind (Some (x + 1)) (fun y ->
          Some (y * 2)))
  |};
  [%expect {| O(1) |}]

let%expect_test "Option.map over list iteration" =
  check_and_print {|
    let f lst =
      List.map (fun x -> Option.map (fun y -> y + 1) x) lst
  |};
  [%expect {| O(n) |}]

let%expect_test "filtering Options from list" =
  check_and_print {|
    let f lst =
      List.filter_map (fun x -> 
        if x > 0 then Some x else None
      ) lst
  |};
  [%expect {| O(n) |}]

let%expect_test "Option.value with expensive default" =
  check_and_print {|
    let f opt =
      let expensive_default () = List.fold_left (+) 0 [1;2;3;4;5] in
      Option.value opt ~default:(expensive_default ())
  |};
  [%expect {| O(n) |}]

let%expect_test "nested Option operations" =
  check_and_print {|
    let f opt =
      Option.map (fun x ->
        Option.fold ~none:0 ~some:(fun y -> x + y) (Some 10)
      ) opt
  |};
  [%expect {| O(1) |}]

(* Edge cases *)

let%expect_test "Option in pattern matching" =
  check_and_print {|
    let f opt =
      match opt with
      | None -> 0
      | Some x -> x + 1
  |};
  [%expect {| O(1) |}]

let%expect_test "Option with List operations" =
  check_and_print {|
    let f opt =
      Option.map (fun lst -> List.length lst) opt
  |};
  [%expect {| O(n) |}]

let%expect_test "multiple Option checks" =
  check_and_print {|
    let f opt1 opt2 opt3 =
      Option.is_some opt1 && Option.is_some opt2 && Option.is_some opt3
  |};
  [%expect {| O(1) |}]
