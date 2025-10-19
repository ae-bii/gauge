(* Tests for improved recursion and flow analysis *)

let%expect_test "for loop with O(1) operations O(n)" =
  let code = {|
(* @complexity loop_sum: O(n) *)
let loop_sum n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  !total
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    loop_sum: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "nested for loops O(n^2)" =
  let code = {|
(* @complexity nested_loops: O(n^2) *)
let nested_loops n =
  let count = ref 0 in
  for i = 1 to n do
    for j = 1 to n do
      incr count
    done
  done;
  !count
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    nested_loops: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "let binding with List.length tracked O(n)" =
  let code = {|
(* @complexity get_length: O(n) *)
let get_length lst =
  let len = List.length lst in
  len
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    get_length: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "let binding with Array.length O(1)" =
  let code = {|
(* @complexity get_array_length: O(1) *)
let get_array_length arr =
  let len = Array.length arr in
  len
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    get_array_length: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "let binding used in loop O(n)" =
  let code = {|
(* @complexity use_in_loop: O(n) *)
let use_in_loop lst =
  let len = List.length lst in
  let result = ref 0 in
  for i = 1 to len do
    result := !result + i
  done;
  !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    use_in_loop: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "chained let bindings O(n)" =
  let code = {|
(* @complexity chained: O(n) *)
let chained lst =
  let len = List.length lst in
  let doubled = len * 2 in
  doubled
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    chained: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "multiple operations tracked O(n)" =
  let code = {|
(* @complexity multi_ops: O(n) *)
let multi_ops lst1 lst2 =
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  len1 + len2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    multi_ops: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "let with expensive operation in loop O(n^2)" =
  let code = {|
(* @complexity expensive_loop: O(n^2) *)
let expensive_loop lists =
  let result = ref 0 in
  for i = 0 to Array.length lists - 1 do
    let lst = Array.get lists i in
    let len = List.length lst in
    result := !result + len
  done;
  !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    expensive_loop: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "while loop with counter O(n)" =
  let code = {|
(* @complexity while_loop: O(n) *)
let while_loop n =
  let i = ref 0 in
  let sum = ref 0 in
  while !i < n do
    sum := !sum + !i;
    incr i
  done;
  !sum
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    while_loop: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "let binding with map O(n)" =
  let code = {|
(* @complexity map_in_let: O(n) *)
let map_in_let lst =
  let doubled = List.map (fun x -> x * 2) lst in
  doubled
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_in_let: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "sequence of O(n) operations O(n)" =
  let code = {|
(* @complexity sequence: O(n) *)
let sequence lst =
  let rev = List.rev lst in
  let len = List.length rev in
  len
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    sequence: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "ref in nested context O(n)" =
  let code = {|
(* @complexity nested_ref: O(n) *)
let nested_ref arr =
  let compute () =
    let total = ref 0 in
    for i = 0 to Array.length arr - 1 do
      total := !total + arr.(i)
    done;
    !total
  in
  compute ()
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    nested_ref: inferred=O(n) declared=O(n) [OK]
  |}]
