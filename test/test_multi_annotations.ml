let%expect_test "multiple annotations in one file" =
  let code = {|
(* @complexity foo: O(1) *)
let foo x = x + 1

(* @complexity bar: O(n) *)
let rec bar n = if n <= 0 then 0 else 1 + bar (n-1)

(* @complexity baz: O(n^2) *)
let baz lst = List.map (fun x -> List.length lst + x) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  Gauge.Report.report_many inferred declared;
  [%expect {|
    foo: inferred=O(1) declared=O(1) [OK]
    bar: inferred=O(n) declared=O(n) [OK]
    baz: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "mixed annotations with unannotated function" =
  let code = {|
(* @complexity alpha: O(n) *)
let rec alpha n = if n <= 0 then 0 else 1 + alpha (n-1)

let beta x = x * 2

(* @complexity gamma: O(n^2) *)
let gamma lst = List.map (fun y -> List.fold_left (+) 0 lst) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  Gauge.Report.report_many inferred declared;
  [%expect {|
    alpha: inferred=O(n) declared=O(n) [OK]
    beta: inferred=O(1) declared=(none)
    gamma: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "annotation for non-existent function" =
  let code = {|
(* @complexity real_func: O(1) *)
let real_func x = x + 1

(* @complexity missing_func: O(n) *)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  Gauge.Report.report_many inferred declared;
  [%expect {|
    real_func: inferred=O(1) declared=O(1) [OK]
    missing_func: inferred=(none) declared=O(n) [DECLARED-BUT-NOT-FOUND]
  |}]

let%expect_test "multiple functions with mismatches" =
  let code = {|
(* @complexity slow: O(1) *)
let rec slow n = if n <= 0 then 0 else 1 + slow (n-1)

(* @complexity fast: O(n^2) *)
let fast x = x + 1
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  Gauge.Report.report_many inferred declared;
  [%expect {|
    slow: inferred=O(n) declared=O(1) [MISMATCH]
    fast: inferred=O(1) declared=O(n^2) [MISMATCH]
  |}]
