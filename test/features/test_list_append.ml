let%expect_test "simple list append O(n)" =
  let code = {|
(* @complexity simple_append: O(n) *)
let simple_append lst1 lst2 = lst1 @ lst2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    simple_append: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "append in recursive function O(n^2)" =
  let code = {|
(* @complexity rev: O(n^2) *)
let rec rev lst =
  match lst with
  | [] -> []
  | x :: xs -> (rev xs) @ [x]
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    rev: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "multiple appends in sequence O(n)" =
  let code = {|
(* @complexity multi_append: O(n) *)
let multi_append a b c = a @ b @ c
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    multi_append: inferred=O(n) declared=O(n) [OK]
  |}]
