let%expect_test "fibonacci with two recursive calls O(n^2)" =
  let code = {|
(* @complexity fib: O(n^2) *)
let rec fib n =
  if n <= 1 then 1
  else fib (n-1) + fib (n-2)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    fib: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "single recursive call remains O(n)" =
  let code = {|
(* @complexity countdown: O(n) *)
let rec countdown n =
  if n <= 0 then 0
  else 1 + countdown (n-1)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    countdown: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "ackermann with multiple recursive calls O(n^2)" =
  let code = {|
(* @complexity ackermann: O(n^2) *)
let rec ackermann m n =
  if m = 0 then n + 1
  else if n = 0 then ackermann (m-1) 1
  else ackermann (m-1) (ackermann m (n-1))
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    ackermann: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "tree traversal with two branches O(n)" =
  let code = {|
type tree = Leaf of int | Node of tree * tree

(* @complexity sum_tree: O(n) *)
let rec sum_tree t =
  match t with
  | Leaf v -> v
  | Node (left, right) -> sum_tree left + sum_tree right
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    sum_tree: inferred=O(n) declared=O(n) [OK]
  |}]

(* Note: Complex patterns with List.filter and multiple let bindings
   before recursive calls may not be fully detected yet. *)

