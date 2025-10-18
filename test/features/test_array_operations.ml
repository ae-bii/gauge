let%expect_test "Array.length O(1)" =
  let code = {|
(* @complexity array_length: O(1) *)
let array_length arr = Array.length arr
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_length: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Array.get O(1) - key advantage!" =
  let code = {|
(* @complexity array_get: O(1) *)
let array_get arr i = Array.get arr i
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_get: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Array.set O(1)" =
  let code = {|
(* @complexity array_set: O(1) *)
let array_set arr i v = Array.set arr i v
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_set: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Array.make O(n)" =
  let code = {|
(* @complexity array_make: O(n) *)
let array_make n v = Array.make n v
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_make: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Array.init O(n)" =
  let code = {|
(* @complexity array_init: O(n) *)
let array_init n = Array.init n (fun i -> i * 2)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_init: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Array.map O(n)" =
  let code = {|
(* @complexity array_map: O(n) *)
let array_map arr = Array.map (fun x -> x + 1) arr
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_map: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Array.fold_left O(n)" =
  let code = {|
(* @complexity array_fold: O(n) *)
let array_fold arr = Array.fold_left (+) 0 arr
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_fold: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Array.to_list O(n)" =
  let code = {|
(* @complexity array_to_list: O(n) *)
let array_to_list arr = Array.to_list arr
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_to_list: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Array.of_list O(n)" =
  let code = {|
(* @complexity array_of_list: O(n) *)
let array_of_list lst = Array.of_list lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    array_of_list: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Array.map with expensive function O(n^2)" =
  let code = {|
(* @complexity expensive_map: O(n^2) *)
let expensive_map arr = Array.map (fun lst -> List.length lst) arr
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    expensive_map: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "Contrast: List.nth is O(n)" =
  let code = {|
(* @complexity list_nth: O(n) *)
let list_nth lst i = List.nth lst i
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    list_nth: inferred=O(n) declared=O(n) [OK]
  |}]
