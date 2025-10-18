let%expect_test "List.length O(n)" =
  let code = {|
(* @complexity test_length: O(n) *)
let test_length lst = List.length lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_length: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "List.rev O(n)" =
  let code = {|
(* @complexity test_rev: O(n) *)
let test_rev lst = List.rev lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_rev: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "List.hd O(1)" =
  let code = {|
(* @complexity test_hd: O(1) *)
let test_hd lst = List.hd lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_hd: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "List.map with simple function O(n)" =
  let code = {|
(* @complexity test_map: O(n) *)
let test_map lst = List.map (fun x -> x + 1) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_map: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "List.map with expensive function O(n^2)" =
  let code = {|
(* @complexity expensive_map: O(n^2) *)
let expensive_map lst = List.map (fun x -> List.length x) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    expensive_map: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "List.filter O(n)" =
  let code = {|
(* @complexity test_filter: O(n) *)
let test_filter lst = List.filter (fun x -> x > 0) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_filter: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "List.fold_left O(n)" =
  let code = {|
(* @complexity test_fold: O(n) *)
let test_fold lst = List.fold_left (+) 0 lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_fold: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "List.concat O(n^2)" =
  let code = {|
(* @complexity test_concat: O(n^2) *)
let test_concat lsts = List.concat lsts
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_concat: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "List.flatten O(n)" =
  let code = {|
(* @complexity test_flatten: O(n) *)
let test_flatten lsts = List.flatten lsts
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_flatten: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "List.exists O(n)" =
  let code = {|
(* @complexity test_exists: O(n) *)
let test_exists lst = List.exists (fun x -> x > 10) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    test_exists: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "List.fold with expensive operation O(n^2)" =
  let code = {|
(* @complexity fold_expensive: O(n^2) *)
let fold_expensive lst =
  List.fold_left (fun acc x -> List.rev x @ acc) [] lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    fold_expensive: inferred=O(n^2) declared=O(n^2) [OK]
  |}]
