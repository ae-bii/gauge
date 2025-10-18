let%expect_test "CLI exits with code 1 on mismatch" =
  let code = {|
(* @complexity wrong: O(1) *)
let rec wrong n = if n <= 0 then 0 else 1 + wrong (n-1)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let has_mismatch = Gauge.Report.report_many inferred declared in
  Printf.printf "Exit code would be: %d\n" (if has_mismatch then 1 else 0);
  [%expect {|
    wrong: inferred=O(n) declared=O(1) [MISMATCH]
    Exit code would be: 1
  |}]

let%expect_test "CLI exits with code 0 when all OK" =
  let code = {|
(* @complexity correct: O(n) *)
let rec correct n = if n <= 0 then 0 else 1 + correct (n-1)

let no_annotation x = x + 1
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let has_mismatch = Gauge.Report.report_many inferred declared in
  Printf.printf "Exit code would be: %d\n" (if has_mismatch then 1 else 0);
  [%expect {|
    correct: inferred=O(n) declared=O(n) [OK]
    no_annotation: inferred=O(1) declared=(none)
    Exit code would be: 0
  |}]
