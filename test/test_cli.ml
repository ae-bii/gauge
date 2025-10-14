let%expect_test "cli reports example" =
  let file = "examples/example.ml" in
  let code = "(* @complexity O(n) *)\nlet rec sum n = if n <= 0 then 0 else n + sum (n-1)\n" in
  let declared = Gauge.Contracts.extract_complexity_annotation code in
  let inferred = Gauge.Infer.infer_of_string_code code in
  Gauge.Report.report file inferred declared;
  [%expect {| examples/example.ml: inferred=O(n) declared=O(n) [OK] |}]
