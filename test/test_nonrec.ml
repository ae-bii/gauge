let%expect_test "non-rec helper" =
  let code = "let f n = let helper x = x + 1 in helper n" in
  let inferred = Gauge.Infer.infer_of_string_code code in
  Printf.printf "%s\n" (Gauge.Cost_model.to_string inferred);
  [%expect {| O(1) |}]
