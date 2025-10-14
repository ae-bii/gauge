let%expect_test "local let rec helper" =
  let code = "let f n = let rec go k = if k <= 0 then 0 else go (k-1) in go n" in
  let inferred = Gauge.Infer.infer_of_string_code code in
  Printf.printf "%s\n" (Gauge.Cost_model.to_string inferred);
  [%expect {| O(n) |}]
