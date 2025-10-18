let%expect_test "tail-recursive helper" =
  let code = "let f n = let rec go k acc = if k <= 0 then acc else go (k-1) (acc+1) in go n 0" in
  let inferred = Gauge.Infer.infer_of_string_code code in
  Printf.printf "%s\n" (Gauge.Cost_model.to_string inferred);
  [%expect {| O(n) |}]
