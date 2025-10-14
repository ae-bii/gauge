let%expect_test "shadowed helper name" =
  let code = "let f n = let go x = 1 in let go n = let rec go k = if k <= 0 then 0 else go (k-1) in go n in f 5" in
  let inferred = Gauge.Infer.infer_of_string_code code in
  Printf.printf "%s\n" (Gauge.Cost_model.to_string inferred);
  [%expect {| O(n) |}]
