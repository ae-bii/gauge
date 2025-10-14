let%expect_test "mutual recursion top-level" =
  let code = "let rec even n = if n = 0 then true else odd (n-1) and odd n = if n = 0 then false else even (n-1) in even 3" in
  let inferred = Gauge.Infer.infer_of_string_code code in
  Printf.printf "%s\n" (Gauge.Cost_model.to_string inferred);
  [%expect {| O(n) |}]
