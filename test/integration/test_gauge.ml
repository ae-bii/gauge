let%expect_test "infer example" =
	let file = "examples/example.ml" in
	let code = {|
		(* @complexity O(n) *)
		let rec sum n =
			if n <= 0 then 0 else n + sum (n-1)

		let () = Printf.printf "%d\n" (sum 5)
	|} in
	let inferred = Gauge.Frontend.infer_from_source ~filename:file code in
	Printf.printf "%s\n" (Gauge.Cost_model.to_string inferred);
	[%expect {| O(n) |}]
