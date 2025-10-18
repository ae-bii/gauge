let%expect_test "parse O(n^3)" =
  let result = Gauge.Cost_model.of_string "O(n^3)" in
  (match result with
  | Some c -> print_endline (Gauge.Cost_model.to_string c)
  | None -> print_endline "Failed to parse");
  [%expect {| O(n^3) |}]

let%expect_test "parse O(n^5)" =
  let result = Gauge.Cost_model.of_string "O(n^5)" in
  (match result with
  | Some c -> print_endline (Gauge.Cost_model.to_string c)
  | None -> print_endline "Failed to parse");
  [%expect {| O(n^5) |}]

let%expect_test "parse n^4 without wrapper" =
  let result = Gauge.Cost_model.of_string "n^4" in
  (match result with
  | Some c -> print_endline (Gauge.Cost_model.to_string c)
  | None -> print_endline "Failed to parse");
  [%expect {| O(n^4) |}]

let%expect_test "parse standard cases still work" =
  let test_cases = ["O(1)"; "O(n)"; "O(n^2)"; "O(log n)"] in
  List.iter (fun s ->
    match Gauge.Cost_model.of_string s with
    | Some c -> Printf.printf "%s -> %s\n" s (Gauge.Cost_model.to_string c)
    | None -> Printf.printf "%s -> Failed\n" s
  ) test_cases;
  [%expect {|
    O(1) -> O(1)
    O(n) -> O(n)
    O(n^2) -> O(n^2)
    O(log n) -> O(n^0 log^1 n)
  |}]

let%expect_test "invalid formats fail gracefully" =
  let test_cases = ["O(n^)"; "O(n^abc)"; "invalid"; "O(n^-1)"] in
  List.iter (fun s ->
    match Gauge.Cost_model.of_string s with
    | Some c -> Printf.printf "%s -> %s\n" s (Gauge.Cost_model.to_string c)
    | None -> Printf.printf "%s -> Failed\n" s
  ) test_cases;
  [%expect {|
    O(n^) -> Failed
    O(n^abc) -> Failed
    invalid -> Failed
    O(n^-1) -> Failed
  |}]
