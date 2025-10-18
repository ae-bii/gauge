let%expect_test "simple nested lambda O(n^2)" =
  let code = {|
(* @complexity simple_nested: O(n^2) *)
let simple_nested lst = 
  List.map (fun x -> List.map (fun y -> x + y) lst) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    simple_nested: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "triple nested lambda O(n^3)" =
  let code = {|
(* @complexity triple_nested: O(n^3) *)
let triple_nested lst = 
  List.map (fun x -> 
    List.map (fun y -> 
      List.map (fun z -> x + y + z) lst
    ) lst
  ) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    triple_nested: inferred=O(n^3) declared=O(n^3) [OK]
  |}]

let%expect_test "nested with List.length O(n^3)" =
  let code = {|
(* @complexity nested_with_length: O(n^3) *)
let nested_with_length lst1 lst2 = 
  List.map (fun x -> 
    List.map (fun y -> 
      List.length lst1 + x + y
    ) lst2
  ) lst1
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    nested_with_length: inferred=O(n^3) declared=O(n^3) [OK]
  |}]

let%expect_test "deeply nested with capture O(n^4)" =
  let code = {|
(* @complexity deep_capture: O(n^4) *)
let deep_capture lst =
  List.map (fun a ->
    List.map (fun b ->
      List.map (fun c ->
        List.length lst + a + b + c
      ) lst
    ) lst
  ) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    deep_capture: inferred=O(n^4) declared=O(n^4) [OK]
  |}]
