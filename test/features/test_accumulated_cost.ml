(* Tests for accumulated cost detection in loops *)

(* ============ Simple Cases (Should Work) ============ *)

let%expect_test "append in loop naive O(n)" =
  let code = {|
(* @complexity append_naive: O(n) *)
let append_naive lists =
  let result = ref [] in
  for i = 0 to Array.length lists - 1 do
    result := !result @ lists.(i)
  done;
  !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    append_naive: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "prepend in loop O(n)" =
  let code = {|
(* @complexity prepend_good: O(n) *)
let prepend_good lists =
  let result = ref [] in
  for i = 0 to Array.length lists - 1 do
    result := lists.(i) :: !result
  done;
  List.rev !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    prepend_good: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "sum in loop O(n)" =
  let code = {|
(* @complexity sum_loop: O(n) *)
let sum_loop arr =
  let total = ref 0 in
  for i = 0 to Array.length arr - 1 do
    total := !total + arr.(i)
  done;
  !total
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    sum_loop: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "concat strings O(n)" =
  let code = {|
(* @complexity concat_strings: O(n) *)
let concat_strings strs =
  let result = ref "" in
  for i = 0 to Array.length strs - 1 do
    result := !result ^ strs.(i)
  done;
  !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    concat_strings: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Non-Loop Cases ============ *)

let%expect_test "single append O(n)" =
  let code = {|
(* @complexity single_append: O(n) *)
let single_append lst1 lst2 =
  lst1 @ lst2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    single_append: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "list concat O(n)" =
  let code = {|
(* @complexity list_concat: O(n) *)
let list_concat lists =
  List.concat lists
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    list_concat: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "efficient concatenation O(n)" =
  let code = {|
(* @complexity efficient_concat: O(n) *)
let efficient_concat lists =
  List.concat (Array.to_list lists)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    efficient_concat: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Documentation Examples ============ *)

let%expect_test "documented workaround O(n)" =
  let code = {|
(* @complexity good_append: O(n) *)
let good_append lists =
  let result = ref [] in
  Array.iter (fun lst ->
    List.iter (fun x -> result := x :: !result) lst
  ) lists;
  List.rev !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    good_append: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "buffer pattern O(n)" =
  let code = {|
(* @complexity buffer_concat: O(n) *)
let buffer_concat strs =
  let buf = Buffer.create 16 in
  Array.iter (Buffer.add_string buf) strs;
  Buffer.contents buf
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    buffer_concat: inferred=O(n) declared=O(n) [OK]
  |}]
