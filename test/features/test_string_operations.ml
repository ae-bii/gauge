let%expect_test "String.length O(1)" =
  let code = {|
(* @complexity string_length: O(1) *)
let string_length s = String.length s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    string_length: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "String concatenation operator ^ O(n)" =
  let code = {|
(* @complexity string_concat: O(n) *)
let string_concat s1 s2 = s1 ^ s2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    string_concat: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "String.concat O(n)" =
  let code = {|
(* @complexity join: O(n) *)
let join sep lst = String.concat sep lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    join: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "String.sub O(n)" =
  let code = {|
(* @complexity substring: O(n) *)
let substring s start len = String.sub s start len
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    substring: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "String.map O(n)" =
  let code = {|
(* @complexity uppercase: O(n) *)
let uppercase s = String.map Char.uppercase_ascii s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    uppercase: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "String.equal O(n)" =
  let code = {|
(* @complexity compare: O(n) *)
let compare s1 s2 = String.equal s1 s2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    compare: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Repeated concatenation O(n^2) - the classic bug!" =
  let code = {|
(* @complexity bad_concat: O(n^2) *)
let rec bad_concat = function
  | [] -> ""
  | x :: xs -> x ^ bad_concat xs
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    bad_concat: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "String.concat is the right way - O(n)" =
  let code = {|
(* @complexity good_concat: O(n) *)
let good_concat lst = String.concat "" lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    good_concat: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Building string in loop - O(n^2)" =
  let code = {|
(* @complexity build_string: O(n^2) *)
let build_string n =
  let rec loop i acc =
    if i <= 0 then acc
    else loop (i-1) (acc ^ "x")
  in loop n ""
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    build_string: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "String.make is efficient - O(n)" =
  let code = {|
(* @complexity make_string: O(n) *)
let make_string n = String.make n 'x'
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    make_string: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "String operations in List.map - O(n^2)" =
  let code = {|
(* @complexity map_uppercase: O(n^2) *)
let map_uppercase lst = List.map (fun s -> String.uppercase_ascii s) lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_uppercase: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "Bytes.length O(1)" =
  let code = {|
(* @complexity bytes_length: O(1) *)
let bytes_length b = Bytes.length b
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    bytes_length: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Bytes.concat O(n)" =
  let code = {|
(* @complexity bytes_concat: O(n) *)
let bytes_concat sep lst = Bytes.concat sep lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    bytes_concat: inferred=O(n) declared=O(n) [OK]
  |}]
