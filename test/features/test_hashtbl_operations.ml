let%expect_test "Hashtbl.create O(1)" =
  let code = {|
(* @complexity hashtbl_create: O(1) *)
let hashtbl_create n = Hashtbl.create n
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_create: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.add O(1) - average case" =
  let code = {|
(* @complexity hashtbl_add: O(1) *)
let hashtbl_add tbl key value = Hashtbl.add tbl key value
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_add: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.find O(1) - average case" =
  let code = {|
(* @complexity hashtbl_find: O(1) *)
let hashtbl_find tbl key = Hashtbl.find tbl key
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_find: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.find_opt O(1)" =
  let code = {|
(* @complexity hashtbl_find_opt: O(1) *)
let hashtbl_find_opt tbl key = Hashtbl.find_opt tbl key
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_find_opt: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.mem O(1)" =
  let code = {|
(* @complexity hashtbl_mem: O(1) *)
let hashtbl_mem tbl key = Hashtbl.mem tbl key
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_mem: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.remove O(1)" =
  let code = {|
(* @complexity hashtbl_remove: O(1) *)
let hashtbl_remove tbl key = Hashtbl.remove tbl key
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_remove: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.replace O(1)" =
  let code = {|
(* @complexity hashtbl_replace: O(1) *)
let hashtbl_replace tbl key value = Hashtbl.replace tbl key value
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_replace: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.length O(1)" =
  let code = {|
(* @complexity hashtbl_length: O(1) *)
let hashtbl_length tbl = Hashtbl.length tbl
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_length: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Hashtbl.iter O(n) - iterates all entries" =
  let code = {|
(* @complexity hashtbl_iter: O(n) *)
let hashtbl_iter tbl = Hashtbl.iter (fun k v -> ()) tbl
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_iter: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Hashtbl.fold O(n)" =
  let code = {|
(* @complexity hashtbl_fold: O(n) *)
let hashtbl_fold tbl = Hashtbl.fold (fun k v acc -> acc + v) tbl 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_fold: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Hashtbl.to_seq O(n)" =
  let code = {|
(* @complexity hashtbl_to_seq: O(n) *)
let hashtbl_to_seq tbl = Hashtbl.to_seq tbl
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    hashtbl_to_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Hashtbl.fold with expensive function O(n^2)" =
  let code = {|
(* @complexity expensive_fold: O(n^2) *)
let expensive_fold tbl = 
  Hashtbl.fold (fun k v acc -> acc + List.length v) tbl 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    expensive_fold: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "Building hashtbl from list O(n)" =
  let code = {|
(* @complexity build_hashtbl: O(n) *)
let build_hashtbl pairs =
  let tbl = Hashtbl.create 16 in
  let rec add_all = function
    | [] -> tbl
    | (k, v) :: rest -> 
        Hashtbl.add tbl k v;
        add_all rest
  in
  add_all pairs
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    build_hashtbl: inferred=O(n) declared=O(n) [OK]
  |}]
