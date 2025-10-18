let%expect_test "Map.empty O(1)" =
  let code = {|
(* @complexity map_empty: O(1) *)
let map_empty () = Map.empty
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_empty: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Map.is_empty O(1)" =
  let code = {|
(* @complexity map_is_empty: O(1) *)
let map_is_empty m = Map.is_empty m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_is_empty: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Map.add O(log n) - balanced tree insertion" =
  let code = {|
(* @complexity map_add: O(log n) *)
let map_add m key value = Map.add key value m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_add: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Map.find O(log n) - tree traversal" =
  let code = {|
(* @complexity map_find: O(log n) *)
let map_find m key = Map.find key m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_find: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Map.find_opt O(log n)" =
  let code = {|
(* @complexity map_find_opt: O(log n) *)
let map_find_opt m key = Map.find_opt key m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_find_opt: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Map.mem O(log n)" =
  let code = {|
(* @complexity map_mem: O(log n) *)
let map_mem m key = Map.mem key m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_mem: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Map.remove O(log n)" =
  let code = {|
(* @complexity map_remove: O(log n) *)
let map_remove m key = Map.remove key m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_remove: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Map.singleton O(log n)" =
  let code = {|
(* @complexity map_singleton: O(log n) *)
let map_singleton k v = Map.singleton k v
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_singleton: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Map.iter O(n) - visit all entries" =
  let code = {|
(* @complexity map_iter: O(n) *)
let map_iter m = Map.iter (fun k v -> ()) m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_iter: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Map.fold O(n)" =
  let code = {|
(* @complexity map_fold: O(n) *)
let map_fold m = Map.fold (fun k v acc -> acc + v) m 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_fold: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Map.map O(n)" =
  let code = {|
(* @complexity map_map: O(n) *)
let map_map m = Map.map (fun v -> v * 2) m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_map: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Map.filter O(n)" =
  let code = {|
(* @complexity map_filter: O(n) *)
let map_filter m = Map.filter (fun k v -> v > 0) m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_filter: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Map.bindings O(n) - convert to list" =
  let code = {|
(* @complexity map_bindings: O(n) *)
let map_bindings m = Map.bindings m
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_bindings: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Map.fold with expensive function O(n^2)" =
  let code = {|
(* @complexity expensive_fold: O(n^2) *)
let expensive_fold m =
  Map.fold (fun k v acc -> acc + List.length v) m 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    expensive_fold: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "Build map from list O(n log n)" =
  let code = {|
(* @complexity build_map: O(n log n) *)
let rec build_map pairs =
  match pairs with
  | [] -> Map.empty
  | (k, v) :: rest -> Map.add k v (build_map rest)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    build_map: inferred=O(n log n) declared=O(n log n) [OK]
  |}]
