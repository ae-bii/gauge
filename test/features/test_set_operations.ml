let%expect_test "Set.empty O(1)" =
  let code = {|
(* @complexity set_empty: O(1) *)
let set_empty () = Set.empty
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_empty: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Set.is_empty O(1)" =
  let code = {|
(* @complexity set_is_empty: O(1) *)
let set_is_empty s = Set.is_empty s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_is_empty: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Set.cardinal O(1)" =
  let code = {|
(* @complexity set_cardinal: O(1) *)
let set_cardinal s = Set.cardinal s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_cardinal: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Set.add O(log n) - balanced tree insertion" =
  let code = {|
(* @complexity set_add: O(log n) *)
let set_add s elem = Set.add elem s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_add: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.mem O(log n) - membership test" =
  let code = {|
(* @complexity set_mem: O(log n) *)
let set_mem s elem = Set.mem elem s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_mem: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.remove O(log n)" =
  let code = {|
(* @complexity set_remove: O(log n) *)
let set_remove s elem = Set.remove elem s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_remove: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.singleton O(log n)" =
  let code = {|
(* @complexity set_singleton: O(log n) *)
let set_singleton x = Set.singleton x
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_singleton: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.min_elt O(log n)" =
  let code = {|
(* @complexity set_min: O(log n) *)
let set_min s = Set.min_elt s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_min: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.max_elt O(log n)" =
  let code = {|
(* @complexity set_max: O(log n) *)
let set_max s = Set.max_elt s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_max: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.choose O(log n)" =
  let code = {|
(* @complexity set_choose: O(log n) *)
let set_choose s = Set.choose s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_choose: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.split O(log n)" =
  let code = {|
(* @complexity set_split: O(log n) *)
let set_split s x = Set.split x s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_split: inferred=O(log n) declared=O(log n) [OK]
  |}]

let%expect_test "Set.iter O(n) - visit all elements" =
  let code = {|
(* @complexity set_iter: O(n) *)
let set_iter s = Set.iter (fun x -> ()) s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_iter: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.fold O(n)" =
  let code = {|
(* @complexity set_fold: O(n) *)
let set_fold s = Set.fold (fun x acc -> acc + x) s 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_fold: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.map O(n)" =
  let code = {|
(* @complexity set_map: O(n) *)
let set_map s = Set.map (fun x -> x * 2) s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_map: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.filter O(n)" =
  let code = {|
(* @complexity set_filter: O(n) *)
let set_filter s = Set.filter (fun x -> x > 0) s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_filter: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.partition O(n)" =
  let code = {|
(* @complexity set_partition: O(n) *)
let set_partition s = Set.partition (fun x -> x mod 2 = 0) s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_partition: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.elements O(n) - convert to list" =
  let code = {|
(* @complexity set_elements: O(n) *)
let set_elements s = Set.elements s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_elements: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.of_list O(n log n) - build from list" =
  let code = {|
(* @complexity set_of_list: O(n) *)
let set_of_list lst = Set.of_list lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_of_list: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.for_all O(n)" =
  let code = {|
(* @complexity set_for_all: O(n) *)
let set_for_all s = Set.for_all (fun x -> x > 0) s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_for_all: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.exists O(n)" =
  let code = {|
(* @complexity set_exists: O(n) *)
let set_exists s = Set.exists (fun x -> x = 42) s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_exists: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Set.union O(n^2) - combine two sets" =
  let code = {|
(* @complexity set_union: O(n^2) *)
let set_union s1 s2 = Set.union s1 s2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_union: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "Set.inter O(n^2) - intersection" =
  let code = {|
(* @complexity set_inter: O(n^2) *)
let set_inter s1 s2 = Set.inter s1 s2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_inter: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "Set.diff O(n^2) - set difference" =
  let code = {|
(* @complexity set_diff: O(n^2) *)
let set_diff s1 s2 = Set.diff s1 s2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    set_diff: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "Build set from list recursively O(n log n)" =
  let code = {|
(* @complexity build_set: O(n log n) *)
let rec build_set lst =
  match lst with
  | [] -> Set.empty
  | x :: rest -> Set.add x (build_set rest)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    build_set: inferred=O(n log n) declared=O(n log n) [OK]
  |}]

let%expect_test "Set.fold with expensive function O(n^2)" =
  let code = {|
(* @complexity expensive_fold: O(n^2) *)
let expensive_fold s =
  Set.fold (fun lst acc -> acc + List.length lst) s 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    expensive_fold: inferred=O(n^2) declared=O(n^2) [OK]
  |}]
