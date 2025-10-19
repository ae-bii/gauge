(* Tests for Seq module operations - lazy sequences *)

(* ============ O(1) Lazy Constructors ============ *)

let%expect_test "Seq.empty O(1)" =
  let code = {|
(* @complexity empty_seq: O(1) *)
let empty_seq () = Seq.empty
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    empty_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.return O(1)" =
  let code = {|
(* @complexity return_seq: O(1) *)
let return_seq x = Seq.return x
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    return_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.cons O(1)" =
  let code = {|
(* @complexity cons_seq: O(1) *)
let cons_seq x s = Seq.cons x s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    cons_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.singleton O(1)" =
  let code = {|
(* @complexity singleton_seq: O(1) *)
let singleton_seq x = Seq.singleton x
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    singleton_seq: inferred=O(1) declared=O(1) [OK]
  |}]

(* ============ O(1) Inspection Operations ============ *)

let%expect_test "Seq.is_empty O(1)" =
  let code = {|
(* @complexity is_empty_seq: O(1) *)
let is_empty_seq s = Seq.is_empty s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    is_empty_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.uncons O(1)" =
  let code = {|
(* @complexity uncons_seq: O(1) *)
let uncons_seq s = Seq.uncons s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    uncons_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.head O(1)" =
  let code = {|
(* @complexity head_seq: O(1) *)
let head_seq s = Seq.head s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    head_seq: inferred=O(1) declared=O(1) [OK]
  |}]

(* ============ O(1) Lazy Transformations ============ *)

let%expect_test "Seq.map O(1) lazy" =
  let code = {|
(* @complexity map_seq: O(1) *)
let map_seq f s = Seq.map f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.filter O(1) lazy" =
  let code = {|
(* @complexity filter_seq: O(1) *)
let filter_seq f s = Seq.filter f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    filter_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.filter_map O(1) lazy" =
  let code = {|
(* @complexity filter_map_seq: O(1) *)
let filter_map_seq f s = Seq.filter_map f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    filter_map_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.take O(1) lazy" =
  let code = {|
(* @complexity take_seq: O(1) *)
let take_seq n s = Seq.take n s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    take_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.drop O(1) lazy" =
  let code = {|
(* @complexity drop_seq: O(1) *)
let drop_seq n s = Seq.drop n s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    drop_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.append O(1) lazy" =
  let code = {|
(* @complexity append_seq: O(1) *)
let append_seq s1 s2 = Seq.append s1 s2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    append_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.flat_map O(1) lazy" =
  let code = {|
(* @complexity flat_map_seq: O(1) *)
let flat_map_seq f s = Seq.flat_map f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    flat_map_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Seq.zip O(1) lazy" =
  let code = {|
(* @complexity zip_seq: O(1) *)
let zip_seq s1 s2 = Seq.zip s1 s2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    zip_seq: inferred=O(1) declared=O(1) [OK]
  |}]

(* ============ O(n) Consumption Operations ============ *)

let%expect_test "Seq.iter O(n)" =
  let code = {|
(* @complexity iter_seq: O(n) *)
let iter_seq f s = Seq.iter f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    iter_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Seq.fold_left O(n)" =
  let code = {|
(* @complexity fold_seq: O(n) *)
let fold_seq f acc s = Seq.fold_left f acc s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    fold_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Seq.length O(n)" =
  let code = {|
(* @complexity length_seq: O(n) *)
let length_seq s = Seq.length s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    length_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Seq.for_all O(n)" =
  let code = {|
(* @complexity for_all_seq: O(n) *)
let for_all_seq f s = Seq.for_all f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    for_all_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Seq.exists O(n)" =
  let code = {|
(* @complexity exists_seq: O(n) *)
let exists_seq f s = Seq.exists f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    exists_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Seq.find O(n)" =
  let code = {|
(* @complexity find_seq: O(n) *)
let find_seq f s = Seq.find f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    find_seq: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ O(n) Conversions ============ *)

let%expect_test "Seq.of_list O(n)" =
  let code = {|
(* @complexity seq_of_list: O(n) *)
let seq_of_list lst = Seq.of_list lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    seq_of_list: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Seq.to_list O(n)" =
  let code = {|
(* @complexity list_of_seq: O(n) *)
let list_of_seq s = Seq.to_list s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    list_of_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Seq.of_array O(n)" =
  let code = {|
(* @complexity seq_of_array: O(n) *)
let seq_of_array arr = Seq.of_array arr
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    seq_of_array: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Real-World Lazy Patterns ============ *)

let%expect_test "chained lazy transformations O(1)" =
  let code = {|
(* @complexity chain_lazy: O(1) *)
let chain_lazy s =
  s 
  |> Seq.map (fun x -> x * 2)
  |> Seq.filter (fun x -> x > 0)
  |> Seq.take 10
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    chain_lazy: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "lazy map then consume O(n)" =
  let code = {|
(* @complexity map_consume: O(n) *)
let map_consume s =
  let mapped = Seq.map (fun x -> x * 2) s in
  Seq.to_list mapped
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_consume: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "sum sequence O(n)" =
  let code = {|
(* @complexity sum_seq: O(n) *)
let sum_seq s =
  Seq.fold_left (+) 0 s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    sum_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "lazy infinite sequence O(1)" =
  let code = {|
(* @complexity infinite_seq: O(1) *)
let infinite_seq () =
  Seq.unfold (fun n -> Some (n, n + 1)) 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    infinite_seq: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "take from infinite O(1)" =
  let code = {|
(* @complexity take_infinite: O(1) *)
let take_infinite n =
  Seq.unfold (fun i -> Some (i, i + 1)) 0
  |> Seq.take n
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    take_infinite: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "consume taken sequence O(n)" =
  let code = {|
(* @complexity consume_taken: O(n) *)
let consume_taken n =
  Seq.unfold (fun i -> Some (i, i + 1)) 0
  |> Seq.take n
  |> Seq.fold_left (+) 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    consume_taken: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "filter and count O(n)" =
  let code = {|
(* @complexity filter_count: O(n) *)
let filter_count s =
  let filtered = Seq.filter (fun x -> x mod 2 = 0) s in
  Seq.length filtered
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    filter_count: inferred=O(n) declared=O(n) [OK]
  |}]
