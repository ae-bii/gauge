(* Tests for Queue module operations *)

(* ============ O(1) Operations ============ *)

let%expect_test "Queue.create O(1)" =
  let code = {|
(* @complexity create_queue: O(1) *)
let create_queue () = Queue.create ()
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    create_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.add O(1)" =
  let code = {|
(* @complexity add_to_queue: O(1) *)
let add_to_queue q x = Queue.add x q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    add_to_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.push O(1)" =
  let code = {|
(* @complexity push_to_queue: O(1) *)
let push_to_queue q x = Queue.push x q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    push_to_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.take O(1)" =
  let code = {|
(* @complexity take_from_queue: O(1) *)
let take_from_queue q = Queue.take q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    take_from_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.take_opt O(1)" =
  let code = {|
(* @complexity take_opt_from_queue: O(1) *)
let take_opt_from_queue q = Queue.take_opt q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    take_opt_from_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.pop O(1)" =
  let code = {|
(* @complexity pop_from_queue: O(1) *)
let pop_from_queue q = Queue.pop q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    pop_from_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.pop_opt O(1)" =
  let code = {|
(* @complexity pop_opt_from_queue: O(1) *)
let pop_opt_from_queue q = Queue.pop_opt q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    pop_opt_from_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.peek O(1)" =
  let code = {|
(* @complexity peek_queue: O(1) *)
let peek_queue q = Queue.peek q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    peek_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.peek_opt O(1)" =
  let code = {|
(* @complexity peek_opt_queue: O(1) *)
let peek_opt_queue q = Queue.peek_opt q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    peek_opt_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.top O(1)" =
  let code = {|
(* @complexity top_queue: O(1) *)
let top_queue q = Queue.top q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    top_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.top_opt O(1)" =
  let code = {|
(* @complexity top_opt_queue: O(1) *)
let top_opt_queue q = Queue.top_opt q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    top_opt_queue: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.is_empty O(1)" =
  let code = {|
(* @complexity is_queue_empty: O(1) *)
let is_queue_empty q = Queue.is_empty q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    is_queue_empty: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.length O(1)" =
  let code = {|
(* @complexity queue_length: O(1) *)
let queue_length q = Queue.length q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    queue_length: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Queue.clear O(1)" =
  let code = {|
(* @complexity clear_queue: O(1) *)
let clear_queue q = Queue.clear q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    clear_queue: inferred=O(1) declared=O(1) [OK]
  |}]

(* ============ O(n) Operations ============ *)

let%expect_test "Queue.iter O(n)" =
  let code = {|
(* @complexity iter_queue: O(n) *)
let iter_queue f q = Queue.iter f q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    iter_queue: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Queue.fold O(n)" =
  let code = {|
(* @complexity fold_queue: O(n) *)
let fold_queue f acc q = Queue.fold f acc q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    fold_queue: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Queue.transfer O(n)" =
  let code = {|
(* @complexity transfer_queue: O(n) *)
let transfer_queue q1 q2 = Queue.transfer q1 q2
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    transfer_queue: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Queue.to_seq O(n)" =
  let code = {|
(* @complexity queue_to_seq: O(n) *)
let queue_to_seq q = Queue.to_seq q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    queue_to_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Queue.add_seq O(n)" =
  let code = {|
(* @complexity add_seq_to_queue: O(n) *)
let add_seq_to_queue q s = Queue.add_seq q s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    add_seq_to_queue: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Queue.of_seq O(n)" =
  let code = {|
(* @complexity queue_of_seq: O(n) *)
let queue_of_seq s = Queue.of_seq s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    queue_of_seq: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Real-World Patterns ============ *)

let%expect_test "build queue from list O(n)" =
  let code = {|
(* @complexity build_queue: O(n) *)
let build_queue lst =
  let q = Queue.create () in
  List.iter (Queue.add q) lst;
  q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    build_queue: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "process all in queue O(n)" =
  let code = {|
(* @complexity process_queue: O(n) *)
let process_queue q =
  let result = ref [] in
  while not (Queue.is_empty q) do
    let x = Queue.take q in
    result := x :: !result
  done;
  !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    process_queue: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "count queue elements O(n)" =
  let code = {|
(* @complexity count_queue: O(n) *)
let count_queue q =
  Queue.fold (fun acc _ -> acc + 1) 0 q
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    count_queue: inferred=O(n) declared=O(n) [OK]
  |}]
