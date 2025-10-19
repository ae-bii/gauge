(* Tests for Stack module operations *)

(* ============ O(1) Operations ============ *)

let%expect_test "Stack.create O(1)" =
  let code = {|
(* @complexity create_stack: O(1) *)
let create_stack () = Stack.create ()
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    create_stack: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.push O(1)" =
  let code = {|
(* @complexity push_to_stack: O(1) *)
let push_to_stack s x = Stack.push x s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    push_to_stack: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.pop O(1)" =
  let code = {|
(* @complexity pop_from_stack: O(1) *)
let pop_from_stack s = Stack.pop s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    pop_from_stack: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.pop_opt O(1)" =
  let code = {|
(* @complexity pop_opt_from_stack: O(1) *)
let pop_opt_from_stack s = Stack.pop_opt s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    pop_opt_from_stack: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.top O(1)" =
  let code = {|
(* @complexity top_stack: O(1) *)
let top_stack s = Stack.top s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    top_stack: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.top_opt O(1)" =
  let code = {|
(* @complexity top_opt_stack: O(1) *)
let top_opt_stack s = Stack.top_opt s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    top_opt_stack: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.is_empty O(1)" =
  let code = {|
(* @complexity is_stack_empty: O(1) *)
let is_stack_empty s = Stack.is_empty s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    is_stack_empty: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.length O(1)" =
  let code = {|
(* @complexity stack_length: O(1) *)
let stack_length s = Stack.length s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    stack_length: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "Stack.clear O(1)" =
  let code = {|
(* @complexity clear_stack: O(1) *)
let clear_stack s = Stack.clear s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    clear_stack: inferred=O(1) declared=O(1) [OK]
  |}]

(* ============ O(n) Operations ============ *)

let%expect_test "Stack.iter O(n)" =
  let code = {|
(* @complexity iter_stack: O(n) *)
let iter_stack f s = Stack.iter f s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    iter_stack: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Stack.fold O(n)" =
  let code = {|
(* @complexity fold_stack: O(n) *)
let fold_stack f acc s = Stack.fold f acc s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    fold_stack: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Stack.to_seq O(n)" =
  let code = {|
(* @complexity stack_to_seq: O(n) *)
let stack_to_seq s = Stack.to_seq s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    stack_to_seq: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "Stack.of_seq O(n)" =
  let code = {|
(* @complexity stack_of_seq: O(n) *)
let stack_of_seq seq = Stack.of_seq seq
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    stack_of_seq: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Real-World Patterns ============ *)

let%expect_test "build stack from list O(n)" =
  let code = {|
(* @complexity build_stack: O(n) *)
let build_stack lst =
  let s = Stack.create () in
  List.iter (Stack.push s) lst;
  s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    build_stack: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "process all in stack O(n)" =
  let code = {|
(* @complexity process_stack: O(n) *)
let process_stack s =
  let result = ref [] in
  while not (Stack.is_empty s) do
    let x = Stack.pop s in
    result := x :: !result
  done;
  !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    process_stack: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "reverse using stack O(n)" =
  let code = {|
(* @complexity reverse_with_stack: O(n) *)
let reverse_with_stack lst =
  let s = Stack.create () in
  List.iter (Stack.push s) lst;
  let result = ref [] in
  while not (Stack.is_empty s) do
    result := Stack.pop s :: !result
  done;
  !result
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    reverse_with_stack: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "sum stack elements O(n)" =
  let code = {|
(* @complexity sum_stack: O(n) *)
let sum_stack s =
  Stack.fold (fun acc x -> acc + x) 0 s
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    sum_stack: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "peek without modification O(1)" =
  let code = {|
(* @complexity peek_stack: O(1) *)
let peek_stack s =
  match Stack.top_opt s with
  | Some x -> x
  | None -> 0
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    peek_stack: inferred=O(1) declared=O(1) [OK]
  |}]
