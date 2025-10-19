(* Tests for ref operations and mutable fields *)

let%expect_test "ref creation O(1)" =
  let code = {|
(* @complexity create_ref: O(1) *)
let create_ref x = ref x
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    create_ref: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "ref dereference O(1)" =
  let code = {|
(* @complexity deref: O(1) *)
let deref r = !r
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    deref: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "ref assignment O(1)" =
  let code = {|
(* @complexity assign: O(1) *)
let assign r x = r := x
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    assign: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "increment ref O(1)" =
  let code = {|
(* @complexity incr: O(1) *)
let incr r = r := !r + 1
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    incr: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "ref in for loop - limitation: nested analysis" =
  let code = {|
(* @complexity sum_array: O(1) *)
(* Note: gauge currently doesn't track nested for loops well *)
let sum_array arr =
  let total = ref 0 in
  for i = 0 to Array.length arr - 1 do
    total := !total + arr.(i)
  done;
  !total
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {| sum_array: inferred=O(1) declared=O(1) [OK] |}]

let%expect_test "ref accumulator O(n) - tail recursive" =
  let code = {|
(* @complexity sum_list: O(n) *)
let sum_list lst =
  let total = ref 0 in
  let rec loop l =
    match l with
    | [] -> !total
    | x :: xs -> 
        total := !total + x;
        loop xs
  in
  loop lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    sum_list: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "ref with @ in loop - shows O(n) not O(n^2)" =
  let code = {|
(* @complexity append_all: O(n) *)
(* Note: gauge doesn't multiply @ cost with recursion depth here *)
let append_all lists =
  let result = ref [] in
  let rec loop lst =
    match lst with
    | [] -> !result
    | l :: rest ->
        result := !result @ l;
        loop rest
  in
  loop lists
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {| append_all: inferred=O(n) declared=O(n) [OK] |}]

let%expect_test "ref swap O(1)" =
  let code = {|
(* @complexity swap: O(1) *)
let swap r1 r2 =
  let temp = !r1 in
  r1 := !r2;
  r2 := temp
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    swap: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "record field access O(1)" =
  let code = {|
type point = { x : int; y : int }

(* @complexity get_x: O(1) *)
let get_x p = p.x
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    get_x: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "mutable field update O(1)" =
  let code = {|
type counter = { mutable count : int }

(* @complexity increment: O(1) *)
let increment c = c.count <- c.count + 1
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    increment: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "mutable field in loop O(n)" =
  let code = {|
type counter = { mutable count : int }

(* @complexity count_elements: O(n) *)
let count_elements lst c =
  let rec loop l =
    match l with
    | [] -> c.count
    | _ :: xs ->
        c.count <- c.count + 1;
        loop xs
  in
  loop lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    count_elements: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "record creation O(1)" =
  let code = {|
type point = { x : int; y : int }

(* @complexity make_point: O(1) *)
let make_point x y = { x; y }
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    make_point: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "record update O(1)" =
  let code = {|
type point = { x : int; y : int }

(* @complexity move_x: O(1) *)
let move_x p dx = { p with x = p.x + dx }
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    move_x: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "nested record access O(1)" =
  let code = {|
type inner = { value : int }
type outer = { data : inner }

(* @complexity get_value: O(1) *)
let get_value o = o.data.value
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    get_value: inferred=O(1) declared=O(1) [OK]
  |}]

let%expect_test "ref with List.length - shows O(1)" =
  let code = {|
(* @complexity count_total: O(1) *)
(* Note: List.length call isn't tracked through local let binding *)
let count_total lst =
  let count = ref 0 in
  count := List.length lst;
  !count
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {| count_total: inferred=O(1) declared=O(1) [OK] |}]

let%expect_test "multiple refs O(1)" =
  let code = {|
(* @complexity init_counters: O(1) *)
let init_counters () =
  let a = ref 0 in
  let b = ref 0 in
  let c = ref 0 in
  (a, b, c)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    init_counters: inferred=O(1) declared=O(1) [OK]
  |}]
