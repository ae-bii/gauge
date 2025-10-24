(* Tests for improved recursive pattern recognition *)

(* ============ Tail Recursion Patterns ============ *)

let%expect_test "tail recursive list length O(n)" =
  let code = {|
(* @complexity length: O(n) *)
let length lst =
  let rec aux acc = function
    | [] -> acc
    | _ :: xs -> aux (acc + 1) xs
  in
  aux 0 lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    length: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "tail recursive sum O(n)" =
  let code = {|
(* @complexity sum: O(n) *)
let sum lst =
  let rec loop acc = function
    | [] -> acc
    | x :: xs -> loop (acc + x) xs
  in
  loop 0 lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    sum: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "tail recursive reverse O(n)" =
  let code = {|
(* @complexity reverse: O(n) *)
let reverse lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    reverse: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Non-Tail Recursive with @ ============ *)

let%expect_test "non-tail recursive append O(n^2)" =
  let code = {|
(* @complexity slow_reverse: O(n^2) *)
let rec slow_reverse = function
  | [] -> []
  | x :: xs -> slow_reverse xs @ [x]
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    slow_reverse: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

let%expect_test "recursive flatten O(n^2)" =
  let code = {|
(* @complexity flatten: O(n^2) *)
let rec flatten = function
  | [] -> []
  | x :: xs -> x @ flatten xs
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    flatten: inferred=O(n^2) declared=O(n^2) [OK]
  |}]

(* ============ Tree Recursion ============ *)

let%expect_test "binary tree size O(n)" =
  let code = {|
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* @complexity size: O(n) *)
let rec size = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + size left + size right
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    size: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "tree map O(n)" =
  let code = {|
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* @complexity tree_map: O(n) *)
let rec tree_map f = function
  | Leaf -> Leaf
  | Node (x, left, right) -> 
      Node (f x, tree_map f left, tree_map f right)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    tree_map: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Mutual Recursion ============ *)

let%expect_test "mutually recursive even/odd O(n)" =
  let code = {|
(* @complexity is_even: O(n) *)
let rec is_even n =
  if n = 0 then true
  else is_odd (n - 1)

and is_odd n =
  if n = 0 then false
  else is_even (n - 1)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    is_odd: inferred=O(n) declared=(none)
    is_even: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Nested Recursion ============ *)

let%expect_test "nested list map O(n)" =
  let code = {|
(* @complexity map_inc: O(n) *)
let rec map_inc = function
  | [] -> []
  | x :: xs -> (x + 1) :: map_inc xs
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    map_inc: inferred=O(n) declared=O(n) [OK]
  |}]

let%expect_test "recursive filter O(n)" =
  let code = {|
(* @complexity filter_positive: O(n) *)
let rec filter_positive = function
  | [] -> []
  | x :: xs ->
      if x > 0 then x :: filter_positive xs
      else filter_positive xs
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    filter_positive: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Divide and Conquer ============ *)

let%expect_test "merge sort O(n log n) - currently over-approximated" =
  let code = {|
(* @complexity merge_sort: O(n log n) *)
(* Note: Divide-and-conquer with O(log n) depth not yet detected *)
let rec merge_sort lst =
  match lst with
  | [] | [_] -> lst
  | _ ->
      let rec split l =
        match l with
        | [] -> ([], [])
        | [x] -> ([x], [])
        | x :: y :: rest ->
            let (left, right) = split rest in
            (x :: left, y :: right)
      in
      let (left, right) = split lst in
      let sorted_left = merge_sort left in
      let sorted_right = merge_sort right in
      List.merge compare sorted_left sorted_right
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    merge_sort: inferred=O(n^12) declared=O(n log n) [MISMATCH]
  |}]

(* ============ Accumulator Pattern ============ *)

let%expect_test "tail recursive fold O(n)" =
  let code = {|
(* @complexity fold: O(n) *)
let fold f init lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (f acc x) xs
  in
  aux init lst
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    fold: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ CPS (Continuation Passing Style) ============ *)

let%expect_test "CPS factorial O(n)" =
  let code = {|
(* @complexity fact_cps: O(n) *)
let fact_cps n =
  let rec aux n k =
    if n <= 0 then k 1
    else aux (n - 1) (fun x -> k (n * x))
  in
  aux n (fun x -> x)
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    fact_cps: inferred=O(n) declared=O(n) [OK]
  |}]

(* ============ Multiple Base Cases ============ *)

let%expect_test "recursive with multiple base cases O(n)" =
  let code = {|
(* @complexity count_evens: O(n) *)
let rec count_evens = function
  | [] -> 0
  | [x] -> if x mod 2 = 0 then 1 else 0
  | x :: xs -> (if x mod 2 = 0 then 1 else 0) + count_evens xs
|} in
  let declared = Gauge.Contracts.extract_complexity_annotations code in
  let inferred = Gauge.Infer.infer_all_of_string_code code in
  let _ = Gauge.Report.report_many inferred declared in
  [%expect {|
    count_evens: inferred=O(n) declared=O(n) [OK]
  |}]
