type cost = { degree : int; log : int }

let o1 = { degree = 0; log = 0 }
let on = { degree = 1; log = 0 }
let on2 = { degree = 2; log = 0 }
let olog = { degree = 0; log = 1 }
let ounk = { degree = -1; log = -1 }

let to_string c =
  if c.degree < 0 then "O(?)"
  else if c.log <> 0 then Printf.sprintf "O(n^%d log^%d n)" c.degree c.log
  else if c.degree = 0 then "O(1)"
  else if c.degree = 1 then "O(n)"
  else Printf.sprintf "O(n^%d)" c.degree

let of_string s =
  match String.trim s with
  | "O(1)" | "1" -> Some o1
  | "O(n)" | "n" -> Some on
  | "O(n^2)" | "n^2" -> Some on2
  | "O(log n)" | "log n" -> Some olog
  | _ -> None

(* Algebra *)
let max_cost a b =
  if a.degree < 0 then b else if b.degree < 0 then a
  else if a.degree > b.degree then a
  else if b.degree > a.degree then b
  else (* same degree *) if a.log >= b.log then a else b

let mul_cost a b =
  if a.degree < 0 || b.degree < 0 then ounk
  else { degree = a.degree + b.degree; log = a.log + b.log }

let seq_cost = max_cost

