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
  let s = String.trim s in
  match s with
  | "O(1)" | "1" -> Some o1
  | "O(n)" | "n" -> Some on
  | "O(n^2)" | "n^2" -> Some on2
  | "O(log n)" | "log n" -> Some olog
  | _ ->
      (* Try to parse O(n^k) for arbitrary k *)
      try
        let len = String.length s in
        if len > 5 && String.sub s 0 4 = "O(n^" then
          let rest = String.sub s 4 (len - 4) in
          if String.length rest > 0 && rest.[String.length rest - 1] = ')' then
            let num_str = String.sub rest 0 (String.length rest - 1) in
            let degree = int_of_string num_str in
            if degree >= 0 then Some { degree; log = 0 } else None
          else None
        (* Also try without the O() wrapper: n^k *)
        else if len > 2 && String.sub s 0 2 = "n^" then
          let num_str = String.sub s 2 (len - 2) in
          let degree = int_of_string num_str in
          if degree >= 0 then Some { degree; log = 0 } else None
        else None
      with _ -> None

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

let equal a b =
  if a.degree < 0 && b.degree < 0 then true
  else a.degree = b.degree && a.log = b.log

