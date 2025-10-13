type t =
  | O1
  | On
  | On2
  | OLog
  | OUnknown

let to_string = function
  | O1 -> "O(1)"
  | On -> "O(n)"
  | On2 -> "O(n^2)"
  | OLog -> "O(log n)"
  | OUnknown -> "O(?)"

let of_string s =
  match String.trim s with
  | "O(1)" | "1" -> Some O1
  | "O(n)" | "n" -> Some On
  | "O(n^2)" | "n^2" -> Some On2
  | "O(log n)" | "log n" -> Some OLog
  | _ -> None

let max a b =
  match (a, b) with
  | OUnknown, x -> x
  | x, OUnknown -> x
  | On2, _ | _, On2 -> On2
  | On, _ | _, On -> On
  | OLog, _ | _, OLog -> OLog
  | O1, O1 -> O1
