(* @complexity O(n)
   A dummy example file for gauge to parse. *)

let rec sum n =
  if n <= 0 then 0 else n + sum (n-1)

let () =
  Printf.printf "%d\n" (sum 5)
