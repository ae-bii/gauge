let f n = let rec go k = if k <= 0 then 0 else go (k-1) in go n

let () = Printf.printf "%d\n" (f 5)
