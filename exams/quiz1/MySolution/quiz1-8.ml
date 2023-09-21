let sort5 a b c d e =
  let min2 x y = if x < y then x else y in
  let max2 x y = if x > y then x else y in
  let min3 x y z = min2 x (min2 y z) in
  let max3 x y z = max2 x (max2 y z) in

  let smallest = min5 a b c d e in
  let biggest = max5 a b c d e in



(* ************************************************ *)
