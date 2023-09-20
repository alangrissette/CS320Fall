

let intrev10 n =
  let rec helper acc n =
    if n = 0 then acc
    else
      let last_digit = n mod 10 in
      let reversed_acc = acc * 10 + last_digit in
      let remaining_n = n / 10 in
      helper reversed_acc remaining_n
  in
  helper 0 n
