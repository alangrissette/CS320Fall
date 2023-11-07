#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

let parse (s : string) : expr option =
  let rec parse_expr tokens =
    match tokens with
    | [] -> None
    | '(' :: 'a' :: 'd' :: 'd' :: rest -> parse_add rest
    | '(' :: 'm' :: 'u' :: 'l' :: rest -> parse_mul rest
    | _ -> parse_num tokens

  and parse_add tokens =
    let rec loop acc tokens =
      match tokens with
      | [] -> None
      | ')' :: rest -> Some (Add (List.rev acc), rest)
      | _ -> (
          match parse_expr tokens with
          | Some (expr, new_tokens) -> loop (expr :: acc) new_tokens
          | None -> None
        )
    in
    loop [] tokens

  and parse_mul tokens =
    let rec loop acc tokens =
      match tokens with
      | [] -> None
      | ')' :: rest -> Some (Mul (List.rev acc), rest)
      | _ -> (
          match parse_expr tokens with
          | Some (expr, new_tokens) -> loop (expr :: acc) new_tokens
          | None -> None
        )
    in
    loop [] tokens

  and parse_num tokens =
    match tokens with
    | [] -> None
    | '0' .. '9' :: rest ->
        let num, new_tokens = parse_digit tokens in
        Some (Int num, new_tokens)
    | '(' :: 'a' :: 'd' :: 'd' :: _ -> parse_add tokens
    | '(' :: 'm' :: 'u' :: 'l' :: _ -> parse_mul tokens
    | _ -> None

  and parse_digit tokens =
    let rec loop acc tokens =
      match tokens with
      | [] -> (acc, tokens)
      | '0' .. '9' :: rest -> loop (acc * 10 + int_of_char rest - int_of_char '0') rest
      | _ -> (acc, tokens)
    in
    loop 0 tokens
  in

  let char_list = string_listize s |> trim in
  match parse_expr char_list with
  | Some (expr, []) -> Some expr
  | _ -> None
