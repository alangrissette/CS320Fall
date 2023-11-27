

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf
#use "./../../../classlib/OCaml/MyOCaml.ml";;
Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.
let interp (s : string) : string list option = (* YOUR CODE *)
*)
type 'a parser = char list -> ('a * char list) option

let implode ls =
   String.of_seq (List.to_seq ls)

let explode (s: string) : char list = 
   let rec expl i l = 
     if i < 0 then l 
     else expl (i - 1) (String.get s i :: l) in 
   expl (String.length s - 1) []

let parse (p : 'a parser) (s : string) : ('a * char list) option =
   p (explode s)
 
 let pure (x : 'a) : 'a parser =
   fun ls -> Some (x, ls)
 
 let fail : 'a parser = fun ls -> None
 
 let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
   fun ls ->
     match p ls with
     | Some (a, ls) -> q a ls
     | None -> None
 
 let (>>=) = bind
 let (let*) = bind
 
 let read : char parser =
   fun ls ->
   match ls with
   | x :: ls -> Some (x, ls)
   | _ -> None
 
 let satisfy (f : char -> bool) : char parser =
   fun ls ->
   match ls with
   | x :: ls ->
     if f x then Some (x, ls)
     else None
   | _ -> None
 
 let char (c : char) : char parser =
   satisfy (fun x -> x = c)
 
 let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
   fun ls ->
   match p1 ls with
   | Some (_, ls) -> p2 ls
   | None -> None
 
 let (>>) = seq
 
 let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
   fun ls ->
   match p1 ls with
   | Some (x, ls) ->
     (match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
   | None -> None
 
 let (<<) = seq'
 
 let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
   fun ls ->
   match p1 ls with
   | Some (x, ls)  -> Some (x, ls)
   | None -> p2 ls
 
 let (<|>) = disj
 
 let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
   fun ls ->
   match p ls with
   | Some (a, ls) -> Some (f a, ls)
   | None -> None
 
 let (>|=) = map
 
 let (>|) = fun p c -> map p (fun _ -> c)
 
 let rec many (p : 'a parser) : ('a list) parser =
   fun ls ->
   match p ls with
   | Some (x, ls) ->
     (match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some (x :: [], ls))
   | None -> Some ([], ls)
 
 let rec many1 (p : 'a parser) : ('a list) parser =
   fun ls ->
   match p ls with
   | Some (x, ls) ->
     (match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some (x :: [], ls))
   | None -> None
 
 let rec many' (p : unit -> 'a parser) : ('a list) parser =
   fun ls ->
   match p () ls with
   | Some (x, ls) ->
     (match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some (x :: [], ls))
   | None -> Some ([], ls)
 
 let rec many1' (p : unit -> 'a parser) : ('a list) parser =
   fun ls ->
   match p () ls with
   | Some (x, ls) ->
     (match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some (x :: [], ls))
   | None -> None
 
 let whitespace : unit parser =
   fun ls ->
   match ls with
   | c :: ls ->
     if String.contains " \012\n\r\t" c
     then Some ((), ls)
     else None
   | _ -> None
 
 let ws : unit parser =
   (many whitespace) >| ()
 
 let ws1 : unit parser =
   (many1 whitespace) >| ()
 

let is_digit c =
   '0' <= c && c <= '9'
 
 let digit : char parser =
   satisfy is_digit
 
 let natural : int parser =
   fun ls ->
   match many1 digit ls with
   | Some (xs, ls) ->
     Some (int_of_string (implode xs), ls)
   | _ -> None
 
 let literal (s : string) : unit parser =
   fun ls ->
   let cs = explode s in
   let rec loop cs ls =
     match cs, ls with
     | [], _ -> Some ((), ls)
     | c :: cs, x :: xs ->
       if x = c
       then loop cs xs
       else None
     | _ -> None
   in loop cs ls
 
 let keyword (s : string) : unit parser =
   (literal s) >> ws >| ()
                          
                          (* end of parser combinators *)
 
 


type 'a parser = char list -> ('a * char list) option

type const = D of int | B of bool | Unit

type stack = const list


type com =
  | Push of const
  | Pop
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt


let push_parser : com parser =
  (literal "push") >>
  whitespace >>
  (natural >>= fun n -> pure (Push (D n)))

let pop_parser : com parser =
  (literal "pop") >>
  pure Pop

let trace_parser : com parser =
  (literal "trace") >>
  pure Trace

let add_parser : com parser =
  (literal "add") >>
  pure Add

let sub_parser : com parser =
  (literal "sub") >>
  pure Sub

let mul_parser : com parser =
  (literal "mul") >>
  pure Mul

let div_parser : com parser =
  (literal "div") >>
  pure Div

let and_parser : com parser =
  (literal "and") >>
  pure And

let or_parser : com parser =
  (literal "or") >>
  pure Or

let not_parser : com parser =
  (literal "not") >>
  pure Not

let lt_parser : com parser =
  (literal "lt") >>
  pure Lt

let gt_parser : com parser =
  (literal "gt") >>
  pure Gt

  let command_list_parser : com list parser =
   many (push_parser <|> pop_parser <|> trace_parser <|> add_parser <|> sub_parser
         <|> mul_parser <|> div_parser <|> and_parser <|> or_parser <|> not_parser
         <|> lt_parser <|> gt_parser)

let parse_commands (s : string) : com list option =
   match parse command_list_parser s with
      | Some (commands, []) -> Some commands
      | _ -> None

let execute_push (stack : stack) (const : const) : stack =
         const :: stack

let pop_stack (s : stack) : stack =
  match s with
  | [] -> []
  | _ :: rest -> rest


let toString (c : const) : string =
   match c with
   | D n -> string_of_int n
   | B true -> "True"
   | B false -> "False"
   | Unit -> "Unit"

  let trace_stack (s : stack) : (stack * string) =
   match s with
   | [] -> ([], "Panic")
   | c :: rest -> (Unit :: rest, toString c)

   let add_stack (stack : const list) : (const list * string list) option =
      match stack with
      | D i :: D j :: rest -> Some (D (i + j) :: rest, [])
      | D _ :: _ :: _ :: _ -> Some ([], ["Panic"; "AddError1"]) (* AddError1: Stack has more than 2 elements *)
      | [_] -> Some ([], ["Panic"; "AddError3"]) (* AddError3: Stack has only 1 element *)
      | _ -> Some ([], ["Panic"; "AddError2"]) (* AddError2: Stack is empty *)
    

      let sub_stack (stack : const list) : (const list * string list) option =
         match stack with
         | D i :: D j :: rest -> Some (D (i - j) :: rest, [])
         | D _ :: _ :: _ :: _ -> Some ([], ["Panic"; "SubError1"]) (* SubError1: Stack has more than 2 elements *)
         | [_] -> Some ([], ["Panic"; "SubError3"]) (* SubError3: Stack has only 1 element *)
         | _ -> Some ([], ["Panic"; "SubError2"]) (* SubError2: Stack is empty *)
       

         let mul_stack (stack : const list) : (const list * string list) option =
            match stack with
            | D i :: D j :: rest -> Some (D (i * j) :: rest, [])
            | D _ :: _ :: _ :: _ -> Some ([], ["Panic"; "MulError1"]) (* MulError1: Stack has more than 2 elements *)
            | [_] -> Some ([], ["Panic"; "MulError3"]) (* MulError3: Stack has only 1 element *)
            | _ -> Some ([], ["Panic"; "MulError2"]) (* MulError2: Stack is empty *)
          

  let div_stack (stack : const list) : (const list * string list) option =
   match stack with
   | D i :: D 0 :: rest -> Some ([], ["Panic"; "DivError0"])
   | D i :: D j :: rest ->
     if i mod j <> 0 then Some ([], ["Panic"; "DivError1"]) (* DivError1: i is not divisible by j *)
     else Some ((D (i / j) :: rest, []))
   | _ :: _ :: _ :: _ -> Some ([], ["Panic"; "DivError2"]) (* DivError2: Stack has more than 2 elements *)
   | [_] -> Some ([], ["Panic"; "DivError3"]) (* DivError3: Stack has only 1 element *)
   | _ -> Some ([], ["Panic"]) (* Default case for unexpected scenarios *)

   let and_stack (stack : const list) : (const list * string list) option =
      match stack with
      | B a :: B b :: rest -> Some (B (a && b) :: rest, [])
      | B _ :: _ :: _ :: _ -> Some ([], ["Panic"; "AndError1"]) (* AndError1: Stack has more than 2 elements *)
      | [_] -> Some ([], ["Panic"; "AndError3"]) (* AndError3: Stack has only 1 element *)
      | _ -> Some ([], ["Panic"; "AndError2"]) (* AndError2: Stack is empty *)
    
      let or_stack (stack : const list) : (const list * string list) option =
         match stack with
         | B a :: B b :: rest -> Some (B (a || b) :: rest, [])
         | B _ :: _ :: _ :: _ -> Some ([], ["Panic"; "OrError1"]) (* OrError1: Stack has more than 2 elements *)
         | [_] -> Some ([], ["Panic"; "OrError3"]) (* OrError3: Stack has only 1 element *)
         | _ -> Some ([], ["Panic"; "OrError2"]) (* OrError2: Stack is empty *)

         let not_stack (stack : const list) : (const list * string list) option =
            match stack with
            | B a :: rest -> Some (B (not a) :: rest, [])
            | _ :: _ :: _ -> Some ([], ["Panic"; "NotError1"]) (* NotError1: Stack has more than 1 element *)
            | [_] -> Some ([], ["Panic"; "NotError2"]) (* NotError2: Stack is empty *)
            | _ -> Some ([], ["Panic"; "NotError1"]) (* NotError1: Top element is not a boolean *)

            let lt_stack (stack : const list) : (const list * string list) option =
               match stack with
               | D i :: D j :: rest -> Some (B (i < j) :: rest, [])
               | _ :: _ :: _ -> Some ([], ["Panic"; "LtError1"]) (* LtError1: Stack has more than 2 elements *)
               | [_] -> Some ([], ["Panic"; "LtError3"]) (* LtError3: Stack has only 1 element *)
               | _ -> Some ([], ["Panic"; "LtError1"]) (* LtError1: Top elements are not integers *)
             
               let gt_stack (stack : const list) : (const list * string list) option =
                  match stack with
                  | D i :: D j :: rest -> Some (B (i > j) :: rest, [])
                  | _ :: _ :: _ -> Some ([], ["Panic"; "GtError1"]) (* GtError1: Stack has more than 2 elements *)
                  | [_] -> Some ([], ["Panic"; "GtError3"]) (* GtError3: Stack has only 1 element *)
                  | _ -> Some ([], ["Panic"; "GtError1"]) (* GtError1: Top elements are not integers *)

                  let rec run_program (commands : com list) (stack : stack) : (stack * string list) option =
                     match commands with
                     | [] -> Some (stack, []) (* Base case: No more commands to execute *)
                     | cmd :: rest ->
                       match cmd with
                       | Push c -> run_program rest (execute_push stack c) (* Push command *)
                       | Pop -> run_program rest (pop_stack stack) (* Pop command *)
                       | Trace ->
                         let (new_stack, output) = trace_stack stack in
                         Printf.printf "%s\n" (String.concat "; " (List.map toString new_stack)); (* Print trace output *)
                         run_program rest new_stack
                       | Add ->
                         (match add_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "AddError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | Sub ->
                         (match sub_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "SubError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | Mul ->
                         (match mul_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "MulError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | Div ->
                         (match div_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "DivError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | And ->
                         (match and_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "AndError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | Or ->
                         (match or_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "OrError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | Not ->
                         (match not_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "NotError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | Lt ->
                         (match lt_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "LtError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                       | Gt ->
                         (match gt_stack stack with
                          | Some (new_stack, errors) ->
                            if List.length errors > 0 then Printf.printf "GtError: %s\n" (String.concat "; " errors);
                            run_program rest new_stack
                          | None -> None)
                   
let interp (s : string) : (stack * string list) option =
   match parse_commands s with
   | Some commands ->
     (match run_program commands [] with
     | Some (final_stack, output) -> Some (final_stack, output)
     | None -> None)
   | None -> None

   