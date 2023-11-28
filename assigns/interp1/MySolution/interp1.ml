

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

exception False;;
(* ****** ****** *)
exception Subscript;;
(* ****** ****** *)

(** Return the character with the given ASCII code. **)
let chr = Char.chr;;

(** return the ASCII code of the argument **)
let ord = Char.code;;

(** make n c is a string of length n with each index holding the character c **)
let str(c0) = String.make 1 c0;;

(** checks if a character is lowercase **)
let char_islower(ch: char) = (ch >= 'a' && ch <= 'z');;

(** checks if a character is uppercase **)
let char_isupper(ch: char) = (ch >= 'A' && ch <= 'Z');;

(** checks if a character is a digit **)
let char_isdigit(ch: char) = (ch >= '0' && ch <= '9');;

(** checks if a character is a alphabetical letter **)
let char_isletter(ch: char) =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');;

(** checks if a character is a alphabetical or numerical letter **)
let char_isalphanum(ch: char) =
  char_islower(ch) || char_isupper(ch) || char_isdigit(ch);;

(** checks if a character is a whitespace character **)
let char_iswhitespace(ch: char) =
  (ch = ' ' || ch = '\n' || ch = '\r' || ch = '\t');;

(** converts a character to lowercase if applicable **)
let char_tolower(ch: char) =
  if char_isupper(ch) then chr(ord(ch) - ord('A') + ord('a')) else ch
;;

(** converts a character to uppercase if applicable **)
let char_toupper(ch: char) =
  if char_islower(ch) then chr(ord(ch) - ord('a') + ord('A')) else ch
;;

(** converts int digit to a character **)
let char_of_digit (d0: int): char =
  let () = assert(d0 >= 0) in
    let () = assert(d0 <= 9) in
      chr(ord('0') + d0)
;;(* end of [char_of_digit] *)


(** converts character to a digit**)
let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in
      ord(ch) - ord('0')
;;(* end of [digit_of_char] *)

(* ****** TYPE ANNOTATIONS ****** *)

(** takes a collection 'xs and a predicate function ('x0 -> bool), and it returns a boolean value if all satisfy or not **)
type ('xs, 'x0) forall = 'xs -> ('x0 -> bool) -> bool

(** takes a generic collection 'xs and applies a function ('x0 -> unit) which may produce a side effect **)
type ('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

(** takes a generic collection 'xs and applies a function ('x0 -> unit) which may produce a side effect in reverse order **)
type ('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

(** takes a generic type 'xs and returns a list of values of type 'x0 **)
type ('xs, 'x0) listize = 'xs -> 'x0 list

(** takes a generic type 'xs and returns an array of values of type 'x0 **)
type ('xs, 'x0) arrnize = 'xs -> 'x0 array

(** takes a generic type 'xs and returns a list of values of type 'x0 in reverse order **)
type ('xs, 'x0) rlistize = 'xs -> 'x0 list

(** takes a generic type 'xs and returns an array of values of type 'x0 in reverse order **)
type ('xs, 'x0) rarrnize = 'xs -> 'x0 array

(** takes a collection 'xs and a function ('x0 -> 'y0) for transforming elements of type 'x0 into elements of type 'y0,
    and it returns a list of the transformed elements. **)
type ('xs, 'x0, 'y0) map_list = 'xs -> ('x0 -> 'y0) -> 'y0 list

(** takes a collection 'xs and a function ('x0 -> 'y0) for transforming elements of type 'x0 into elements of type 'y0,
    and it returns a list of the transformed elements in reverse order. **)
type ('xs, 'x0, 'y0) map_rlist = 'xs -> ('x0 -> 'y0) -> 'y0 list

type ('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

type ('xs, 'x0, 'r0) foldright = 'xs -> 'r0 -> ('x0 -> 'r0 -> 'r0) -> 'r0

(* ****** ****** *)

(** run a work() fxn n0 times starting from 0 and ending at n0 **)
let int1_foreach (n0:int) (work: int -> unit): unit =
  for i0 = 0 to n0-1 do work(i0) done
;;

(** run a work() fxn n0 times starting from n0 and ending at 0 **)
let int1_rforeach (n0:int) (work: int -> unit): unit =
  for i0 = 0 to n0-1 do work(n0-1-i0) done
;;

(** init n f is a string of length n with index i holding the character f i (called in increasing index order). **)
let string_init = String.init;;

(** length s is the length (number of bytes/characters) of s **)
let string_length = String.length;;

(** read the contents of file into a string **)
let string_of_file(path: string) =
  let fp = open_in path in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(** get_at s i is the character at index i in s. This is the same as writing s.[i] **)
let string_get_at(cs:string)(i0:int): char = String.get cs i0;;

(* ****** ****** *)

(** get the first char *)
let string_head(cs:string):char = string_get_at(cs)(0)
(** get the rest of chars *)
let string_tail(cs) =
string_init(string_length(cs)-1)(fun i -> string_get_at(cs)(i+1))

(* ****** ****** *)

(** create a string c0+cs **)
let string_cons(c0: char)(cs: string): string =
  string_init(string_length(cs) + 1)(
    fun i -> if i <= 0 then c0 else string_get_at cs (i-1)
  )
;;

(** create a string cs+c0 **)
let string_snoc(cs: string)(c0: char): string =
  let len = string_length(cs) in
    string_init(len + 1)(
      fun i -> if i < len then string_get_at (cs) (i) else c0
    )
;;

(** turn cs into an uppercase string **)
let string_toupper(cs: string): string =
  string_init(string_length(cs))(
    fun i0 -> char_toupper(string_get_at(cs)(i0))
  )
;;

(** turn cs into a lowercase string **)
let string_tolower(cs: string): string =
  string_init(string_length(cs))(
    fun i0 -> char_tolower(string_get_at(cs)(i0))
  )
;;

(** for each character of string cs from 0 to length cs, apply the work function **)
let string_foreach(cs: string)(work: char -> unit) =
  int1_foreach(string_length(cs))(
    fun i0 -> work(string_get_at(cs)(i0))
  )
;;

(** for each character of string cs from length cs to 0, apply the work function **)
let string_rforeach(cs: string)(work: char -> unit) =
  int1_rforeach(string_length(cs))(
    fun i0 -> work(string_get_at(cs)(i0))
  )
;;

(** init n f is a string of length n with index i holding the character f i (called in increasing index order) **)
let string_tabulate = String.init;;

(** returns a fresh array containing the elements of xs which is a list of generic a **)
let list_arrnize(xs: 'a list): 'a array = Array.of_list(xs)

(** adds the reverse of generic list xs to generic list ys: xs=[1;2;3] ys=[4;5;6] = [3;2;1;4;5;6] **)
let rec list_revapp(xs: 'a list)(ys: 'a list): 'a list =
  match xs with
  | [] -> ys
  | x1 :: xs -> list_revapp(xs)(x1 :: ys)
;;

(** reverses a generic list **)
let list_reverse(xs: 'a list): 'a list = list_revapp(xs)([]);;

(** iterate through all elements in generic list xs and run the test function on each element. If all pass then return true **)
let rec list_forall(xs: 'a list)(test: 'a -> bool): bool =
  (
    match xs with
    | [] -> true
    | x1 :: xs -> (
        test(x1) && list_forall(xs)(test)
      )
  )
;;

(** iterate through all elements in generic list xs and run the test fxn on eahc element. If one passes then return true **)
let rec list_exists(xs: 'a list)(test: 'a -> bool): bool =
  (
    match xs with
    | [] -> false
    | x1 :: xs -> (
        test(x1) || list_exists(xs)(test)
      )
  )
;;

(** iterate through each element of generic list and apply the work function to that element **)
let rec list_foreach(xs: 'a list) (work: 'a -> unit): unit =
  (
    match xs with
    | [] -> ()
    | x1 :: xs -> (
        work(x1); list_foreach(xs)(work)
      )
  )
;;

(** iterate through each element of generic list and apply the work function to that element in reverse order **)
let rec list_rforeach(xs: 'a list) (work: 'a -> unit): unit =
  list_foreach(list_reverse(xs))(work)
;;

(** the forall_to_foreach function takes a forall function and converts it into a foreach function that applies a
    given action to each element in the collection while ensuring that all elements are processed **)
let forall_to_foreach(forall: ('xs, 'x0) forall): ('xs, 'x0) foreach =
  fun(xs)(work) -> let _ = forall(xs)(fun(x0) -> (work(x0); true)) in ()
;;

(** **)
let foreach_to_forall(foreach: ('xs, 'x0) foreach): ('xs, 'x0) forall =
  fun(xs)(test) ->
    try
      let() = foreach(xs)(fun(x0) -> if test(x0) then () else raise False)
    in( true ) with False(*void*) -> (false)
;;(* end of [foreach_to_forall]: let *)

(** **)
let foreach_to_foldleft(foreach: ('xs, 'x0) foreach): 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0 =
  fun(xs)(r0)(fopr) ->
    let res = ref(r0) in
      foreach(xs)(fun(x0) -> res := fopr(!res)(x0));
    !res
;;(* end of [foreach_to_foldleft]: let *)

(** **)
let rec
foreach_to_map_list(foreach: ('xs, 'x0) foreach): ('xs, 'x0, 'y0) map_list =
fun(xs)(fopr) ->
list_reverse(foreach_to_map_rlist(foreach)(xs)(fopr))
and
foreach_to_map_rlist(foreach: ('xs, 'x0) foreach): ('xs, 'x0, 'y0) map_rlist =
fun(xs)(fopr) ->
let res = ref([]) in
foreach(xs)(fun(x0) -> res := fopr(x0) :: !res); !res
;;(* end of [foreach_to_map_rlist]: let *)

(** **)
let rec foreach_to_listize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) listize =
  fun(xs) -> foreach_to_map_list(foreach)(xs)(fun x -> x)
;;

(** **)
let rec foreach_to_rlistize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) rlistize =
  fun(xs) -> foreach_to_map_rlist(foreach)(xs)(fun x -> x)
;;

(** **)
let rec foreach_to_arrnize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) arrnize =
  (
    fun xs -> list_arrnize(foreach_to_listize(foreach)(xs))
  )
;;

(** **)
let rec foreach_to_rarrnize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) rarrnize =
  (
    fun xs -> list_arrnize(foreach_to_rlistize(foreach)(xs))
  )
;;

(** **)
let rec foreach_to_length(foreach: ('xs, 'x0) foreach): 'xs -> int =
  foldleft_to_length(foreach_to_foldleft(foreach))
  and
  foldleft_to_length(foldleft: ('xs,'x0,'r0) foldleft): 'xs -> int =
  (
    fun(xs) -> foldleft(xs)(0)(fun(r0)(x0) -> r0+1)
  )
;;

(** **)
let rforeach_to_foldright(rforeach: ('xs, 'x0) rforeach): ('xs, 'x0, 'r0) foldright =
  fun(xs)(r0)(fopr) ->
    let res = ref(r0) in
      rforeach(xs) (fun(x0) -> res := fopr(x0)(!res));
    !res
;;(* end of [rforeach_to_foldright]: let *)

(** **)
let int1_forall(n0) =
  foreach_to_forall(int1_foreach)(n0)
;;

(** **)
let string_forall(cs) =
  foreach_to_forall(string_foreach)(cs)
;;

(** **)
let int1_listize(n0) =
  foreach_to_listize(int1_foreach)(n0)
;;
let int1_rlistize(n0) =
  foreach_to_rlistize(int1_foreach)(n0)
;;

(** **)
let string_listize(cs) =
  foreach_to_listize(string_foreach)(cs)
;;

(** **)
let string_rlistize(cs) =
  foreach_to_rlistize(string_foreach)(cs)
;;

(** **)
let int1_foldleft(n0) =
  foreach_to_foldleft(int1_foreach)(n0)
;;

(** **)
let list_foldleft(xs) =
  foreach_to_foldleft(list_foreach)(xs)
;;

(** **)
let string_foldleft(cs) =
  foreach_to_foldleft(string_foreach)(cs)
;;

(** **)
let int1_foldright(n0) =
  rforeach_to_foldright(int1_rforeach)(n0)
;;

(** **)
let list_foldright(xs) =
  rforeach_to_foldright(list_rforeach)(xs)
;;

(** **)
let string_foldright(cs) =
  rforeach_to_foldright(string_rforeach)(cs)
;;
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

let is_int (s : string) : bool =
   try
     let _ = int_of_string s in
     true
   with
   | Failure _ -> false
 ;;
type com =
  | Push of const
  | Pop
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt


  let is_digit = function '0' .. '9' -> true | _ -> false

  let integer : int parser =
   (let* _ = char '-' in
   let* x = natural in pure  (-x))
   <|>
     (let* x = natural in pure x)
  
  let push_parser : com parser =
    ws >>
    (literal "Push") >>
    ws >>
    ( (integer >>= fun n -> char ';' >> pure (Push (D n)))
    <|> (literal "True;" >> pure (Push (B true)))
    <|> (literal "False;" >> pure (Push (B false)))
    <|> (literal "Unit;" >> pure (Push Unit))
    )
let pop_parser : com parser =
   ws >>
  (literal "Pop;") >>
  pure Pop 

let trace_parser : com parser =
   ws >>
  (literal "Trace;") >>
  pure Trace

let add_parser : com parser =
   ws >>
  (literal "Add;") >>
  pure Add

let sub_parser : com parser =
   ws >>
  (literal "Sub;") >>
  pure Sub

let mul_parser : com parser =
   ws >>
  (literal "Mul;") >>
  pure Mul

let div_parser : com parser =
   ws >>

  (literal "Div;") >>
  pure Div

let and_parser : com parser =
   ws >>
  (literal "And;") >>
  
  pure And

let or_parser : com parser =
   ws >>
  (literal "Or;") >>
  
  pure Or

let not_parser : com parser =
   ws >>
  (literal "Not;") >>
 
  pure Not

let lt_parser : com parser =
   ws >>
  (literal "Lt;") >>
  pure Lt

let gt_parser : com parser =
   ws >>
  (literal "Gt;") >>
  pure Gt

 

  let command_list_parser : com list parser =
   many (push_parser <|> pop_parser <|> trace_parser <|> add_parser <|> sub_parser
         <|> mul_parser <|> div_parser <|> and_parser <|> or_parser <|> not_parser
         <|> lt_parser <|> gt_parser) 

let parse_commands (s : string) : com list option =
   match parse command_list_parser s with
      | Some (commands, []) -> Some commands
      | _ -> None

let execute_push (stack : stack) (item : const) (trace : string list) : (stack * string list )=
match item with
| D n -> (D n :: stack, trace)
| B bool -> (B bool :: stack, trace)
|Unit -> (Unit:: stack, trace)



         

let pop_stack (s : stack) (trace: string list): (stack * string list) =
  match s with
  | hd :: rest -> (rest, trace)
  | _ ->  s, "Panic":: trace


let toString (c : const) : string =
   match c with
   | D n -> string_of_int n
   | B true -> "True"
   | B false -> "False"
   | Unit -> "Unit"

  let trace_stack (stack : stack) (trace : string list) : (stack * string list ) =
   match stack with
   | [] -> (stack, "Panic"::trace)
   | c :: rest -> (Unit :: rest, toString c:: trace)

   let add_stack (stack : stack) (trace : string list) : (stack * string list ) =
      match stack with
      | D i :: D j :: rest ->  (D (i + j) :: rest, trace)
      | D _ :: _ :: _ :: _ ->  (stack, "panic" :: trace) (* AddError1: Stack has more than 2 elements *)
      | [_] ->  (stack, "panic" :: trace) (* AddError3: Stack has only 1 element *)
      | _ -> (stack, "panic" :: trace) (* AddError2: Stack is empty *)
    

      let sub_stack (stack : stack) (trace : string list) : (stack * string list ) =
         match stack with
         | D i :: D j :: rest ->  (D (i - j) :: rest, trace)
         | D _ :: _ :: _ :: _ ->  (stack, "panic" :: trace) (* SubError1: Stack has more than 2 elements *)
         | [_] ->  (stack, "panic" :: trace) (* SubError3: Stack has only 1 element *)
         | _ ->  (stack, "panic" :: trace) (* SubError2: Stack is empty *)
       

         let mul_stack (stack : stack) (trace : string list) : (stack * string list ) =
            match stack with
            | D i :: D j :: rest -> (D (i * j) :: rest, trace)
            | D _ :: _ :: _ ->  (stack, "panic" :: trace) (* MulError1: Stack has more than 2 elements *)
            | [_] ->  (stack, "panic" :: trace) (* MulError3: Stack has only 1 element *)
            | _ ->  (stack, "panic" :: trace) (* MulError2: Stack is empty *)
          

  let div_stack (stack : stack) (trace : string list) : (stack * string list ) =
   match stack with
   | D i :: D 0 :: rest ->  (stack, "panic" :: trace)
   | D i :: D j :: rest ->
     if i mod j <> 0 then  (stack, "panic" :: trace) (* DivError1: i is not divisible by j *)
     else  ((D (i / j) :: rest, trace))
     | _ :: _ :: _ ->  (stack, "panic" :: trace) (* DivError2: Stack has more than 2 elements *)
   | [_] ->  (stack, "panic" :: trace) (* DivError3: Stack has only 1 element *)
   | _ ->  (stack, "panic" :: trace) (* Default case for unexpected scenarios *)

   let and_stack (stack : stack) (trace : string list) : (stack * string list ) =
      match stack with
      | B a :: B b :: rest ->  (B (a && b) :: rest, trace)
      |  _ :: _ :: _ ->  (stack, "panic" :: trace) (* AndError1: Stack has more than 2 elements *)
      | [_] ->  (stack, "panic" :: trace) (* AndError3: Stack has only 1 element *)
      | _ ->  (stack, "panic" :: trace) (* AndError2: Stack is empty *)
    
      let or_stack (stack : stack) (trace : string list) : (stack * string list ) =
         match stack with
         | B a :: B b :: rest ->  (B (a || b) :: rest, trace)
         |  _ :: _ :: _ ->  (stack, "panic" :: trace) (* OrError1: Stack has more than 2 elements *)
         | [_] ->  (stack, "panic" :: trace) (* OrError3: Stack has only 1 element *)
         | _ ->  (stack, "panic" :: trace) (* OrError2: Stack is empty *)

         let not_stack (stack : stack) (trace : string list) : (stack * string list ) =
            match stack with
            | B a :: rest ->  (B (not a) :: rest, trace)
            | _ :: _ :: _ ->  (stack, "panic" :: trace) (* NotError1: Stack has more than 1 element *)
            | [_] ->  (stack, "panic" :: trace) (* NotError2: Stack is empty *)
            | _ ->  (stack, "panic" :: trace) (* NotError1: Top element is not a boolean *)

            let lt_stack (stack : stack) (trace : string list) : (stack * string list ) =
               match stack with
               | D i :: D j :: rest ->  (B (i < j) :: rest, trace)
               | _ :: _ :: _ ->  (stack, "panic" :: trace) (* LtError1: Stack has more than 2 elements *)
               | [_] ->  (stack, "panic" :: trace) (* LtError3: Stack has only 1 element *)
               | _ ->  (stack, "panic" :: trace) (* LtError1: Top elements are not integers *)
             
               let gt_stack (stack : stack) (trace : string list) : (stack * string list ) =
                  match stack with
                  | D i :: D j :: rest ->  (B (i > j) :: rest, [])
                  | _ :: _ :: _ ->  (stack, "Panic"::trace) (* GtError1: Stack has more than 2 elements *)
                  | [_] ->  (stack, "Panic"::trace) (* GtError3: Stack has only 1 element *)
                  | _ ->  (stack, "Panic"::trace) (* GtError1: Top elements are not integers *)

                  

                  let rec run_program (commands : com list) (stack : stack) (trace : string list) : (stack * string list) =
                     match commands with
                     | [] -> (stack, trace) (* Base case: No more commands to execute *)
                     | cmd :: rest ->

                       match cmd with
                       | Push c -> let (newstack, newtrace) = execute_push stack c trace in 
                       run_program rest newstack newtrace (* Push command *)


                       | Pop -> let (newstack, newtrace) = pop_stack stack trace in 
                       run_program rest newstack newtrace (* Pop command *)
                       | Trace ->
                        let (newstack, newtrace) = trace_stack stack trace in 
                        run_program rest newstack newtrace (* Trace command *)
                       | Add ->
                        let (newstack, newtrace) = add_stack stack  trace in 
                        run_program rest newstack newtrace (* Add command *)
                         
                       | Sub ->
                         let (newstack, newtrace) = sub_stack stack  trace in 
                       run_program rest newstack newtrace

                       | Mul -> let (newstack, newtrace) = mul_stack stack  trace in 
                       run_program rest newstack newtrace
                         
                       | Div -> let (newstack, newtrace) = div_stack stack  trace in 
                       run_program rest newstack newtrace
                       | And -> let (newstack, newtrace) = and_stack stack  trace in 
                       run_program rest newstack newtrace
                       | Or -> let (newstack, newtrace) = or_stack stack trace in 
                       run_program rest newstack newtrace
                       | Not -> let (newstack, newtrace) = not_stack stack trace in 
                       run_program rest newstack newtrace
                       | Lt -> let (newstack, newtrace) = lt_stack stack trace in 
                       run_program rest newstack newtrace
                       | Gt -> let (newstack, newtrace) = gt_stack stack trace in 
                       run_program rest newstack newtrace
                   
                   
 let list_map(xs) = foreach_to_map_list(list_foreach)(xs)


                  
let interp (s : string) : string list option =

   match parse_commands s with
   | Some commands ->
     (match run_program commands [] [] with
     | (final_stack, finaltrace) ->  Some (finaltrace)
     )
   |_ -> None



      
    






