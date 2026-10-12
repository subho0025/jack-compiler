open Jackast

(*Defining the types - digits, letters, symbols, etc.*)
let digits = "1234567890"

let s_alpha = "abcdefghijklmnopqrstuvwxyz"
let c_alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let ident_chars = s_alpha ^ c_alpha ^ "_" ^ digits
let symbols = "{}()[].,;+-*/&|<>=~"
let whitespaces = " \t\r\n"

let keywords = ["class"; "constructor"; "function"; "method"; "field"; "static";
                "var"; "int"; "char"; "boolean"; "void"; "true"; "false"; "null";
                "this"; "let"; "do"; "if"; "else"; "while"; "return"]

(*Helper function to get the index of the first char from i which is not in chars*)
let rec skip_while (program: string) (chars: string) (i: int) : int =
  if i < String.length program && String.contains chars program.[i] then skip_while program chars (i + 1)
  else i

(*Helper function to get the index of the end of a // comment*)
let end_of_line (program: string) (i: int) : int =
  match String.index_from_opt program i '\n' with
  | Some j -> j
  | None -> String.length program

(*Helper function to get the index just after the end of a block comment*)
let rec end_of_comment (program: string) (i: int) : int =
  if i + 1 >= String.length program then failwith "Error comment: no closing */"
  else if program.[i] = '*' && program.[i+1] = '/' then i + 2
  else end_of_comment program (i + 1)

(*Helper function to get the index of the closing '"' of a string constant*)
let end_of_string (program: string) (i: int) : int =
  match String.index_from_opt program i '"' with
  | Some j when not (String.contains (String.sub program i (j - i)) '\n') -> j
  | _ -> failwith "Error string: no closing \" on the same line"

(*This function gives the token of an integer constant, which has to be in 0..32767*)
let parse_int (str: string) : Token.t =
  let n = int_of_string str in
  if n > 32767 then failwith ("Error integer: too large " ^ str)
  else Token.IntConst n

(*This function gives the token of a word, which is either a keyword or an identifier*)
let parse_word (str: string) : Token.t =
  if List.mem str keywords then Token.Keyword str
  else Token.Identifier str

(*
   Main tokenizer function
   It takes the whole program string and gives the list of tokens
   It goes through the string char by char, i is the current index and tokens is the list made so far (in reverse)
   whitespaces and comments (//, /* */ and /** */) are skipped
*)
let tokenize (program: string) : Token.t list =
  let len = String.length program in
  let sub i j = String.sub program i (j - i) in
  let rec next i tokens =
    if i >= len then List.rev tokens
    else
      match program.[i] with
      | c when String.contains whitespaces c -> next (i + 1) tokens
      | '/' when i + 1 < len && program.[i+1] = '/' -> next (end_of_line program i) tokens
      | '/' when i + 1 < len && program.[i+1] = '*' -> next (end_of_comment program (i + 2)) tokens
      | '"' ->
        let j = end_of_string program (i + 1) in
        next (j + 1) (Token.StringConst (sub (i + 1) j) :: tokens)
      | c when String.contains symbols c -> next (i + 1) (Token.Symbol c :: tokens)
      | c when String.contains digits c ->
        let j = skip_while program digits i in
        next j (parse_int (sub i j) :: tokens)
      | c when String.contains ident_chars c ->
        let j = skip_while program ident_chars i in
        next j (parse_word (sub i j) :: tokens)
      | c -> failwith (Printf.sprintf "Error char: '%c'" c)
  in
  next 0 []

