(*
   This is the encoding file of the analyzer, it encodes the tokens and the AST to xml
   Every function gives a list of xml lines
   - the tokens xml ( NameT.xml) is a flat list of tokens
   - the parse tree xml (Name.xml) has one tag for every grammar rule, with the tokens as leaves
*)
open Jackast

(*Helper function to escape the xml special characters: <, >, & and "" *)
let escape (str: string) : string =
  let escape_char = function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | c -> String.make 1 c
  in
  String.to_seq str |> List.of_seq |> List.map escape_char |> String.concat ""

(*This function encodes a single token, ex=> <keyword> class </keyword>*)
let encode_token (token: Token.t) : string =
  let leaf tag value = Printf.sprintf "<%s> %s </%s>" tag (escape value) tag in
  match token with
  | Token.Keyword k -> leaf "keyword" k
  | Token.Symbol c -> leaf "symbol" (String.make 1 c)
  | Token.IntConst n -> leaf "integerConstant" (string_of_int n)
  | Token.StringConst s -> leaf "stringConstant" s
  | Token.Identifier i -> leaf "identifier" i

(*Helper function to wrap the lines inside a tag, the lines are indented by 2 spaces*)
let tag (name: string) (lines: string list) : string list =
  ("<" ^ name ^ ">") :: List.map (fun l -> "  " ^ l) lines @ ["</" ^ name ^ ">"]

(*Helper functions for the leaves of the parse tree*)
let kw (k: string) : string = encode_token (Token.Keyword k)
let sym (c: char) : string = encode_token (Token.Symbol c)
let ident (i: string) : string = encode_token (Token.Identifier i)
let typ (t: string) : string =
  if List.mem t ["int"; "char"; "boolean"; "void"] then kw t else ident t

(*Helper function to join the xml of a list of items with ','*)
let rec commas (f: 'a -> string list) (items: 'a list) : string list =
  match items with
  | [] -> []
  | [x] -> f x
  | x :: rest -> f x @ sym ',' :: commas f rest

let rec encode_expression ((t, ops): Expr.t) : string list =
  tag "expression" (encode_term t @ List.concat_map (fun (op, t) -> sym op :: encode_term t) ops)

and encode_term (t: Expr.term) : string list =
  tag "term" (match t with
      | Expr.IntConst n -> [encode_token (Token.IntConst n)]
      | Expr.StringConst s -> [encode_token (Token.StringConst s)]
      | Expr.KeywordConst k -> [kw k]
      | Expr.Var name -> [ident name]
      | Expr.Index (name, e) -> ident name :: sym '[' :: encode_expression e @ [sym ']']
      | Expr.Call c -> encode_call c
      | Expr.Paren e -> sym '(' :: encode_expression e @ [sym ')']
      | Expr.Unary (op, t) -> sym op :: encode_term t)

(*A subroutine call has no tag of its own, only its expressionList has one*)
and encode_call ({ target; name; args }: Expr.call) : string list =
  let target = match target with
    | Some t -> [ident t; sym '.']
    | None -> []
  in
  target @ ident name :: sym '(' :: tag "expressionList" (commas encode_expression args) @ [sym ')']

let rec encode_statements (ss: Statement.t list) : string list =
  tag "statements" (List.concat_map encode_statement ss)

(*Helper function to encode '{' statements '}'*)
and encode_block (ss: Statement.t list) : string list =
  sym '{' :: encode_statements ss @ [sym '}']

and encode_statement (s: Statement.t) : string list =
  match s with
  | Statement.Let (name, index, e) ->
    let index = match index with
      | Some i -> sym '[' :: encode_expression i @ [sym ']']
      | None -> []
    in
    tag "letStatement" (kw "let" :: ident name :: index @ sym '=' :: encode_expression e @ [sym ';'])
  | Statement.If (cond, then_, else_) ->
    let else_ = match else_ with
      | Some ss -> kw "else" :: encode_block ss
      | None -> []
    in
    tag "ifStatement" (kw "if" :: sym '(' :: encode_expression cond @ sym ')' :: encode_block then_ @ else_)
  | Statement.While (cond, body) ->
    tag "whileStatement" (kw "while" :: sym '(' :: encode_expression cond @ sym ')' :: encode_block body)
  | Statement.Do c -> tag "doStatement" (kw "do" :: encode_call c @ [sym ';'])
  | Statement.Return e ->
    let e = match e with
      | Some e -> encode_expression e
      | None -> []
    in
    tag "returnStatement" (kw "return" :: e @ [sym ';'])

let encode_class_var ((kind, t, names): Class.var_dec) : string list =
  let kind = match kind with
    | Class.Static -> "static"
    | Class.Field -> "field"
  in
  tag "classVarDec" (kw kind :: typ t :: commas (fun n -> [ident n]) names @ [sym ';'])

let encode_local ((t, names): string * string list) : string list =
  tag "varDec" (kw "var" :: typ t :: commas (fun n -> [ident n]) names @ [sym ';'])

let encode_subroutine (s: Class.subroutine) : string list =
  let kind = match s.sub_kind with
    | Class.Constructor -> "constructor"
    | Class.Function -> "function"
    | Class.Method -> "method"
  in
  let params = tag "parameterList" (commas (fun (t, n) -> [typ t; ident n]) s.params) in
  let body = tag "subroutineBody" (sym '{' :: List.concat_map encode_local s.locals
                                   @ encode_statements s.body @ [sym '}']) in
  tag "subroutineDec" (kw kind :: typ s.return_type :: ident s.sub_name :: sym '(' :: params @ sym ')' :: body)

(*Main function to encode the tokens of a file, gives the lines of NameT.xml*)
let encode_tokens (tokens: Token.t list) : string list =
  "<tokens>" :: List.map encode_token tokens @ ["</tokens>"]

(*Main function to encode the AST of a class, gives the lines of Name.xml*)
let encode_class (c: Class.t) : string list =
  tag "class" (kw "class" :: ident c.class_name :: sym '{'
               :: List.concat_map encode_class_var c.class_vars
               @ List.concat_map encode_subroutine c.subroutines @ [sym '}'])

