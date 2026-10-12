(*
   This is the parser of the jack project
   Every parse function takes a list of tokens and gives the AST and the rest of the tokens
   The grammar rule the function parses is written just above it
*)
open Jackast
open Token

(*Helper function to fail with a parse error at the given tokens*)
let error (expected: string) (tokens: Token.t list) =
  match tokens with
  | t :: _ -> failwith (Printf.sprintf "parse error: expected %s but got '%s'" expected (Token.to_string t))
  | [] -> failwith (Printf.sprintf "parse error: expected %s but reached the end of file" expected)

(*Helper function to consume the given symbol*)
let expect (c: char) (tokens: Token.t list) : Token.t list =
  match tokens with
  | Symbol s :: rest when s = c -> rest
  | _ -> error (Printf.sprintf "'%c'" c) tokens

(*Helper function to consume an identifier*)
let identifier (tokens: Token.t list) : string * Token.t list =
  match tokens with
  | Identifier name :: rest -> (name, rest)
  | _ -> error "an identifier" tokens

let is_op (c: char) : bool = String.contains "+-*/&|<>=" c

(*type: 'int' | 'char' | 'boolean' | className*)
let parse_type (tokens: Token.t list) : string * Token.t list =
  match tokens with
  | Keyword ("int" | "char" | "boolean" as t) :: rest -> (t, rest)
  | Identifier t :: rest -> (t, rest)
  | _ -> error "a type" tokens

(*varName (',' varName)* ';'*)
let rec parse_names (tokens: Token.t list) : string list * Token.t list =
  match tokens with
  | Identifier name :: Symbol ',' :: rest ->
    let names, rest = parse_names rest in
    (name :: names, rest)
  | Identifier name :: Symbol ';' :: rest -> ([name], rest)
  | _ -> error "a variable name followed by ',' or ';'" tokens

(*expression: term (op term)* *)
let rec parse_expression (tokens: Token.t list) : Expr.t * Token.t list =
  let rec parse_ops tokens =
    match tokens with
    | Symbol op :: rest when is_op op ->
      let t, rest = parse_term rest in
      let ops, rest = parse_ops rest in
      ((op, t) :: ops, rest)
    | _ -> ([], tokens)
  in
  let t, rest = parse_term tokens in
  let ops, rest = parse_ops rest in
  ((t, ops), rest)

(*
   term: integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']'
         | subroutineCall | '(' expression ')' | unaryOp term
   varName, varName[...] and a subroutineCall all start with an identifier,
   so the token after it decides which one it is
*)
and parse_term (tokens: Token.t list) : Expr.term * Token.t list =
  match tokens with
  | IntConst n :: rest -> (Expr.IntConst n, rest)
  | StringConst s :: rest -> (Expr.StringConst s, rest)
  | Keyword ("true" | "false" | "null" | "this" as k) :: rest -> (Expr.KeywordConst k, rest)
  | Identifier name :: Symbol '[' :: rest ->
    let e, rest = parse_expression rest in
    (Expr.Index (name, e), expect ']' rest)
  | Identifier _ :: Symbol ('(' | '.') :: _ ->
    let c, rest = parse_call tokens in
    (Expr.Call c, rest)
  | Identifier name :: rest -> (Expr.Var name, rest)
  | Symbol '(' :: rest ->
    let e, rest = parse_expression rest in
    (Expr.Paren e, expect ')' rest)
  | Symbol ('-' | '~' as op) :: rest ->
    let t, rest = parse_term rest in
    (Expr.Unary (op, t), rest)
  | _ -> error "a term" tokens

(*subroutineCall: subroutineName '(' expressionList ')' | (className | varName) '.' subroutineName '(' expressionList ')'*)
and parse_call (tokens: Token.t list) : Expr.call * Token.t list =
  let target, rest =
    match tokens with
    | Identifier t :: Symbol '.' :: rest -> (Some t, rest)
    | _ -> (None, tokens)
  in
  let name, rest = identifier rest in
  let args, rest = parse_expression_list (expect '(' rest) in
  ({ Expr.target; name; args }, expect ')' rest)

(*expressionList: (expression (',' expression)* )?*)
and parse_expression_list (tokens: Token.t list) : Expr.t list * Token.t list =
  match tokens with
  | Symbol ')' :: _ -> ([], tokens)
  | _ ->
    let e, rest = parse_expression tokens in
    match rest with
    | Symbol ',' :: rest ->
      let es, rest = parse_expression_list rest in
      (e :: es, rest)
    | _ -> ([e], rest)

(*Helper function to parse '(' expression ')'*)
let parse_condition (tokens: Token.t list) : Expr.t * Token.t list =
  let e, rest = parse_expression (expect '(' tokens) in
  (e, expect ')' rest)

(*statements: statement*  , it stops at the first token which does not start a statement*)
let rec parse_statements (tokens: Token.t list) : Statement.t list * Token.t list =
  match tokens with
  | Keyword ("let" | "if" | "while" | "do" | "return") :: _ ->
    let s, rest = parse_statement tokens in
    let ss, rest = parse_statements rest in
    (s :: ss, rest)
  | _ -> ([], tokens)

(*Helper function to parse '{' statements '}'*)
and parse_block (tokens: Token.t list) : Statement.t list * Token.t list =
  let ss, rest = parse_statements (expect '{' tokens) in
  (ss, expect '}' rest)

(*statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement*)
and parse_statement (tokens: Token.t list) : Statement.t * Token.t list =
  match tokens with
  (*'let' varName '[' expression ']' '=' expression ';'*)
  | Keyword "let" :: Identifier name :: Symbol '[' :: rest ->
    let index, rest = parse_expression rest in
    let e, rest = parse_expression (expect '=' (expect ']' rest)) in
    (Statement.Let (name, Some index, e), expect ';' rest)
  (*'let' varName '=' expression ';'*)
  | Keyword "let" :: Identifier name :: rest ->
    let e, rest = parse_expression (expect '=' rest) in
    (Statement.Let (name, None, e), expect ';' rest)
  (*'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?*)
  | Keyword "if" :: rest ->
    let cond, rest = parse_condition rest in
    let then_, rest = parse_block rest in
    (match rest with
     | Keyword "else" :: rest ->
       let else_, rest = parse_block rest in
       (Statement.If (cond, then_, Some else_), rest)
     | _ -> (Statement.If (cond, then_, None), rest))
  (*'while' '(' expression ')' '{' statements '}'*)
  | Keyword "while" :: rest ->
    let cond, rest = parse_condition rest in
    let body, rest = parse_block rest in
    (Statement.While (cond, body), rest)
  (*'do' subroutineCall ';'*)
  | Keyword "do" :: rest ->
    let c, rest = parse_call rest in
    (Statement.Do c, expect ';' rest)
  (*'return' expression? ';'*)
  | Keyword "return" :: Symbol ';' :: rest -> (Statement.Return None, rest)
  | Keyword "return" :: rest ->
    let e, rest = parse_expression rest in
    (Statement.Return (Some e), expect ';' rest)
  | _ -> error "a statement" tokens

(*classVarDec: ('static' | 'field') type varName (',' varName)* ';'*)
let rec parse_class_vars (tokens: Token.t list) : Class.var_dec list * Token.t list =
  let kind = function
    | "static" -> Class.Static
    | _ -> Class.Field
  in
  match tokens with
  | Keyword ("static" | "field" as k) :: rest ->
    let t, rest = parse_type rest in
    let names, rest = parse_names rest in
    let vars, rest = parse_class_vars rest in
    ((kind k, t, names) :: vars, rest)
  | _ -> ([], tokens)

(*parameterList: ((type varName) (',' type varName)* )?*)
let rec parse_params (tokens: Token.t list) : (string * string) list * Token.t list =
  match tokens with
  | Symbol ')' :: _ -> ([], tokens)
  | _ ->
    let t, rest = parse_type tokens in
    let name, rest = identifier rest in
    match rest with
    | Symbol ',' :: rest ->
      let params, rest = parse_params rest in
      ((t, name) :: params, rest)
    | _ -> ([(t, name)], rest)

(*varDec: ('var' type varName (',' varName)* ';')* *)
let rec parse_locals (tokens: Token.t list) : (string * string list) list * Token.t list =
  match tokens with
  | Keyword "var" :: rest ->
    let t, rest = parse_type rest in
    let names, rest = parse_names rest in
    let locals, rest = parse_locals rest in
    ((t, names) :: locals, rest)
  | _ -> ([], tokens)

(*
   subroutineDec: ('constructor' | 'function' | 'method') ('void' | type) subroutineName
                  '(' parameterList ')' '{' varDec* statements '}'
*)
let rec parse_subroutines (tokens: Token.t list) : Class.subroutine list * Token.t list =
  let kind = function
    | "constructor" -> Class.Constructor
    | "function" -> Class.Function
    | _ -> Class.Method
  in
  match tokens with
  | Keyword ("constructor" | "function" | "method" as k) :: rest ->
    let return_type, rest =
      match rest with
      | Keyword "void" :: rest -> ("void", rest)
      | _ -> parse_type rest
    in
    let sub_name, rest = identifier rest in
    let params, rest = parse_params (expect '(' rest) in
    let locals, rest = parse_locals (expect '{' (expect ')' rest)) in
    let body, rest = parse_statements rest in
    let sub = { Class.sub_kind = kind k; return_type; sub_name; params; locals; body } in
    let subs, rest = parse_subroutines (expect '}' rest) in
    (sub :: subs, rest)
  | _ -> ([], tokens)

(*
   Main function to parse the whole program
   class: 'class' className '{' classVarDec* subroutineDec* '}'
   Takes the list of tokens of a file and gives the AST of the class
*)
let parse_program (tokens: Token.t list) : Class.t =
  match tokens with
  | Keyword "class" :: Identifier class_name :: Symbol '{' :: rest ->
    let class_vars, rest = parse_class_vars rest in
    let subroutines, rest = parse_subroutines rest in
    (match rest with
     | [Symbol '}'] -> { Class.class_name; class_vars; subroutines }
     | _ -> error "'}' at the end of the class" rest)
  | _ -> error "'class' className '{'" tokens

