(*
   This is the encoding file of the compiler, it encodes the AST of a class to vm commands
   Every function gives a list of vm commands
*)
open Jackast

(*
   The environment of a subroutine
   the class name, the symbol table of the class and the symbol table of the subroutine
*)
type env = { class_name  : string;
             class_table : SymbolTable.t;
             sub_table   : SymbolTable.t;
           }

(*Function to find a variable, first in the subroutine table and then in the class table*)
let lookup (env: env) (name: string) : SymbolTable.entry option =
  match SymbolTable.find name env.sub_table with
  | Some e -> Some e
  | None -> SymbolTable.find name env.class_table

(*Helper function to get the vm segment of a kind of variable*)
let segment (kind: SymbolTable.kind) : string =
  match kind with
  | SymbolTable.Static -> "static"
  | SymbolTable.Field -> "this"
  | SymbolTable.Arg -> "argument"
  | SymbolTable.Local -> "local"

(*Helper function to get the "segment index" of a variable, ex=> a local x -> "local 0"*)
let var (env: env) (name: string) : string =
  match lookup env name with
  | Some e -> Printf.sprintf "%s %d" (segment e.kind) e.index
  | None -> failwith (Printf.sprintf "Error variable: %s is not defined in class %s" name env.class_name)

(*Helper function to get the vm command of a binary operator, * and / are calls to the OS*)
let encode_op (op: char) : string =
  match op with
  | '+' -> "add"
  | '-' -> "sub"
  | '*' -> "call Math.multiply 2"
  | '/' -> "call Math.divide 2"
  | '&' -> "and"
  | '|' -> "or"
  | '<' -> "lt"
  | '>' -> "gt"
  | '=' -> "eq"
  | c -> failwith (Printf.sprintf "Error operator: %c" c)

(*
   An expression is encoded as its first term, then every (op, term) as the term followed by the op
   ex=> x + 1 -> push x, push constant 1, add
*)
let rec encode_expression (env: env) ((t, ops): Expr.t) : string list =
  encode_term env t @ List.concat_map (fun (op, t) -> encode_term env t @ [encode_op op]) ops

and encode_term (env: env) (t: Expr.term) : string list =
  match t with
  | Expr.IntConst n -> ["push constant " ^ string_of_int n]
  | Expr.StringConst s ->
    let append c = ["push constant " ^ string_of_int (Char.code c); "call String.appendChar 2"] in
    ["push constant " ^ string_of_int (String.length s); "call String.new 1"]
    @ List.concat_map append (List.of_seq (String.to_seq s))
  | Expr.KeywordConst "true" -> ["push constant 0"; "not"]
  | Expr.KeywordConst "this" -> ["push pointer 0"]
  | Expr.KeywordConst _ -> ["push constant 0"]
  | Expr.Var name -> ["push " ^ var env name]
  | Expr.Index (name, e) ->
    ("push " ^ var env name) :: encode_expression env e @ ["add"; "pop pointer 1"; "push that 0"]
  | Expr.Call c -> encode_call env c
  | Expr.Paren e -> encode_expression env e
  | Expr.Unary ('-', t) -> encode_term env t @ ["neg"]
  | Expr.Unary (_, t) -> encode_term env t @ ["not"]

(*
   A subroutine call is one of
   f(...)       -> a method of this class, this object is pushed as the first argument
   obj.f(...)   -> a method of the class of obj, obj is pushed as the first argument
   Class.f(...) -> a function or a constructor, no object is pushed
*)
and encode_call (env: env) ({ target; name; args }: Expr.call) : string list =
  let args_codes = List.concat_map (encode_expression env) args in
  let n = List.length args in
  let call cls nargs = [Printf.sprintf "call %s.%s %d" cls name nargs] in
  match target with
  | None -> "push pointer 0" :: args_codes @ call env.class_name (n + 1)
  | Some t ->
    match lookup env t with
    | Some e -> ("push " ^ var env t) :: args_codes @ call e.typ (n + 1)
    | None -> args_codes @ call t n

let rec encode_statements (env: env) (c: int) (ss: Statement.t list) : string list * int =
  match ss with
  | [] -> ([], c)
  | s :: rest ->
    let codes, c = encode_statement env c s in
    let rest_codes, c = encode_statements env c rest in
    (codes @ rest_codes, c)

and encode_statement (env: env) (c: int) (s: Statement.t) : string list * int =
  let label name = Printf.sprintf "%s%d" name c in
  match s with
  | Statement.Let (name, None, e) -> (encode_expression env e @ ["pop " ^ var env name], c)
  | Statement.Let (name, Some index, e) ->
    (("push " ^ var env name) :: encode_expression env index @ ["add"]
     @ encode_expression env e @ ["pop temp 0"; "pop pointer 1"; "push temp 0"; "pop that 0"], c)
  | Statement.If (cond, then_, None) ->
    let then_, next = encode_statements env (c + 1) then_ in
    (encode_expression env cond @ ["not"; "if-goto " ^ label "IF_END"]
     @ then_ @ ["label " ^ label "IF_END"], next)
  | Statement.If (cond, then_, Some else_) ->
    let then_, next = encode_statements env (c + 1) then_ in
    let else_, next = encode_statements env next else_ in
    (encode_expression env cond @ ["not"; "if-goto " ^ label "IF_ELSE"]
     @ then_ @ ["goto " ^ label "IF_END"; "label " ^ label "IF_ELSE"]
     @ else_ @ ["label " ^ label "IF_END"], next)
  | Statement.While (cond, body) ->
    let body, next = encode_statements env (c + 1) body in
    (["label " ^ label "WHILE_EXP"] @ encode_expression env cond
     @ ["not"; "if-goto " ^ label "WHILE_END"]
     @ body @ ["goto " ^ label "WHILE_EXP"; "label " ^ label "WHILE_END"], next)
  | Statement.Do call -> (encode_call env call @ ["pop temp 0"], c)
  | Statement.Return None -> (["push constant 0"; "return"], c)
  | Statement.Return (Some e) -> (encode_expression env e @ ["return"], c)

(*Helper function to add a list of (type, name) of the same kind to a symbol table*)
let rec add_vars (kind: SymbolTable.kind) (table: SymbolTable.t) (vars: (string * string) list) : SymbolTable.t =
  match vars with
  | [] -> table
  | (t, name) :: rest -> add_vars kind (SymbolTable.add name t kind table) rest

(*Helper function to turn declarations (type, names) into a list of (type, name)*)
let flatten (decs: (string * string list) list) : (string * string) list =
  List.concat_map (fun (t, names) -> List.map (fun name -> (t, name)) names) decs

(*
   This function encodes a subroutine
   function Class.name nLocals, then the setup of this, then the body
   - a constructor allocates memory for the fields and sets this to it
   - a method gets this object as argument 0 and sets this to it
*)
let encode_subroutine (class_name: string) (class_table: SymbolTable.t) (s: Class.subroutine) : string list =
  let table = match s.sub_kind with
    | Class.Method -> SymbolTable.add "this" class_name SymbolTable.Arg SymbolTable.empty
    | _ -> SymbolTable.empty
  in
  let table = add_vars SymbolTable.Arg table s.params in
  let table = add_vars SymbolTable.Local table (flatten s.locals) in
  let env = { class_name; class_table; sub_table = table } in
  let setup = match s.sub_kind with
    | Class.Constructor ->
      let fields = SymbolTable.count SymbolTable.Field class_table in
      ["push constant " ^ string_of_int fields; "call Memory.alloc 1"; "pop pointer 0"]
    | Class.Method -> ["push argument 0"; "pop pointer 0"]
    | Class.Function -> []
  in
  let body, _ = encode_statements env 0 s.body in
  Printf.sprintf "function %s.%s %d" class_name s.sub_name (SymbolTable.count SymbolTable.Local table)
  :: setup @ body

(*Main function to encode the AST of a class, gives the lines of Name.vm *)
let encode_class (c: Class.t) : string list =
  let kind = function
    | Class.Static -> SymbolTable.Static
    | Class.Field -> SymbolTable.Field
  in
  let class_table =
    List.fold_left (fun table (k, t, names) -> add_vars (kind k) table (List.map (fun n -> (t, n)) names))
      SymbolTable.empty c.class_vars
  in
  List.concat_map (encode_subroutine c.class_name class_table) c.subroutines

