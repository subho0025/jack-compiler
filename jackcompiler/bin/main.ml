(*
   Main file for the jack project - Syntax Analyzer and Compiler
*)
open Jackcompiler

(*Helper function to check if the given file has extension ".jack" *)
let check_jack_file (filename: string) : bool =
  Filename.check_suffix filename ".jack"

(*This function reads the whole file and gives its list of tokens *)
let tokenize_file (filepath: string) : Jackast.Token.t list =
  let in_chan = In_channel.open_text filepath in
  let program = In_channel.input_all in_chan in
  let tokens = Tokenizer.tokenize program in
  In_channel.close in_chan;
  tokens

(*
   This function gives the list of jack files in a directory to compile
*)
let get_files (path: string) : string list =
  if not (Sys.file_exists path) then failwith ("No such file or directory: " ^ path)
  else if Sys.is_directory path then
    match Sys.readdir path |> Array.to_list |> List.filter check_jack_file |> List.sort compare with
    | [] -> failwith ("No .jack files in the directory " ^ path)
    | files -> List.map (Filename.concat path) files
  else if check_jack_file path then [path]
  else failwith "Expected a file with .jack extension or a directory"

(*This function writes the given lines line by line into the output file*)
let write_lines (filepath : string) (lines: string list) : bool =
  let out_chan = Out_channel.open_text filepath in
  let rec write = function
  | [] -> ()
  | h :: t-> Out_channel.output_string out_chan (h ^ "\n");
    write t
  in
  write lines;
  Out_channel.close out_chan;
  true

(*This function tokenizes and parses a jack file *)
let parse_file (file: string) : Jackast.Token.t list * Jackast.Class.t =
  try
    let tokens = tokenize_file file in
    (tokens, Parser.parse_program tokens)
  with Failure msg -> failwith (file ^ ": " ^ msg)

(*
   This function analyzes one jack file
   Name.jack -> out/NameT.xml (the tokens) and out/Name.xml (the parse tree)
*)
let analyze (file: string) : unit =
  let out_dir = Filename.concat (Filename.dirname file) "out" in
  if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
  let name = Filename.concat out_dir (Filename.basename file |> Filename.remove_extension) in
  let tokens, program = parse_file file in
  Printf.printf "Analyzing %s -> %sT.xml, %s.xml\n%!" file name name;
  if write_lines (name ^ "T.xml") (Xml.encode_tokens tokens)
  && write_lines (name ^ ".xml") (Xml.encode_class program) then ()
  else Printf.eprintf "Error in writing%!"

(*
   This function compiles one jack file
   Name.jack -> Name.vm
*)
let compile (file: string) : unit =
  let out_file = Filename.remove_extension file ^ ".vm" in
  let _, program = parse_file file in
  let vm_codes = try Vm.encode_class program with Failure msg -> failwith (file ^ ": " ^ msg) in
  Printf.printf "Compiling %s -> %s\n%!" file out_file;
  if write_lines out_file vm_codes then ()
  else Printf.eprintf "Error in writing%!"

(*This function runs the given function (compile or analyze) on every jack file of the path *)
let run (f: string -> unit) (path: string) : unit =
  try List.iter f (get_files path) with
  | Failure msg | Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1


(*
   Main Function
   jackcompiler {path}      -> compiles to .vm , then jackvm gives .asm and .hack
   jackcompiler -xml {path} -> writes the xml files 
*)
let () =
  (*Checks for correct number of input files*)
  match Array.length Sys.argv with
  | 2 ->
    let in_path = Sys.argv.(1) in
    run compile in_path;
    Printf.printf "Done compiling.\n%!";
 
    (*After compiling the jack codes to vm codes, the vm codes are translated to asm and hack using the jackvm project*)
    (*Name.jack -> Name.vm is given to jackvm, a directory is given as it is (with all its .vm files)*)
    (*jackvm is run from its own project dir, so the path is made absolute*)
    let vm_path = if Sys.is_directory in_path then in_path else Filename.remove_extension in_path ^ ".vm" in
    (match Sys.command (Printf.sprintf "dune exec jackvm -- %s" (Filename.quote vm_path)) with
     | 0 -> ()
     | _ -> Printf.eprintf "Error: translating %s failed\n" vm_path; exit 1)
 
  | 3 when Sys.argv.(1) = "-xml" ->
    run analyze Sys.argv.(2);
    Printf.printf "Done analyzing.\n%!"
 
  | _ -> Printf.eprintf "Error filename";
    exit 1
 
