(*
   Main file for the jack project - Syntax Analyzer
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
   This function gives the list of jack files in a directory to analyze
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

(*
   This function analyzes one jack file
   Name.jack -> out/NameT.xml (the tokens) and out/Name.xml (the parse tree)
*)
let analyze (file: string) : unit =
  let out_dir = Filename.concat (Filename.dirname file) "out" in
  if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
  let name = Filename.concat out_dir (Filename.basename file |> Filename.remove_extension) in
  let tokens = try tokenize_file file with Failure msg -> failwith (file ^ ": " ^ msg) in
  let program = try Parser.parse_program tokens with Failure msg -> failwith (file ^ ": " ^ msg) in
  Printf.printf "Analyzing %s -> %sT.xml, %s.xml\n%!" file name name;
  if write_lines (name ^ "T.xml") (Xml.encode_tokens tokens)
  && write_lines (name ^ ".xml") (Xml.encode_class program) then ()
  else Printf.eprintf "Error in writing%!"

(*
   Main Function
*)
let () =
  (*Checks for correct number of input files*)
  match Array.length Sys.argv with
  | 2 ->
    let in_path = Sys.argv.(1) in
    (try List.iter analyze (get_files in_path) with
     | Failure msg | Sys_error msg ->
       Printf.eprintf "Error: %s\n" msg;
       exit 1);
    Printf.printf "Done analyzing.\n%!"

  | _ -> Printf.eprintf "Error filename";
    exit 1

