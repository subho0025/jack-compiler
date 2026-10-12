(*Symbol Table*)

module StringMap = Map.Make (String)

type kind = Static | Field | Arg | Local

type entry = { kind  : kind;
               typ   : string;
               index : int;
             }

type t = entry StringMap.t

let empty : t = StringMap.empty

(*Function to count the variables of the given kind, it is also the index of the next variable of that kind*)
let count (kind: kind) (table: t) : int =
  StringMap.fold (fun _ e n -> if e.kind = kind then n + 1 else n) table 0

(*Function to add a variable, its index is the next free index of its kind*)
let add (name: string) (typ: string) (kind: kind) (table: t) : t =
  StringMap.add name { kind; typ; index = count kind table } table

let contains = StringMap.mem

let find = StringMap.find_opt
