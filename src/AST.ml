type document = block list

 and block =
   | Title of int * inline list
   | Paragraph of inline list
   | MathBlock of inline list
   | Table of inline list list option * inline list list list
   | List of list_t
   | CodeBlock of string
   | SourceBlock of string

 and list_t = bool * item_t list

 and item_t = Item of inline list * list_t option

 and inline =
   | Plain of string
   | InlineCode of string
   | InlineSource of string
   | InlineMath of inline list
   | Bold of inline list
   | Italic of inline list
   | Underline of inline list
   | Strike of inline list
   | Sup of char
   | Sub of char

let rec string_of_document doc =
  String.concat "\n" @@ List.map string_of_block doc

and string_of_block = function
  | Title(i, c) -> "Title("  ^ string_of_int i ^ ", "
                   ^ string_of_inlines c ^ ")"
  | Paragraph p -> "Paragraph(" ^ string_of_inlines p ^ ")"
  | CodeBlock txt -> "CodeBlock \"" ^ txt ^ "\""
  | SourceBlock txt -> "SourceBlock \"" ^ txt ^ "\""
  | MathBlock l -> "CodeBlock \"" ^ string_of_inlines l ^ "\""
  | Table (head, cont) -> "Table(" ^ string_of_table (head, cont) ^ ")"
  | List l -> string_of_list l

and string_of_table (head, cont) =
  (match head with
   | None -> ""
   | Some h -> "Head(" ^ string_of_tline h ^ "), ")
  ^ "Content([" ^ string_of_tlines cont ^ "])"

and string_of_tline line = 
  "[" ^ String.concat ";" (List.map string_of_inlines line) ^ "]"

and string_of_tlines cont =
  "[" ^ String.concat ";" (List.map string_of_tline cont) ^ "]"
                                
and string_of_list = function
  | true, l -> "OList(" ^ string_of_items l ^ ")"
  | false, l -> "UList(" ^ string_of_items l ^ ")"

and string_of_items l =
  "[" ^ String.concat ";" (List.map string_of_item l) ^ "]"

and string_of_item = function
  | Item(txt, None) -> string_of_inlines txt
  | Item(txt, Some sub) -> string_of_inlines txt ^ string_of_list sub

and string_of_inlines l = 
  "[" ^ String.concat ";" (List.map string_of_inline l) ^ "]"

and string_of_inline = function
  | Plain txt -> "Plain \"" ^ txt ^ "\""
  | InlineCode txt -> "InlineCode \"" ^ txt ^ "\""
  | InlineSource txt -> "InlineSource \"" ^ txt ^ "\""
  | Bold b -> "Bold (" ^ string_of_inlines b ^ ")"
  | Italic i -> "Italic (" ^ string_of_inlines i ^ ")"
  | Underline u -> "Underline (" ^ string_of_inlines u ^ ")"
  | Strike s -> "Strike (" ^ string_of_inlines s ^ ")"
  | InlineMath m -> "Math (" ^ string_of_inlines m ^ ")"
  | Sup c -> "Sup " ^ String.make 1 c
  | Sub c -> "Sub " ^ String.make 1 c
