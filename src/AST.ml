type document = block list

 and block =
   | Title of int * inline list
   | Paragraph of inline list
   | CodeBlock of string
   | SourceBlock of string

 and inline =
   | Plain of string
   | InlineCode of string
   | InlineSource of string
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
  | Sup c -> "Sup " ^ String.make 1 c
  | Sub c -> "Sub " ^ String.make 1 c
