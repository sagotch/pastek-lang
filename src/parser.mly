%{
  open AST
  open Str

  let rec add_to_list li (ord, dep, txt) =
    let ord', li = li in
    let li = List.rev li in
    if dep = 1
    then if ord <> ord'
         then failwith "List error"
         else ord', List.rev (Item(txt, None) :: li)
    else let Item(txt', child) = (List.hd li) in
         let item =  match child with
           | None ->
              if dep <> 2 || ord <> ord'
              then failwith "List error"
              else Item(txt', Some (ord, [Item(txt, None)]))
           | Some child ->
              Item(txt', Some (add_to_list child (ord, (dep - 1), txt)))
         in ord', List.rev @@ item :: (List.tl li)

  let mk_list l =
    let (ord, _, txt) = List.hd l in
    let first = ord, [Item(txt, None)] in
    List(List.fold_left add_to_list first (List.tl l))
%}

%token<int> TITLE OITEM UITEM
%token<string> PLAIN INLINE_CODE CODE_BLOCK INLINE_SOURCE SOURCE_BLOCK
%token<char> SUP SUB
%token BOLD ITALIC UNDERLINE STRIKE EMPTYLINE MATH MATH_BLOCK
%token EOF

%start <AST.document> document

%%

document:
| EMPTYLINE* block_list { $2 }

(*** BLOCKS ***)

block_list:
| bl=header | bl=paragraph | bl=code_block | bl=source_block | bl=math_block
| bl=eof | bl=list_t { bl }

eof:
| EOF { [] }

(* NB: with this configuration, headers have to be separated from
 paragraphs with an empty line.
 NB: This implentation add the Str library dependancy *)
header:
| TITLE inline(regular)+ header_f
  { let l = List.rev $2 in
    let h = List.hd l and q = List.tl l in
    match h with
    | Plain s ->
       (try let i =
              search_forward (regexp (" *" ^ String.make $1 '=' ^ " *$")) s 0 
            in Title ($1, List.rev (Plain (String.sub s 0 i) :: q)) :: $3
        with Not_found -> Title ($1, $2) :: $3)
    | _ -> Title ($1, $2) :: $3 }

header_f:
| EMPTYLINE+ hf=paragraph { hf }
| EMPTYLINE* hf=header | EMPTYLINE* hf=code_block | EMPTYLINE* hf=source_block
| EMPTYLINE* hf=eof { hf }

list_t:
| item_t+ list_t_f { mk_list $1 :: $2 }

item_t:
| OITEM inline(regular)* { (true, $1, $2) }
| UITEM inline(regular)* { (false, $1, $2) }

list_t_f:
| lf=header | lf=code_block | lf=math_block | lf=source_block | lf=eof { lf }
| EMPTYLINE+ lf=block_list { lf }

paragraph:
| inline(regular)+ paragraph_f { Paragraph $1 :: $2 }

paragraph_f:
| EMPTYLINE* pf=header | EMPTYLINE* pf=eof | EMPTYLINE* pf=code_block
| EMPTYLINE* pf=source_block { pf }
| EMPTYLINE+ paragraph { $2 }

code_block:
| CODE_BLOCK code_block_f { CodeBlock $1 :: $2 }

code_block_f:
| EMPTYLINE* block_list { $2 }

source_block:
| SOURCE_BLOCK source_block_f { SourceBlock $1 :: $2 }

source_block_f:
| EMPTYLINE* block_list { $2 }

math_block:
| MATH_BLOCK inline(math)+ MATH_BLOCK math_block_f { MathBlock $2 :: $4 }

math_block_f:
| EMPTYLINE* block_list { $2 }

(*** INLINE ELEMENTS ***)

regular:
| BOLD inline(bold)+ BOLD { Bold $2 }
| ITALIC inline(italic)+ ITALIC { Italic $2 }
| UNDERLINE inline(underline)+ UNDERLINE { Underline $2 }
| STRIKE inline(strike)+ STRIKE { Strike $2 }
| INLINE_CODE { InlineCode $1 }
| MATH inline(math)+ MATH { InlineMath $2 }

bold:
| ITALIC inline(italic)+ ITALIC { Italic $2 }
| UNDERLINE inline(underline)+ UNDERLINE { Underline $2 }
| STRIKE inline(strike)+ STRIKE { Strike $2 }

italic:
| BOLD inline(bold)+ BOLD { Bold $2 }
| UNDERLINE inline(underline)+ UNDERLINE { Underline $2 }
| STRIKE inline(strike)+ STRIKE { Strike $2 }

underline:
| BOLD inline(bold)+ BOLD { Bold $2 }
| ITALIC inline(italic)+ ITALIC { Italic $2 }
| STRIKE inline(strike)+ STRIKE { Strike $2 }

strike:
| BOLD inline(bold)+ BOLD { Bold $2 }
| ITALIC inline(italic)+ ITALIC { Italic $2 }
| UNDERLINE inline(underline)+ UNDERLINE { Underline $2 }

math: error { failwith "TODO: raise Parser.Error" }

inline(param):
| PLAIN { Plain $1 }
| SUB { Sub $1 }
| SUP { Sup $1 }
| INLINE_SOURCE { InlineSource $1 }
| param { $1 }
