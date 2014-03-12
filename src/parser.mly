%{
  open Type
  open Str
  open Toml

  let config_blocks = ref []
  let links_urls = ref []

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
              if dep <> 2
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
               LINK LINK_URL HTML_ENTITIE COMMENT_BLOCK
               CONFIG
%token<string * string> IMAGE EXT
%token<char> SUP SUB GREEK_LETTER
%token BOLD ITALIC UNDERLINE STRIKE EMPTYLINE MATH MATH_BLOCK
       TBL_HSEP TBL_START TBL_SEP TBL_END
       SUP_START SUP_END SUB_START SUB_END
       LINK_END
%token EOF

%start <TomlType.tomlTable * Type.document> document

%%

document:
| EMPTYLINE* block_list
  { let config = Toml.from_string 
                 @@ String.concat "\n"
                 @@ List.rev !config_blocks in
    let links = Hashtbl.create 0 in
    List.iter (fun (k, v) ->
               Hashtbl.add links k (TomlType.TString v))
              @@ List.rev !links_urls;
    Hashtbl.add config "__pastek_links_urls" (TomlType.TTable links);
    config, $2 }

(*** BLOCKS ***)

block_list:
| bl=header | bl=paragraph | bl=code_block | bl=source_block | bl=math_block
| bl=eof | bl=list_t | bl=table | bl=ext | bl=comment_block 
| bl=link_url | bl=config_block { bl }

eof:
| EOF { [] }

(* NB: with this configuration, headers have to be separated from
 * paragraphs with an empty line.
 * NB: This implentation add the Str library dependency *)
header:
| TITLE inline(regular)* header_f
  { let default () = Title ($1, $2) :: $3 in
    if $2 = []
    then default ()
    else
      let l = List.rev $2 in
      let h = List.hd l and q = List.tl l in
      match h with
      | Plain s ->
         (try let i =
                search_forward (regexp (" *" ^ String.make $1 '=' ^ " *$")) s 0 
              in if i = 0
                 then Title ($1, List.rev q) :: $3
                 else Title ($1, List.rev (Plain (String.sub s 0 i) :: q)) :: $3
          with Not_found -> default ())
      | _ -> default ()}

header_f:
| EMPTYLINE+ hf=block_list { hf }
| hf=header | hf=code_block | hf=source_block | hf=math_block | hf=eof
| hf=table | hf=ext | hf=list_t | hf=link_url | hf=comment_block 
| hf=config_block
  { hf }

table:
| table_line TBL_HSEP table_line+ table_f
  { Table(Some $1, $3) :: $4 }
| table_line+ table_f
  { Table(None, $1) :: $2 }

table_line:
| TBL_START separated_nonempty_list(TBL_SEP, inline(regular)*) TBL_END
  { $2 }

table_f:
| tf=header | tf=paragraph | tf=code_block | tf=source_block | tf=math_block
| tf=eof | tf=list_t | tf=ext | tf=link_url | tf=comment_block
| tf=config_block { tf }
| EMPTYLINE+ block_list { $2 }

list_t:
| item_t+ list_t_f { mk_list $1 :: $2 }

item_t:
| OITEM inline(regular)* { (true, $1, $2) }
| UITEM inline(regular)* { (false, $1, $2) }

list_t_f:
| lf=header | lf=code_block | lf=math_block | lf=table | lf=source_block
| lf=eof | lf=ext | lf=link_url | lf=comment_block | lf=config_block { lf }
| EMPTYLINE+ lf=block_list { lf }

paragraph:
| inline(regular)+ paragraph_f { Paragraph $1 :: $2 }

paragraph_f:
| pf=header | pf=eof | pf=code_block | pf=source_block | pf=math_block
| pf=table | pf=ext | pf=list_t | pf=link_url | pf=comment_block
| pf=config_block
  { pf }
| EMPTYLINE+ block_list { $2 }

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

ext:
| EXT ext_f { ExternRender (fst $1, snd $1) :: $2 }

ext_f:
| EMPTYLINE* block_list { $2 }

comment_block:
| COMMENT_BLOCK comment_block_f { CommentBlock $1 :: $2 }

comment_block_f:
| EMPTYLINE* block_list { $2 }

link_url:
| LINK_URL LINK LINK_END link_url_f { links_urls := ($1, $2) :: !links_urls;
                                      $4 }

link_url_f:
| EMPTYLINE* block_list { $2 }

config_block:
| CONFIG config_block_f { config_blocks := $1 :: !config_blocks; $2 }

config_block_f:
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
| SUB { Sub [Plain (String.make 1 $1)] }
| SUP { Sup [Plain (String.make 1 $1)] }
| SUP_START inline(regular)* SUP_END  { Sup $2 }
| SUB_START inline(regular)* SUB_END { Sub $2 }
| INLINE_SOURCE { InlineSource $1 }
| IMAGE { Image (fst $1, snd $1) }
| LINK inline(param)* LINK_END { Link ($1, $2) }
| HTML_ENTITIE { HTMLEntitie $1 }
| GREEK_LETTER { GreekLetter $1 }
| param { $1 }
