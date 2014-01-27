%{
  open AST
%}

%token<int> TITLE
%token<string> PLAIN INLINE_CODE CODE_BLOCK INLINE_SOURCE SOURCE_BLOCK
%token<char> SUP SUB
%token BOLD ITALIC UNDERLINE STRIKE EMPTYLINE
%token EOF

%start <AST.document> document

%%

document:
| EMPTYLINE* block_list { $2 }

(*** BLOCKS ***)

block_list:
| bl=header | bl=paragraph | bl=code_block | bl=source_block | bl=eof { bl }

eof:
| EOF { [] }

(* NB: with this configuration, headers have to be separated from
 paragraphs with an empty line. *)
header:
| TITLE inline(regular)+ header_f { Title ($1, $2) :: $3 }

header_f:
| EMPTYLINE+ hf=paragraph { hf }
| EMPTYLINE* hf=header | EMPTYLINE* hf=code_block | EMPTYLINE* hf=source_block
| EMPTYLINE* hf=eof { hf }

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

(*** INLINE ELEMENTS ***)

regular:
| BOLD inline(bold)+ BOLD { Bold $2 }
| ITALIC inline(italic)+ ITALIC { Italic $2 }
| UNDERLINE inline(underline)+ UNDERLINE { Underline $2 }
| STRIKE inline(strike)+ STRIKE { Strike $2 }
| INLINE_CODE { InlineCode $1 }

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

inline(param):
| PLAIN { Plain $1 }
| SUB { Sub $1 }
| SUP { Sup $1 }
| INLINE_SOURCE { InlineSource $1 }
| param { $1 }
