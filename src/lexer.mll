{
  open Buffer
  open Parser

  (* token list addition operator *)

  let ( <<< ) acc buf =
    match contents buf with
    | "" -> acc
    | txt -> PLAIN txt :: acc

  let ( << ) acc tok = tok :: acc

  (* Buffer addition operators *)

  let ( >>> ) str buff = Buffer.add_string buff str; buff

  let ( >> ) c buff = Buffer.add_char buff c; buff

  let return = List.rev

  let maybe_eol str lexbuf = if str = "\n" then Lexing.new_line lexbuf

}

let escapable = ['-' '#' '|' '=' '`' '{' '$' '*' '/' '_' '~' '^' '\\' '['
                 '&' '<']

let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let num = ['0' - '9']
let alphanum = alpha | num

rule line_beginning acc = parse
| eof                                                            { return acc }
| ' '                                             { line_beginning acc lexbuf }
| ('-'+ as l) ' '* { line (acc << UITEM (String.length l)) (create 42) lexbuf }
| ('#'+ as l) ' '* { line (acc << OITEM (String.length l)) (create 42) lexbuf }
| ('='+ as l) ' '* { line (acc << TITLE (String.length l)) (create 42) lexbuf }
| '|' ' '*                       { line (acc << TBL_START) (create 42) lexbuf }
| '+' ['+' '-']* "+\n"              { Lexing.new_line lexbuf;
                                      line_beginning (acc << TBL_HSEP) lexbuf }
| '+' ['+' '-']* '+' eof                           { return (acc << TBL_HSEP) }
| '\n'                             { Lexing.new_line lexbuf;
                                     line_beginning (acc << EMPTYLINE) lexbuf }
| "```" ('\n'? as e)                      { maybe_eol e lexbuf;
                                            code_block acc (create 42) lexbuf }
| "{{{" ('\n'? as e)                    { maybe_eol e lexbuf;
                                          source_block acc (create 42) lexbuf }
| "$$$" ('\n'? as e)      { maybe_eol e lexbuf;
                            math_block (acc << MATH_BLOCK) (create 42) lexbuf }
| "%%%" ([^' ' '\n']+ as cmd) ([' ' '\n']? as e)
                                      { maybe_eol e lexbuf;
                                        ext_render cmd acc (create 42) lexbuf }
| "<<<"                                { comment_block acc (create 42) lexbuf }
| ""                                            { line acc (create 42) lexbuf }
| "%{"                                 { config acc (Buffer.create 42) lexbuf }

and line acc buffer = parse
| "**"                     { line (acc <<< buffer << BOLD) (create 42) lexbuf }
| "//"                   { line (acc <<< buffer << ITALIC) (create 42) lexbuf }
| "__"                { line (acc <<< buffer << UNDERLINE) (create 42) lexbuf }
| "~~"                   { line (acc <<< buffer << STRIKE) (create 42) lexbuf }
| "``"                      { inline_code (acc <<< buffer) (create 42) lexbuf }
| "{{"                    { inline_source (acc <<< buffer) (create 42) lexbuf }
| "$$"                     { line (acc <<< buffer << MATH) (create 42) lexbuf }
| "^{"   { sup_sub 1 SUP_END (acc <<< buffer << SUP_START) (create 42) lexbuf }
| "_{"   { sup_sub 1 SUB_END (acc <<< buffer << SUB_START) (create 42) lexbuf }
| "[[" ' '*                         { url (acc <<< buffer) (create 42) lexbuf }
| '^' (_ as c)            { line (acc <<< buffer << SUP c) (create 42) lexbuf }
| '_' (_ as c)            { line (acc <<< buffer << SUB c) (create 42) lexbuf }
| ' '* '|' ' '* '\n'      { Lexing.new_line lexbuf;
                            line_beginning (acc <<< buffer << TBL_END) lexbuf }
| ' '* '|' ' '* eof                      { return (acc <<< buffer << TBL_END) }
| ' '* '|' ' '*         { line (acc <<< buffer << TBL_SEP) (create 42) lexbuf }
| '\n'       { Lexing.new_line lexbuf; line_beginning (acc <<< buffer) lexbuf }
| '\\' (escapable as c)           { add_char buffer c; line acc buffer lexbuf }
| '\\' (_ # escapable as c)
                  { failwith ("Illegal backslash escape: " ^ String.make 1 c) }
| '&' ('#' num+ | alpha alphanum+ as e) ';'
                 { line (acc <<< buffer << HTML_ENTITIE e) (create 42) lexbuf }
| '&' (alpha as c)
                 { line (acc <<< buffer << GREEK_LETTER c) (create 42) lexbuf }
| _ as c                                      { line acc (c >> buffer) lexbuf }
| eof                                               { return (acc <<< buffer) }

(* INLINE DELIMITED BLOCK *)

and inline_code acc buffer = parse
| "``"       { line (acc << INLINE_CODE (contents buffer)) (create 42) lexbuf }
| '\n'                                        { Lexing.new_line lexbuf;
                                                inline_code acc buffer lexbuf }
| _ as c                               { inline_code acc (c >> buffer) lexbuf }

and inline_source acc buffer = parse
| "}}"     { line (acc << INLINE_SOURCE (contents buffer)) (create 42) lexbuf }
| '\n'                                      { Lexing.new_line lexbuf;
                                              inline_source acc buffer lexbuf }
| _ as c                             { inline_source acc (c >> buffer) lexbuf }

(* DELIMITED BLOCKS *)

and ext_render cmd acc buffer = parse
| "\\%"                           { ext_render cmd acc ('%' >> buffer) lexbuf }
| ('\n'? as s) "%%%"
                  { maybe_eol s lexbuf;
                    line_beginning (acc << EXT (cmd, contents buffer)) lexbuf }
| _ as c                            { ext_render cmd acc (c >> buffer) lexbuf }

and code_block acc buffer = parse
| "\\`"                               { code_block acc ('`' >> buffer) lexbuf }
| '\n'       { Lexing.new_line lexbuf; code_block acc ('\n' >> buffer) lexbuf }
| ('\n'? as s) "```"
                { maybe_eol s lexbuf;
                  line_beginning (acc << CODE_BLOCK (contents buffer)) lexbuf }
| _ as c                                { code_block acc (c >> buffer) lexbuf }

and source_block acc buffer = parse
| "\\}"                             { source_block acc ('}' >> buffer) lexbuf }
| '\n'     { Lexing.new_line lexbuf; source_block acc ('\n' >> buffer) lexbuf }
| ('\n'? as s) "}}}"
              { maybe_eol s lexbuf;
                line_beginning (SOURCE_BLOCK (contents buffer) :: acc) lexbuf }
| _ as c                  { add_char buffer c; source_block acc buffer lexbuf }

and math_block acc buffer = parse
| "\\$"                               { math_block acc ('$' >> buffer) lexbuf }
| '\n'       { Lexing.new_line lexbuf; math_block acc ('\n' >> buffer) lexbuf }
| ('\n'? as s) "$$$"
{ maybe_eol s lexbuf;
  let tokens = line_beginning [] @@ Lexing.from_string (contents buffer) in
  line_beginning (List.rev_append tokens acc << MATH_BLOCK) lexbuf }
| _ as c                    { add_char buffer c; math_block acc buffer lexbuf }

and comment_block acc buff = parse
| '\n'      { Lexing.new_line lexbuf; comment_block acc ('\n' >> buff) lexbuf }
| "\\>"                              { comment_block acc ('>' >> buff) lexbuf }
| ">>>"        { line_beginning (acc << COMMENT_BLOCK (contents buff)) lexbuf }
| _ as c                               { comment_block acc (c >> buff) lexbuf }

and config acc buff = parse
| '\n'             { Lexing.new_line lexbuf; config acc ('\n' >> buff) lexbuf }
| "\\%"                                     { config acc ('%' >> buff) lexbuf }
| "%}"         { line_beginning (acc << CONFIG (Buffer.contents buff)) lexbuf }
| _ as c                                      { config acc (c >> buff) lexbuf }

(* SUPERSCRIPT/SUBSCRIPT *)

and sup_sub opened closing acc buffer = parse
| "\\{"                   { sup_sub opened closing acc ('{' >> buffer) lexbuf }
| "\\}"                   { sup_sub opened closing acc ('}' >> buffer) lexbuf }
| ['_' '^'] '{' as s { sup_sub (opened + 1) closing acc (s >>> buffer) lexbuf }
| "{{"            { sup_sub (opened + 2) closing acc ("{{" >>> buffer) lexbuf }
| '}'
    { if opened = 1
      then let lex = Lexing.from_string @@ contents buffer in
           let tokens = line_beginning [] lex in
           line (List.rev_append tokens acc << closing) (create 42) lexbuf
      else sup_sub (opened - 1) closing acc ('}' >> buffer) lexbuf }
| _ as c        { add_char buffer c; sup_sub opened closing acc buffer lexbuf }

(* LINK/IMAGE *)

(**
 * FIXME: Unable to write "[[link]] ::" without declaring a link
 * FIXME: Updated line number
 *)
and url acc buff = parse
| [' ' '\n']* "||" [' ' '\n']* { image (contents buff) acc (create 42) lexbuf }
| [' ' '\n']* "<<" [' ' '\n']*{ link 1 (contents buff) acc (create 42) lexbuf }
| [' ' '\n']* "]]" (' '* '\n'? ' '*) "::" (' '* '\n'? ' '*)
                   { line_beginning ( acc << LINK_URL (contents buff)) lexbuf }
| [' ' '\n']* "]]"
         { line ( acc << LINK (contents buff) << LINK_END) (create 42) lexbuf }
| '\\' (['|' '<' ']'] as c)                      { url acc (c >> buff) lexbuf }
| ('\n' ' '*)                                           { url acc buff lexbuf }
|  _ as c                                        { url acc (c >> buff) lexbuf }

and image url acc buff = parse
| "\\]"                                  { image url acc (']' >> buff) lexbuf }
| ' '* "]]"     { line (acc << IMAGE (url, contents buff)) (create 42) lexbuf }
| '\n' ' '*               { Lexing.new_line lexbuf; image url acc buff lexbuf }
| _ as c                                   { image url acc (c >> buff) lexbuf }

and link depth url acc buff = parse
| "\\["                             { link depth url acc ('[' >> buff) lexbuf }
| "\\]"                             { link depth url acc (']' >> buff) lexbuf }
| "[["                      { link (depth + 1) url acc ("[[" >>> buff) lexbuf }
| (' '* "]]") as s
  { if depth = 1
    then let tokens =
           line_beginning [] @@ Lexing.from_string (contents buff) in
         line (List.rev_append tokens (acc << LINK url) << LINK_END)
              (create 42) lexbuf
    else link (depth - 1) url acc (s >>> buff) lexbuf }
| _ as c                              { link depth url acc (c >> buff) lexbuf }

{
  (* Dirty fix to use Menhir with token list *)
  let parse lexbuf =
    let tokens = line_beginning [] lexbuf in
    let tokens = ref tokens in
    let token _ = 
      match !tokens with 
      | []     -> EOF 
      | h :: t -> tokens := t ; h 
    in Parser.document token @@ Lexing.from_string ""
}
