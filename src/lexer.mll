{
  open Buffer
  open Parser
  open Toml

  let flush acc buffer =
    let txt = contents buffer in
    if txt = "" then acc else PLAIN txt :: acc

  let flush_and_add acc buffer tok =
    let acc = flush acc buffer in tok :: acc
}

let escapable = ['-' '#' '|' '=' '`' '{' '$' '*' '/' '_' '~' '^' '\\' '[']

(*** BLOCK MARKERS ***)

rule line_beginning acc = parse
| eof
  { List.rev acc }
| ' '
  { line_beginning acc lexbuf }
| ('-'+ as l) ' '*
  { line (UITEM (String.length l) :: acc) (create 15) lexbuf }
| ('#'+ as l) ' '*
  { line (OITEM (String.length l) :: acc) (create 15) lexbuf }
| ('='+ as l) ' '*
  { line (TITLE (String.length l) :: acc) (create 15) lexbuf }
| '|' ' '*
  { line (TBL_START :: acc) (create 15) lexbuf }
| '+' ['+' '-']* '+' ("\n" | eof as ending)
  { match ending with
    | "\n" -> line_beginning (TBL_HSEP :: acc) lexbuf
    | eof -> List.rev (TBL_HSEP :: acc)}
| '\n'
  { line_beginning (EMPTYLINE :: acc) lexbuf }
| ""
  { line acc (create 15) lexbuf }

(*** REGULAR LINES ***)

and line acc read_buf = parse
| "```" '\n'?
  { code_block (flush acc read_buf) (create 15) lexbuf }
| "{{{" '\n'?
  { source_block (flush acc read_buf) (create 15) lexbuf }
| "$$$" '\n'?
  { line (flush_and_add acc read_buf MATH_BLOCK) (create 15) lexbuf }
| "**"
  { line (flush_and_add acc read_buf BOLD) (create 15) lexbuf }
| "//"
  { line (flush_and_add acc read_buf ITALIC) (create 15) lexbuf }
| "__"
  { line (flush_and_add acc read_buf UNDERLINE) (create 15) lexbuf }
| "~~"
  { line (flush_and_add acc read_buf STRIKE) (create 15) lexbuf }
| "``"
  { inline_code (flush acc read_buf) (create 15) lexbuf }
| "{{"
  { inline_source (flush acc read_buf) (create 15) lexbuf }
| "$$"
  { line (flush_and_add acc read_buf MATH) (create 15) lexbuf }
| "^{"
  { sup_sub 1 SUP_END (flush_and_add acc read_buf SUP_START)
            (create 15) lexbuf }
| "_{"
  { sup_sub 1 SUB_END (flush_and_add acc read_buf SUB_START)
            (create 15) lexbuf }
| "[[" ' '*
  { url (flush acc read_buf) (create 15) lexbuf }
| '^' (_ as c)
  { line (flush_and_add acc read_buf (SUP c)) (create 15) lexbuf }
| '_' (_ as c)
  { line (flush_and_add acc read_buf (SUB c)) (create 15) lexbuf }
| ' '* '|' ' '*
  { line (flush_and_add acc read_buf TBL_SEP) (create 15) lexbuf }
| ' '* '|' ' '* (('\n' | eof) as c)
  { match c with
    | "\n" -> line_beginning
                (flush_and_add acc read_buf TBL_END) lexbuf
    | _ -> List.rev (flush_and_add acc read_buf TBL_END) }
| '\n'
  { line_beginning (flush acc read_buf) lexbuf }
| '\\' (escapable as c)
  { add_char read_buf c;
    line acc read_buf lexbuf }
| '&' ((('#' ['0'-'9']+) | ['a'-'z']['a'-'z' '0'-'9']+) as e) ';'
  { line (flush_and_add acc read_buf (HTML_ENTITIE e)) (create 15) lexbuf }
| '&' (['a'-'z' 'A'-'Z'] as c)
  { line (flush_and_add acc read_buf (GREEK_LETTER c)) (create 15) lexbuf }
| _ as c
  { add_char read_buf c;
    line acc read_buf lexbuf }
| eof { List.rev @@ flush acc read_buf }

and inline_code acc read_buf = parse
| "``"
  { line (INLINE_CODE (contents read_buf) :: acc)
    (create 15) lexbuf }
| '\n'
  { inline_code acc read_buf lexbuf }
| _ as c
  { add_char read_buf c;
    inline_code acc read_buf lexbuf }

and code_block acc read_buf = parse
| '\n'? "```" '\n'?
  { line_beginning (CODE_BLOCK (contents read_buf) :: acc) lexbuf }
| _ as c
  { add_char read_buf c;
    code_block acc read_buf lexbuf }

and inline_source acc read_buf = parse
| "}}" ('}'* as s)
  { add_string read_buf s;
    line (INLINE_SOURCE (contents read_buf) :: acc)
    (create 15) lexbuf }
| '\n'
  { inline_source acc read_buf lexbuf }
| _ as c
  { add_char read_buf c;
    inline_source acc read_buf lexbuf }

and source_block acc read_buf = parse
| '\n'? "}}}" ('}'* as s) '\n'?
  { add_string read_buf s;
    line_beginning (SOURCE_BLOCK (contents read_buf) :: acc) lexbuf }
| _ as c
  { add_char read_buf c;
    source_block acc read_buf lexbuf }

and sup_sub opened closing acc read_buff = parse
| "\\{"
  { add_char read_buff '{';
    sup_sub opened closing acc read_buff lexbuf }
| "\\}"
  { add_char read_buff '}';
    sup_sub opened closing acc read_buff lexbuf }
| ['_' '^'] '{' as s
  { add_string read_buff s;
    sup_sub (opened + 1) closing acc read_buff lexbuf }
| "{{"
  { add_string read_buff "{{";
    sup_sub (opened + 2) closing acc read_buff lexbuf }
| '}'
  { if opened = 1
    then let lex = Lexing.from_string (contents read_buff) in
         let tokens = line_beginning [] lex in
         let acc = closing
                   :: List.fold_left
                        (fun acc -> fun tok -> tok :: acc)
                        acc tokens in
         line acc (create 15) lexbuf
    else (add_char read_buff '}';
          sup_sub (opened - 1) closing acc read_buff lexbuf) }
| _ as c { add_char read_buff c;
           sup_sub opened closing acc read_buff lexbuf }

and url acc buff = parse
| [' ' '\n']* "||" [' ' '\n']*
  { image (contents buff) acc (create 15) lexbuf }
| [' ' '\n']* "<<" [' ' '\n']*
  { link 1 (contents buff) acc (create 15) lexbuf }
| [' ' '\n']* "]]" [' ' '\n']*
  { line (LINK_END :: LINK (contents buff) :: acc) (create 15) lexbuf }
| '\\' (['|' '<' ']'] as c) {add_char buff c; url acc buff lexbuf }
| _ as c {add_char buff c; url acc buff lexbuf }

and image url acc buff = parse
| "\\]" { add_char buff ']'; image url acc buff lexbuf }
| ' '* "]]"
  { line (IMAGE (url, contents buff) :: acc) (create 15) lexbuf }
| '\n' ' '* { image url acc buff lexbuf }
| _ as c { add_char buff c; image url acc buff lexbuf }

and link depth url acc buff = parse
| "\\["
  { add_char buff '[';
    link depth url acc buff lexbuf }
| "\\]"
  { add_char buff ']';
    link depth url acc buff lexbuf }
| "[["
  { add_string buff "[[";
    link (depth + 1) url acc buff lexbuf }
| (' '* "]]") as s
  { if depth = 1
    then let lex = Lexing.from_string (contents buff) in
         let tokens = line_beginning [] lex in
         let acc =
           LINK_END ::
             List.fold_left
               (fun acc -> fun tok -> tok :: acc)
               (LINK url :: acc) tokens in
         line acc (create 15) lexbuf
    else (add_string buff s;
          link (depth - 1) url acc buff lexbuf) }
| _ as c { add_char buff c;
           link depth url acc buff lexbuf }

(*** CONFIGURATION ***)
and first_line = parse
| ['\n' ' '] { first_line lexbuf }
| "%{" { config (Buffer.create 15) lexbuf }
| "" { (Hashtbl.create 0), (line_beginning [] lexbuf) }


(* "%}" point the end of configuration part.
 * It may be escaped *)
and config buff = parse
| "\\%}" { add_string buff "%}"; config buff lexbuf }
| "%}" { (Toml.from_string (Buffer.contents buff)),
         line_beginning [] lexbuf }
| _ as c { add_char buff c; config buff lexbuf }

{
  (* Dirty fix to use Menhir with token list *)
  let parse lexbuf =
    let config, tokens = first_line lexbuf in
    let tokens = ref tokens in
    let token _ = 
      match !tokens with 
      | []     -> EOF 
      | h :: t -> tokens := t ; h 
    in config, Parser.document token @@ Lexing.from_string ""
}
