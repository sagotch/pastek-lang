{
  open Parser

  let flush acc buffer =
    let txt = Buffer.contents buffer in
    if txt = "" then acc else PLAIN txt :: acc

  let flush_and_add acc buffer tok =
    let acc = flush acc buffer in tok :: acc
}

(*** BLOCK MARKERS ***)

rule line_beginning acc = parse
| eof
  { List.rev acc }
| ' '
  { line_beginning acc lexbuf }
| ('-'+ as l) ' '*
  { line (UITEM (String.length l) :: acc) (Buffer.create 15) lexbuf }
| ('#'+ as l) ' '*
  { line (OITEM (String.length l) :: acc) (Buffer.create 15) lexbuf }
| ('='+ as l) ' '*
  { line (TITLE (String.length l) :: acc) (Buffer.create 15) lexbuf }
| '|' ' '*
  { line (TBL_START :: acc) (Buffer.create 15) lexbuf }
| '+' ['+' '-']* '+' ("\n" | eof as ending)
  { match ending with
    | "\n" -> line_beginning (TBL_HSEP :: acc) lexbuf
    | eof -> List.rev (TBL_HSEP :: acc)}
| '\n'
  { line_beginning (EMPTYLINE :: acc) lexbuf }
| ""
  { line acc (Buffer.create 15) lexbuf }

(*** REGULAR LINES ***)

and line acc read_buf = parse
| "```" '\n'?
  { code_block (flush acc read_buf) (Buffer.create 15) lexbuf }
| "{{{" '\n'?
  { source_block (flush acc read_buf) (Buffer.create 15) lexbuf }
| "$$$" '\n'?
  { line (flush_and_add acc read_buf MATH_BLOCK) (Buffer.create 15) lexbuf }
| "**"
  { line (flush_and_add acc read_buf BOLD) (Buffer.create 15) lexbuf }
| "//"
  { line (flush_and_add acc read_buf ITALIC) (Buffer.create 15) lexbuf }
| "__"
  { line (flush_and_add acc read_buf UNDERLINE) (Buffer.create 15) lexbuf }
| "~~"
  { line (flush_and_add acc read_buf STRIKE) (Buffer.create 15) lexbuf }
| "``"
  { inline_code (flush acc read_buf) (Buffer.create 15) lexbuf }
| "{{"
  { inline_source (flush acc read_buf) (Buffer.create 15) lexbuf }
| "$$"
  { line (flush_and_add acc read_buf MATH) (Buffer.create 15) lexbuf }
| "^{"
  { sup_sub 1 SUP_END (flush_and_add acc read_buf SUP_START)
            (Buffer.create 15) lexbuf }
| "_{"
  { sup_sub 1 SUB_END (flush_and_add acc read_buf SUB_START)
            (Buffer.create 15) lexbuf }
| '^' (_ as c)
  { line (flush_and_add acc read_buf (SUP c)) (Buffer.create 15) lexbuf }
| '_' (_ as c)
  { line (flush_and_add acc read_buf (SUB c)) (Buffer.create 15) lexbuf }
| ' '* '|' ' '*
  { line (flush_and_add acc read_buf TBL_SEP) (Buffer.create 15) lexbuf }
| ' '* '|' ' '* (('\n' | eof) as c)
  { match c with
    | "\n" -> line_beginning
                (flush_and_add acc read_buf TBL_END) lexbuf
    | _ -> List.rev (flush_and_add acc read_buf TBL_END) }
| '\n'
  { line_beginning (flush acc read_buf) lexbuf }
| _ as c
  { Buffer.add_char read_buf c;
    line acc read_buf lexbuf }
| eof { List.rev @@ flush acc read_buf }

and inline_code acc read_buf = parse
| "``"
  { line (INLINE_CODE (Buffer.contents read_buf) :: acc)
    (Buffer.create 15) lexbuf }
| '\n'
  { inline_code acc read_buf lexbuf }
| _ as c
  { Buffer.add_char read_buf c;
    inline_code acc read_buf lexbuf }

and code_block acc read_buf = parse
| '\n'? "```" '\n'?
  { line_beginning (CODE_BLOCK (Buffer.contents read_buf) :: acc) lexbuf }
| _ as c
  { Buffer.add_char read_buf c;
    code_block acc read_buf lexbuf }

and inline_source acc read_buf = parse
| "}}" ('}'* as s)
  { Buffer.add_string read_buf s;
    line (INLINE_SOURCE (Buffer.contents read_buf) :: acc)
    (Buffer.create 15) lexbuf }
| '\n'
  { inline_source acc read_buf lexbuf }
| _ as c
  { Buffer.add_char read_buf c;
    inline_source acc read_buf lexbuf }

and source_block acc read_buf = parse
| '\n'? "}}}" ('}'* as s) '\n'?
  { Buffer.add_string read_buf s;
    line_beginning (SOURCE_BLOCK (Buffer.contents read_buf) :: acc) lexbuf }
| _ as c
  { Buffer.add_char read_buf c;
    source_block acc read_buf lexbuf }

and sup_sub opened closing acc read_buff = parse
| "\\{"
  { Buffer.add_char read_buff '{';
    sup_sub opened closing acc read_buff lexbuf }
| ['_' '^'] '{' as s
  { Buffer.add_string read_buff s;
    sup_sub (opened + 1) closing acc read_buff lexbuf }
| "{{"
  { Buffer.add_string read_buff "{{";
    sup_sub (opened + 2) closing acc read_buff lexbuf }
| '}'
  { if opened = 1
    then let lex = Lexing.from_string (Buffer.contents read_buff) in
         let tokens = line_beginning [] lex in
         let acc = closing
                   :: List.fold_left
                        (fun acc -> fun tok -> tok :: acc)
                        acc tokens in
         line acc (Buffer.create 15) lexbuf
    else (Buffer.add_char read_buff '}';
          sup_sub (opened - 1) closing acc read_buff lexbuf) }
| _ as c { Buffer.add_char read_buff c;
           sup_sub opened closing acc read_buff lexbuf }

{
  (* Quick and dirty fix to use Menhir with token list *)
  let parse lexbuf =
    let tokens = ref @@ line_beginning [] lexbuf in
    let token _ = 
      match !tokens with 
      | []     -> EOF 
      | h :: t -> tokens := t ; h 
    in Parser.document token @@ Lexing.from_string ""
}
