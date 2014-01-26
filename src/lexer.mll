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
| ('='+ as l) ' '*
  { line (TITLE (String.length l) :: acc) (Buffer.create 15) lexbuf }
| '\n'
  { line_beginning (EMPTYLINE :: acc) lexbuf }
| ""
  { line acc (Buffer.create 15) lexbuf }

(*** REGULAR LINES ***)

and line acc read_buf = parse
| "**"
  { line (flush_and_add acc read_buf BOLD) (Buffer.create 15) lexbuf }
| "//"
  { line (flush_and_add acc read_buf ITALIC) (Buffer.create 15) lexbuf }
| "__"
  { line (flush_and_add acc read_buf UNDERLINE) (Buffer.create 15) lexbuf }
| "~~"
  { line (flush_and_add acc read_buf STRIKE) (Buffer.create 15) lexbuf }
| '\n'
  { line_beginning (flush acc read_buf) lexbuf }
| _ as c
  { Buffer.add_char read_buf c;
    line acc read_buf lexbuf }
| eof { List.rev @@ flush acc read_buf }
{}
