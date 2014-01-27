open OUnit2
open AST
open Parser

let assert_equal ctxt = assert_equal ~printer:string_of_document ctxt

(* Quick and dirty fix to use Menhir with token list *)
let parse str =
  let lexbuf = Lexing.from_string str in
  let tokens = ref @@ Lexer.line_beginning [] lexbuf in
  let token _ = 
    match !tokens with 
    | []     -> EOF 
    | h :: t -> tokens := t ; h 
  in Parser.document token @@ Lexing.from_string ""

let simple_title _ =
  assert_equal
    [Title(3, [Plain "Lorem"])]
    (parse "=== Lorem === ");
  assert_equal
    [Title(3, [Plain "Lorem ="])]
    (parse "=== Lorem ==== ");
  assert_equal
    [Title(3, [Plain "Lorem ="])]
    (parse "=== Lorem = === ");
  assert_equal
    [Title(1, [Plain "Lorem"])]
    (parse "= Lorem")

let simple_paragraph _ =
  assert_equal
    [Paragraph [Plain "Lorem ipsum."]]
    (parse "Lorem ipsum.");
  assert_equal
    [Paragraph [Plain "Lorem ipsum."; Plain "Dolor sit amet"]]
    (parse "Lorem ipsum.\nDolor sit amet")

let block_suit _ =
  assert_equal
    [Title(1, [Plain "Lorem"]);
     Title(2, [Plain "Ipsum"]);
     Paragraph [Plain "Dolor sit amet."]]
    (parse "=Lorem\n  ==  Ipsum\n\nDolor sit amet.")

let emphasis _ =
  assert_equal
    [Paragraph [Bold [Plain "Lorem"]; Plain " ipsum."]]
    (parse "**Lorem** ipsum.");
  assert_equal
    [Paragraph [Plain "Lorem "; Italic [Plain "ipsum"]; Plain "."]]
    (parse "Lorem //ipsum//.");
  assert_equal
    [Paragraph [Plain "Lorem "; Underline [Plain "ipsum dolor"]; Plain " sit."]]
    (parse "Lorem __ipsum dolor__ sit.");
  assert_equal
    [Paragraph [Plain "Lorem "; Strike [Plain "ipsum."]]]
    (parse "Lorem ~~ipsum.~~");
  assert_equal
    [Title (3, [Bold [Italic [Plain "Lo"; Strike [Plain "rem"]];
                      Plain " ipsum."]])]
    (parse "  ===  **//Lo~~rem~~// ipsum.**");
  assert_raises
    Parser.Error
    (fun () -> (parse  "  ===  **//Lorem** ipsum.//"))

let sup_sub _ =
  assert_equal
    [Paragraph [Plain "Lorem"; Sup '2'; Plain " ipsum."]]
    (parse "Lorem^2 ipsum.");
  assert_equal
    [Paragraph [Plain "Lorem"; Sub 'i'; Plain " ipsum."]]
    (parse "Lorem_i ipsum.")

let inline_code _ =
  assert_equal
    [Paragraph [InlineCode "Lorem"; Plain " ipsum."]]
    (parse "``Lorem`` ipsum.");
  assert_equal
    [Paragraph [InlineCode "Lorem ipsum"]]
    (parse "``Lorem \nipsum``")

let code_block _ =
  assert_equal
    [CodeBlock "let lorem = ipsum"]
    (parse "```let lorem = ipsum```");
  assert_equal
    [CodeBlock "let lorem = ipsum\nand ipsum = lorem"]
    (parse "```\nlet lorem = ipsum\nand ipsum = lorem\n```")

let inline_source _ =
  assert_equal
    [Paragraph [InlineSource "<b>Lorem <i>ipsum</i>.</b>"]]
    (parse "{{<b>Lorem <i>ipsum</i>.</b>}}");
  assert_equal
    [Paragraph [InlineSource "<b>Lorem <i>ipsum</i>.</b>}}"]]
    (parse "{{<b>Lorem <i>ipsum</i>.</b>}}}}");
  assert_equal
    [Paragraph [InlineSource "<b>Lorem <i>ipsum</i>.</b>"]]
    (parse "{{<b>Lorem <i>\nipsum</i>.</b>}}")

let source_block _ =
  assert_equal
    [SourceBlock "<p>Lorem ipsum.</p>"]
    (parse "{{{\n<p>Lorem ipsum.</p>\n}}}");
  assert_equal
    [SourceBlock "<p>Lorem\nipsum.</p>"]
    (parse "{{{\n<p>Lorem\nipsum.</p>\n}}}");
  assert_equal
    [SourceBlock "<p>Lorem ipsum.</p>}}}"]
    (parse "{{{\n<p>Lorem ipsum.</p>\n}}}}}}")

let math _ =
  assert_equal
    [Paragraph [InlineMath [Plain "Lorem"; Sup '2'; Plain " ipsum."]]]
    (parse "$$Lorem^2 ipsum.$$");
  assert_equal
    [MathBlock [Plain "Lorem"; Sub 'i'; Plain " ipsum."]]
    (parse "$$$Lorem_i ipsum.$$$");
  assert_raises
    (Failure "TODO: raise Parser.Error")
    (fun () -> (parse "$$Lorem **ipsum**$$"))

let suite = 
  "Suite" >:::
    ["Title" >:: simple_title;
     "Paragraph" >:: simple_paragraph;
     "Block suit" >:: block_suit;
     "Emphasis text" >:: emphasis;
     "Superscript and subscript" >:: sup_sub;
     "Inline code" >:: inline_code;
     "Code block" >:: code_block;
     "Inline source" >:: inline_source;
     "Source block" >:: source_block;
     "Math" >:: math]

let () =
  run_test_tt_main suite
