open OUnit
open AST
open Lexer

let assert_equal ctxt = assert_equal ~printer:string_of_document ctxt
let parse str =
  let lexbuf = Lexing.from_string str in
  Lexer.parse lexbuf

let title _ =
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

let paragraph _ =
  assert_equal
    [Paragraph [Plain "Lorem ipsum."]]
    (parse "Lorem ipsum.");
  assert_equal
    [Paragraph [Plain "Lorem ipsum."; Plain "Dolor sit amet"]]
    (parse "Lorem ipsum.\nDolor sit amet")

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

let list_t _ =
  assert_equal
    [List(true, [Item([Plain"Lorem"],
                       Some (true,
                             [Item([Plain"ipsum"], None);
                              Item([Plain"dolor ##sit"], None)]));
                  Item([Plain"amet"], None)])]
    (parse "# Lorem\n##ipsum\n##dolor ##sit\n#amet");
  assert_equal
    [List(false, [Item([Plain"Lorem"],
                       Some (false,
                             [Item([Plain"ipsum"], None);
                              Item([Plain"dolor --sit"], None)]));
                  Item([Plain"amet"], None)])]
    (parse "- Lorem\n--ipsum\n--dolor --sit\n-amet");
  assert_equal
    [List(false, [Item([Plain"Lorem"],
                       Some (true,
                             [Item([Plain"ipsum"], None)]));
                  Item([Plain"dolor"], None)])]
    (parse "- Lorem\n##ipsum\n-dolor");
  assert_raises
    (Failure "List error")
    (fun () -> parse "- Lorem\n---ipsum");
  assert_raises
    (Failure "List error")
    (fun () -> parse "- Lorem\n#ipsum")

let table _ =
  assert_equal
    [Table(None, [[[Plain "* Cell 1.1"]]])]
    (parse "| * Cell 1.1 |\n");
  assert_equal
    [Table(None, [[[Plain "Cell 1"; Sup '1'];[Plain "Cell 1.2"]];
                  [[Plain "Cell 2.1"];[Plain "Cell "; Bold [Plain "2.2"]]]])]
    (parse "| Cell 1^1 | Cell 1.2 |\n| Cell 2.1 | Cell **2.2** |");
  assert_equal
    [Table(None, [[[Plain "Cell 1.1"];[Plain "Cell 1.2"]];
                  [[Plain "Cell 2.1"];[Plain "Cell 2.2"]]])]
    (parse "| Cell 1.1 | Cell 1.2 |\n| Cell 2.1 | Cell 2.2 |");
  assert_equal
    [Table(Some [[Plain "Cell 1.1"];[Plain "Cell 1.2"]],
           [[[Plain "Cell 2.1"];[Plain "Cell 2.2"]]])]
    (parse "| Cell 1.1 | Cell 1.2 |\n+--+\n| Cell 2.1 | Cell 2.2 |");
  assert_equal
    [Table(None, [[[];[]];
                  [[];[]]])]
    (parse "|  |  |\n|  |  |");
  assert_raises
    Parser.Error
    (fun () -> parse "| Cell 1.1 | Cell 1.2 |\n+---------+")

let block_suit _ =
  assert_equal
    [Title(1, [Plain "Lorem"]);
     Title(2, [Plain "Ipsum"]);
     Paragraph [Plain "Dolor sit amet."]]
    (parse "=Lorem\n  ==  Ipsum\n\nDolor sit amet.")

let suite = 
  "Suite" >:::
    ["Title" >:: title;
     "Paragraph" >:: paragraph;
     "Emphasis text" >:: emphasis;
     "Superscript and subscript" >:: sup_sub;
     "Inline code" >:: inline_code;
     "Code block" >:: code_block;
     "Inline source" >:: inline_source;
     "Source block" >:: source_block;
     "Math" >:: math;
     "List" >:: list_t;
     "Table" >:: table;
     "Block suit" >:: block_suit]

let _  =
  run_test_tt_main suite
