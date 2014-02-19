open OUnit
open Type
open Lexer

let parse str =
  let lexbuf = Lexing.from_string str in
  snd @@ Lexer.parse lexbuf

let assert_equal tests =
  List.iter
    (fun (expected, input) ->
     assert_equal ~printer:string_of_document expected input)
    tests

let title _ =
  assert_equal [
      [Title(3, [Plain "Lorem"])],
      (parse "=== Lorem === ");

      [Title(3, [Plain "Lorem ="])],
      (parse "=== Lorem ==== ");

      [Title(3, [Plain "Lorem ="])],
      (parse "=== Lorem = === ");

      [Title(1, [Plain "Lorem"])],
      (parse "= Lorem")
    ]

let paragraph _ =
  assert_equal [
      [Paragraph [Plain "Lorem ipsum."]],
      (parse "Lorem ipsum.")
    ]

let emphasis _ =
  assert_equal [
      [Paragraph [Bold [Plain "Lorem"]; Plain " ipsum."]],
      (parse "**Lorem** ipsum.");

      [Paragraph [Plain "Lorem "; Italic [Plain "ipsum"]; Plain "."]],
      (parse "Lorem //ipsum//.");

      [Paragraph[Plain"Lorem ";Underline[Plain"ipsum dolor"];Plain" sit."]],
      (parse "Lorem __ipsum dolor__ sit.");

      [Paragraph [Plain "Lorem "; Strike [Plain "ipsum."]]],
      (parse "Lorem ~~ipsum.~~");

      [Title (3, [Bold [Italic [Plain "Lo"; Strike [Plain "rem"]];
                        Plain " ipsum."]])],
      (parse "  ===  **//Lo~~rem~~// ipsum.**");

      [Paragraph [
           Bold[Italic[Plain "Lo";Bold[Plain "rem"]];Plain" ipsum."];
           Bold[Underline[Plain "Lo";Bold[Plain "rem"]];Plain" ipsum."];
           Bold[Strike[Plain "Lo";Bold[Plain "rem"]];Plain" ipsum."]]],
      (parse "**//Lo**rem**// ipsum.**
              **__Lo**rem**__ ipsum.**
              **~~Lo**rem**~~ ipsum.**");

      [Paragraph [
           Italic[Bold[Plain"Lo";Italic[Plain "rem"]];Plain " ipsum."];
           Italic[Underline[Plain"Lo";Italic[Plain"rem"]];Plain" ipsum."];
           Italic[Strike[Plain"Lo";Italic[Plain "rem"]];Plain" ipsum."]]],
      (parse "//**Lo//rem//** ipsum.//
              //__Lo//rem//__ ipsum.//
              //~~Lo//rem//~~ ipsum.//");

      [Paragraph [
           Underline[Bold[Plain"Lo";Underline[Plain "rem"]];Plain " ipsum."];
           Underline[Italic[Plain"Lo";Underline[Plain"rem"]];Plain" ipsum."];
           Underline[Strike[Plain"Lo";Underline[Plain"rem"]];Plain" ipsum."]]],
      (parse "__**Lo__rem__** ipsum.__
              __//Lo__rem__// ipsum.__
              __~~Lo__rem__~~ ipsum.__");

      [Paragraph [
           Strike[Bold[Plain"Lo";Strike[Plain "rem"]];Plain " ipsum."];
           Strike[Italic[Plain"Lo";Strike[Plain"rem"]];Plain" ipsum."];
           Strike[Underline[Plain"Lo";Strike[Plain"rem"]];Plain" ipsum."]]],
      (parse "~~**Lo~~rem~~** ipsum.~~
              ~~//Lo~~rem~~// ipsum.~~
              ~~__Lo~~rem~~__ ipsum.~~");
    ];

  assert_raises Parser.Error (fun () -> (parse "**//Lorem** ipsum.//"));
  assert_raises Parser.Error (fun () -> (parse "//**Lorem// ipsum.**"));
  assert_raises Parser.Error (fun () -> (parse "~~//Lorem~~ ipsum.//"));
  assert_raises Parser.Error (fun () -> (parse "__//Lorem__ ipsum.//"))

let sup_sub _ =
  assert_equal [

      [Paragraph [Plain "Lorem"; Sup [Plain "42"]; Plain " ipsum."]],
      (parse "Lorem^{42} ipsum.");

      [Paragraph [Plain "Lorem";
                  Sub [Bold [Plain "dolor ";
                             Italic [Plain "sit"];
                             Plain " amet"]];
                  Plain " ipsum."]],
      (parse "Lorem_{**dolor //sit// amet**} ipsum.");

      [Paragraph [Plain "Lorem"; Sup [Plain "2"]; Plain " ipsum."]],
      (parse "Lorem^{2} ipsum.");

      [Paragraph [Plain "Lorem"; Sub [Plain "i"]; Plain " ipsum."]],
      (parse "Lorem_i ipsum.");
    ];

  assert_raises (Failure"lexing: empty token")(fun()->(parse "Lorem_{ipsum"));
  assert_raises (Failure"lexing: empty token")(fun()->(parse "Lorem^{ipsum"))

let inline_code _ =
  assert_equal [
      [Paragraph [InlineCode "Lorem"; Plain " ipsum."]],
      (parse "``Lorem`` ipsum.");

      [Paragraph [InlineCode "Lorem ipsum"]],
      (parse "``Lorem \nipsum``")
    ]

let code_block _ =
  assert_equal [
      [CodeBlock "let lorem = ipsum"],
      (parse "```let lorem = ipsum```");

      [CodeBlock "let lorem = ipsum\nand ipsum = lorem"],
      (parse "```\nlet lorem = ipsum\nand ipsum = lorem\n```")
    ]

let inline_source _ =
  assert_equal [
      [Paragraph [InlineSource "<b>Lorem <i>ipsum</i>.</b>"]],
      (parse "{{<b>Lorem <i>ipsum</i>.</b>}}");

      [Paragraph [InlineSource "<b>Lorem <i>ipsum</i>.</b>}}"]],
      (parse "{{<b>Lorem <i>ipsum</i>.</b>}}}}")
    ]

let source_block _ =
  assert_equal [
      [SourceBlock "<p>Lorem ipsum.</p>"],
      (parse "{{{\n<p>Lorem ipsum.</p>\n}}}");

      [SourceBlock "<p>Lorem\nipsum.</p>"],
      (parse "{{{\n<p>Lorem\nipsum.</p>\n}}}");

      [SourceBlock "<p>Lorem ipsum.</p>}}}"],
      (parse "{{{\n<p>Lorem ipsum.</p>\n}}}}}}")
    ]

let math _ =
  assert_equal [
      [Paragraph[InlineMath[Plain"Lorem";Sup[Plain "2"];Plain" ipsum."]]],
      (parse "$$Lorem^2 ipsum.$$");

      [MathBlock [Plain "Lorem"; Sub [Plain "i"]; Plain " ipsum."]],
      (parse "$$$Lorem_i ipsum.$$$");
    ];

  assert_raises
    (Failure "TODO: raise Parser.Error")
    (fun () -> (parse "$$Lorem **ipsum**$$"))

let list_t _ =
  assert_equal [
      [List(true,[Item([Plain"Lorem"],
                       Some (true,
                             [Item([Plain"ipsum"], None);
                              Item([Plain"dolor ##sit"], None)]));
                  Item([Plain"amet"], None)])],
      (parse "# Lorem\n##ipsum\n##dolor ##sit\n#amet");

      [List(false, [Item([Plain"Lorem"],
                         Some (false,
                               [Item([Plain"ipsum"], None);
                                Item([Plain"dolor --sit"], None)]));
                    Item([Plain"amet"], None)])],
      (parse "- Lorem\n--ipsum\n--dolor --sit\n-amet");

      [List(true, [Item([Plain"Lorem"],
                        Some (false,
                              [Item([Plain"ipsum"], None)]));
                   Item([Plain"dolor"], None)])],
        (parse "# Lorem\n--ipsum\n#dolor");

        [List(false, [Item([Plain"Lorem"],
                           Some (true,
                                 [Item([Plain"ipsum"], None)]));
                      Item([Plain"dolor"], None)])],
        (parse "- Lorem\n##ipsum\n-dolor");
    ];
  
  assert_raises
    (Failure "List error")
    (fun () -> parse "- Lorem\n---ipsum");
  assert_raises
    (Failure "List error")
    (fun () -> parse "- Lorem\n#ipsum")

let table _ =
  assert_equal [
      [Table(None, [[[Plain "* Cell 1.1"]]])],
      parse "| * Cell 1.1 |\n";
      
      [Table(None,[[[Plain "Cell 1"; Sup [Plain "1"]];[Plain "Cell 1.2"]];
                   [[Plain "Cell 2.1"];[Plain "Cell "; Bold [Plain "2.2"]]]])],
      parse "| Cell 1^1 | Cell 1.2 |\n| Cell 2.1 | Cell **2.2** |";

      [Table(None, [[[Plain "Cell 1.1"];[Plain "Cell 1.2"]];
                    [[Plain "Cell 2.1"];[Plain "Cell 2.2"]]])],
      parse "| Cell 1.1 | Cell 1.2 |\n| Cell 2.1 | Cell 2.2 |";

      [Table(Some [[Plain "Cell 1.1"];[Plain "Cell 1.2"]],
             [[[Plain "Cell 2.1"];[Plain "Cell 2.2"]]])],
      (parse "| Cell 1.1 | Cell 1.2 |\n+--+\n| Cell 2.1 | Cell 2.2 |");

      [Table(None, [[[];[]];
                    [[];[]]])],
      (parse "|  |  |\n|  |  |");
    ];
  
  assert_raises
    Parser.Error
    (fun () -> parse "| Cell 1.1 | Cell 1.2 |\n+---------+")

let escape _ =
  assert_equal [
      [Paragraph [Plain "-"]], (parse "\\-");
      [Paragraph [Plain "#"]], (parse "\\#");
      [Paragraph [Plain "|"]], (parse "\\|");
      [Paragraph [Plain "="]], (parse "\\=");
      [Paragraph [Plain "`"]], (parse "\\`");
      [Paragraph [Plain "{"]], (parse "\\{");
      [Paragraph [Plain "$"]], (parse "\\$");
      [Paragraph [Plain "**"]], (parse "\\**");
      [Paragraph [Plain "//"]], (parse "\\//");
      [Paragraph [Plain "__"]], (parse "\\__");
      [Paragraph [Plain "~~"]], (parse "\\~~");
      [Paragraph [Plain "^"]], (parse "\\^");
      [Paragraph [Plain "\\"]], (parse "\\\\");
      [Paragraph [Plain "\\not escapable"]], (parse "\\not escapable");
      [Paragraph [Plain "-"; Plain "#"; Plain "|"; Plain "=`{$**//__~~^\\"]],
      (parse "\\-\n\\#\n\\|\n\\=\\`\\{\\$\\**\\//\\_\\_\\~~\\^\\")
    ]

let block_suit _ =
  assert_equal [
      [Title(1, [Plain "Lorem"]); Title(2, [Plain "Ipsum"]);
       Paragraph [Plain "Dolor sit amet."]],
      (parse "=Lorem\n  ==  Ipsum\n\nDolor sit amet.")
    ]

let link _ =
  assert_equal [
      [Paragraph[Link("foo", [])]],
      (parse "[[foo]]");
      
      [Paragraph[Plain "foo "; Link("bar", [])]],
      (parse "foo [[bar]]");
      
      [Paragraph[Link("foo[]", [])]],
      (parse "[[foo[] ]]");
      
      [Paragraph [Link("foo", [Plain "bar"])]],
      (parse "[[foo<<bar]]");
      
      [Paragraph[Link("foo", [Plain "bar"])]],
      (parse "[[ foo << bar ]]");
      
      [Paragraph[Link("foo", [Bold[Plain "bar"]])]],
      (parse "[[ foo << **bar** ]]");
      
      [Paragraph[Link("foo", [Bold[Plain "bar"]; Plain " bu"])]],
      (parse "[[ foo << **bar** bu ]]");
      
      [Paragraph[Link("foo<<||bar]]", [])]],
      (parse "[[ foo\\<<\\||bar\\]\\]]]");
      
      [Paragraph[Link("foo", [Plain "bar "; Plain "bu"])]],
      (parse "[[ foo << bar \n           bu]]");
      
      [Paragraph[Link("foo/bar", [Plain "bu"])]],
      (parse "[[ foo\n   /bar << bu]]");
      
      [Paragraph[Link("foo<<bar", [Plain "bu"])]],
      (parse "[[ foo\n   \\<<bar << bu]]")
    ]

let image _ =
  assert_equal [
      [Paragraph[Image("foo", "bar")]],
      (parse "[[foo||bar]]");
      
      [Paragraph[Image("foo", "bar")]],
      (parse "[[ foo || bar ]]");
      
      [Paragraph[Image("foo", "bar bu")]],
      (parse "[[ foo || bar bu ]]");
      
      [Paragraph[Image("foo|", "bar]")]],
      (parse "[[foo\\|||bar\\]]]");
      
      [Paragraph[Link("foo", [Image("bar", "bu")])]],
      (parse "[[foo<<[[bar||bu]]]]")
    ]

let html_entitie _ =
  assert_equal [
      [Paragraph[Plain "x "; HTMLEntitie "isin"; Plain " N"]],
      (parse "x &isin; N");
      
      [Paragraph[Plain "x "; HTMLEntitie "#60"; Plain " y"]],
      (parse "x &#60; y");
      
      [Paragraph[Plain "x"; HTMLEntitie "sup2"; Plain " = y"]],
      (parse "x&sup2; = y");
      
      [Paragraph[Plain "& loremp ipsum;"]],
      (parse "& loremp ipsum;");
      
      [Paragraph[Plain "&#loremp; &42;"]],
      (parse "&#loremp; &42;")
    ]

let greek_letter _ =
  assert_equal [
      [Paragraph[GreekLetter 'a'; Plain "+"; GreekLetter 'b']],
      (parse "&a+&b");
      
      [Paragraph[GreekLetter 'a'; Plain "+"; GreekLetter 'b'; Plain "+1;"]],
      (parse "&a+&b+1;")
    ]

let extern _ =
  assert_equal [
      [ExternRender("dot", "graph g{\na -- b -- c;\nb -- d;\n}")],
        (parse "%%%dot graph g{\na -- b -- c;\nb -- d;\n}%%%");
      
      [ExternRender("dot", "graph g{\na -- b -- c;\nb -- d;\n}")],
        (parse "%%%dot\ngraph g{\na -- b -- c;\nb -- d;\n}\n%%%");  

      [ExternRender("foo", "%%% %%%")],
        (parse "%%%foo \\%%% \\%%%%%%");
      
      [Paragraph [Plain "%%% dot"]],
      (parse "%%% dot")
    ]
                                                               
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
     "Escape" >:: escape;
     "Link" >:: link;
     "Image" >:: image;
     "HTML entitie" >:: html_entitie;
     "Greek letter" >:: greek_letter;
     "Extern rendering" >:: extern;
     "Block suit" >:: block_suit]

let _  =
  run_test_tt_main suite
