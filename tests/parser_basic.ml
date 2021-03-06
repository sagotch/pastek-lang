open OUnit
open Type
open Lexer

(* HELPERS *)

let parse str =
  let lexbuf = Lexing.from_string str in
  snd @@ Lexer.parse lexbuf

let assert_equal tests =
  List.iter
    (fun (expected, input) ->
     assert_equal ~printer:string_of_document expected (parse input))
    tests

let wrap_assert wrapper test =
  List.map (fun (e, i) -> wrapper e, i) test |> assert_equal

let emp = ["**";"//";"~~";"__"]

let mk_emp emp x = match emp with
  | "**" -> Bold x
  | "//" -> Italic x
  | "~~" -> Strike x
  | "__" -> Underline x
  | _ as s -> failwith ("not a emphasis mark: " ^ s)


let combine l1 l2 =
  List.fold_left (fun acc x -> List.rev_append acc
                               @@ List.map (fun y -> (x,y)) l2) [] l1

(* TESTS *)

let empty _ =
  wrap_assert (fun x -> x) [[],"";[],"\n\n"]

let title _ =
  wrap_assert
    (fun (level, content) -> [Title(level, content)])
    [
      (3, [Plain "Lorem"]), "=== Lorem === ";
      (3, [Plain "Lorem"]), "=== Lorem ===";
      (3, [Plain "Lorem "; Plain"Ipsum"]), "=== Lorem \nIpsum===";
      (3, [Plain "Lorem"; LineBreak; Plain"Ipsum"]), "=== Lorem  \nIpsum===";
      (3, [Plain "Lorem ="]), "=== Lorem ==== ";
      (3, [Plain "Lorem ="]), "=== Lorem = === ";
      (3, [Plain "= Lorem ="]), "=== = Lorem = === ";
      (1, [Plain "Lorem"]), "= Lorem";
      (1, []), "=";
      (1, []), "= =";

      (1, [Plain "Lo";Sup[Plain"rem"]]), "= Lo^{rem} = ";
    ]

let paragraph _ =
  wrap_assert
    (fun x -> [Paragraph x])
    [[Plain "Lorem ipsum."], "  Lorem ipsum.";
     [Plain "Lorem "; Plain "ipsum."], "  Lorem \nipsum.";
     [Plain "Lorem"; LineBreak; Plain "ipsum."],"  Lorem  \nipsum."]

let emphasis _ =

  wrap_assert
    (fun x -> [Paragraph x])
  @@ List.map (fun e -> ([mk_emp e[Plain"Lorem"];Plain"Ipsum"],
                        e^"Lorem"^e^"Ipsum")) emp;

  wrap_assert
    (fun x -> [Paragraph x])
  @@ List.fold_left
       begin
         fun acc (emp, emp') ->
         if emp = emp'
         then acc
         else ([mk_emp emp[Plain"Lorem";mk_emp emp'[Plain"Ipsum"]]],
               emp^"Lorem"^emp'^"Ipsum"^emp'^emp) :: acc
       end
       [] (combine emp emp);

  assert_raises Parser.Error (fun () -> parse "**//Lorem** ipsum.//");
  assert_raises Parser.Error (fun () -> parse "//**Lorem// ipsum.**");
  assert_raises Parser.Error (fun () -> parse "~~//Lorem~~ ipsum.//");
  assert_raises Parser.Error (fun () -> parse "__//Lorem__ ipsum.//");

  (* unterminated emphasis *)
  List.iter
    (fun emp -> assert_raises Parser.Error (fun () -> parse (emp^"lorem")))
    emp

let sup_sub _ =

  let mk_tests glue opening closing =
    List.map
      (fun (str, doc) ->
       [Paragraph[Plain"Lorem";doc;Plain" ipsum."]],
       "Lorem" ^ glue ^ opening ^ str ^ closing ^ " ipsum." ) in

  let mk_sup_brack_tests list =
    List.map (fun (i, x) -> i, Sup x) list |> mk_tests "^" "{" "}"
  and mk_sub_brack_tests list =
    List.map (fun (i, x) -> i, Sub x) list |> mk_tests "_" "{" "}"
  and mk_sup_tests list =
    List.map (fun (i, x) -> i, Sup x) list |> mk_tests "^" "" ""
  and mk_sub_tests list =
    List.map (fun (i, x) -> i, Sub x) list |> mk_tests "_" "" ""  in

  let brack_expected = [
    "42", [Plain "42"];

    "x_{i}", [Plain"x";Sub[Plain"i"]];
    "x^{i}", [Plain"x";Sup[Plain"i"]];
    "x_i", [Plain"x";Sub[Plain"i"]];
    "x^2", [Plain"x";Sup[Plain"2"]];

    "\\{", [Plain"{"];
    "\\}", [Plain"}"];

    "{{}}", [InlineSource""]

  ] @ List.map (fun e -> e^"lorem"^e, [mk_emp e[Plain"lorem"]]) emp


  and expected =
    List.map (fun x -> x, [Plain x]) ["i"; "2"; "}"]

  in
  assert_equal @@ mk_sup_brack_tests brack_expected;
  assert_equal @@ mk_sub_brack_tests brack_expected;
  assert_equal @@ mk_sup_tests expected;
  assert_equal @@ mk_sub_tests expected;

  (* curly brackets allow to nest emphasis with same mark *)
  wrap_assert (fun x -> [Paragraph x])
  @@ List.concat
  @@ List.map (fun e ->
               [[mk_emp e[Plain"L";Sup[mk_emp e[Plain"i"]]]],
                (e^"L^{"^e^"i"^e^"}"^e);
                [mk_emp e[Plain"L";Sub[mk_emp e[Plain"i"]]]],
                (e^"L_{"^e^"i"^e^"}"^e)])
              emp;

  assert_raises (Failure"lexing: empty token")(fun()->parse"Lorem_{ipsum");
  assert_raises (Failure"lexing: empty token")(fun()->parse"Lorem^{ipsum")

let inline_code _ =

  wrap_assert
    (fun x -> [Paragraph x])
    [
      [InlineCode "Lorem"; Plain " ipsum."], "``Lorem`` ipsum.";
      [InlineCode "Lorem ipsum"], "``Lorem \nipsum``"
    ]
    
let code_block _ =
  assert_equal [
      [CodeBlock "let lorem = ipsum"], "```let lorem = ipsum```";
      [CodeBlock "let ```lorem = ipsum"], "```let \\```lorem = ipsum```";
      [CodeBlock "let lorem = ipsum\nand ipsum = lorem"],
      "```\nlet lorem = ipsum\nand ipsum = lorem\n```"
    ]

let inline_source _ =
  assert_equal [
      [Paragraph [InlineSource "<b>Lorem <i>ipsum</i>.</b>"]],
      "{{<b>Lorem <i>ipsum</i>.</b>}}";
      
      [Paragraph [InlineSource "<b>Lorem <i>ipsum</i>.</b>"]],
      "{{<b>Lorem \n<i>ipsum</i>.</b>}}"
    ]
               
let source_block _ =
  assert_equal [
      [SourceBlock "<p>Lorem ipsum.</p>"],
      "{{{\n<p>Lorem ipsum.</p>\n}}}";
      
      [SourceBlock "<p>Lorem\nipsum.</p>"],
      "{{{\n<p>Lorem\nipsum.</p>\n}}}";
      
      [SourceBlock "<p>Lorem ipsum.</p>}}}"],
      "{{{\n<p>Lorem ipsum.</p>}}\\}\n}}}"
    ]
               
let math _ =
  assert_equal [
      [Paragraph[InlineMath[Plain"Lorem";Sup[Plain "2"];Plain" ipsum."]]],
      "$$Lorem^2 ipsum.$$";
      
      [MathBlock [Plain "Lorem"; Sub [Plain "i"]; Plain " ipsum."]],
      "$$$Lorem_i ipsum.$$$";
    ];

  (* no emphasis allowed in math block *)
  List.iter
    (fun emp -> assert_raises (Failure "TODO: raise Parser.Error")
                              (fun () -> parse ("$$Lor"^emp^"em"^emp^"$$")))
    emp;

  (* eof is not allowed in inline_math *)
  assert_raises (Failure "TODO: raise Parser.Error")
                (fun () -> parse "$$Lorem")

let list_t _ =
  wrap_assert
    (fun (o, c) -> [List(o,c)])
    [
      (true,[Item([Plain"Lorem"],
                  Some (true,
                        [Item([Plain"ipsum"], None);
                         Item([Plain"dolor ##sit"], None)]));
             Item([Plain"amet"], None)]),
      "# Lorem\n##ipsum\n##dolor ##sit\n#amet";
      
      (false, [Item([Plain"Lorem"],
                    Some (false,
                          [Item([Plain"ipsum"], None);
                           Item([Plain"dolor --sit"], None)]));
               Item([Plain"amet"], None)]),
      "- Lorem\n--ipsum\n--dolor --sit\n-amet";
      
      (true, [Item([Plain"Lorem"],
                   Some (false,
                         [Item([Plain"ipsum"], None)]));
              Item([Plain"dolor"], None)]),
      "# Lorem\n--ipsum\n#dolor";

      (false, [Item([Plain"Lorem"],
                    Some (true,
                          [Item([Plain"ipsum"], None)]));
               Item([Plain"dolor"], None)]),
      "- Lorem\n##ipsum\n-dolor";

      (false, [Item([],None);
               Item([],None)]),
      "-\n-";
    ];
  
  assert_raises
    (Failure "List error")
    (fun () -> parse "- Lorem\n---ipsum");
  assert_raises
    (Failure "List error")
    (fun () -> parse "- Lorem\n#ipsum")
    
let table _ =
  wrap_assert
    (fun (h, x) -> [Table (h, x)])
    [
      (None, [[[Plain "* Cell 1.1"]]]), "| * Cell 1.1 |\n";
      
      (None, [[[Plain "Cell 1"; Sup [Plain "1"]];[Plain "Cell 1.2"]];
              [[Plain "Cell 2.1"];[Plain "Cell "; Bold [Plain "2.2"]]]]),
      "| Cell 1^1 | Cell 1.2 |\n| Cell 2.1 | Cell **2.2** |";
      
      (None, [[[Plain "Cell 1.1"];[Plain "Cell 1.2"]];
              [[Plain "Cell 2.1"];[Plain "Cell 2.2"]]]),
      "| Cell 1.1 | Cell 1.2 |\n| Cell 2.1 | Cell 2.2 |";

      (Some [[Plain "Cell 1.1"];[Plain "Cell 1.2"]],
       [[[Plain "Cell 2.1"];[Plain "Cell 2.2"]]]),
      "| Cell 1.1 | Cell 1.2 |\n+--+\n| Cell 2.1 | Cell 2.2 |";

      (None, [[[];[]];[[];[]]]),
      "|  |  |\n|  |  |";
    ];
  
  List.iter 
    (fun i -> assert_raises Parser.Error (fun () -> parse i))
    [
      "| Cell 1.1 | Cell 1.2 |\n+---------+";
      "| Cell 1.1 | Cell 1.2\n";
      "| Cell 1.1 | Cell 1.2";
    ]

let escape _ =
  wrap_assert
    (fun x -> [Paragraph [Plain x]])
    ["-", "\\-";
     "#", "\\#";
     "|", "\\|";
     "=", "\\=";
     "`", "\\`";
     "{", "\\{";
     "$", "\\$";
     "**", "\\**";
     "//", "\\//";
     "__", "\\__";
     "~~", "\\~~";
     "^", "\\^";
     "\\", "\\\\";
    ];

  assert_equal
    [[Paragraph [Plain "-"; Plain "#"; Plain "|"; Plain "=`{$**//__~~^\\"]],
     "\\-\n\\#\n\\|\n\\=\\`\\{\\$\\**\\//\\_\\_\\~~\\^\\"];

  assert_raises
    (Failure "Illegal backslash escape: n")
    (fun () -> parse "\\not escapable")

let link _ =
  wrap_assert
    (fun x -> [Paragraph x])
    [
      [Link("foo", [])],
      "[[foo]]";
      
      [Plain "foo "; Link("bar", [])],
      "foo [[bar]]";
      
      [Link("foo[]", [])],
      "[[foo[] ]]";
      
      [Link("foo", [Plain "bar"])],
      "[[foo<<bar]]";
      
      [Link("foo", [Plain "bar"])],
      "[[ foo << bar ]]";
      
      [Link("foo", [Bold[Plain "bar"]])],
      "[[ foo << **bar** ]]";
      
      [Link("foo", [Bold[Plain "bar"]; Plain " bu"])],
      "[[ foo << **bar** bu ]]";
      
      [Link("foo<<||bar]]", [])],
      "[[ foo\\<<\\||bar\\]\\]]]";
      
      [Link("foo", [Plain "bar "; Plain "bu"])],
      "[[ foo << bar \n           bu]]";
      
      [Link("foo/bar", [Plain "bu"])],
      "[[ foo\n   /bar << bu]]";
      
      [Link("foo<<bar", [Plain "bu"])],
      "[[ foo\n   \\<<bar << bu]]"
    ]

let image _ =
  wrap_assert
    (fun x -> [Paragraph x])
    [
      [Image("foo", "bar")],
      "[[foo||bar]]";
      
      [Image("foo", "bar")],
      "[[ foo || bar ]]";
      
      [Image("foo", "bar bu")],
      "[[ foo || bar bu ]]";
      
      [Image("foo|", "bar]")],
      "[[foo\\|||bar\\]]]";
      
      [Link("foo", [Image("bar", "bu")])],
      "[[foo<<[[bar||bu]]]]"
    ]

(* FIXME: this does not test that foo is correctly redefined *)
let url_definition _ =
  assert_equal [
    [], "[[ foo ]] :: [[bar]]";
    [], "[[ foo ]] \n:: [[bar]]";
    [], "[[ foo ]] ::\n [[bar]]";
    [], "[[ foo ]]\n ::\n [[bar]]";
  ]

let html_entitie _ =
  wrap_assert
    (fun x -> [Paragraph x])
    [
      [Plain "x "; HTMLEntitie "isin"; Plain " N"],
      "x &isin; N";
      
      [Plain "x "; HTMLEntitie "#60"; Plain " y"],
      "x &#60; y";
      
      [Plain "x"; HTMLEntitie "sup2"; Plain " = y"],
      "x&sup2; = y";
      
      [Plain "& loremp ipsum;"],
      "& loremp ipsum;";
      
      [Plain "&#loremp; &42;"],
      "&#loremp; &42;"
    ]

let greek_letter _ =
  wrap_assert
    (fun x -> [Paragraph x])
    [
      [GreekLetter 'a'; Plain "+"; GreekLetter 'b'],
      "&a+&b";
      
      [GreekLetter 'a'; Plain "+"; GreekLetter 'b'; Plain "+1;"],
      "&a+&b+1;"
    ]

let extern _ =
  assert_equal [
      [ExternRender("dot", "graph g{\na -- b -- c;\nb -- d;\n}")],
        "%%%dot graph g{\na -- b -- c;\nb -- d;\n}%%%";
      
      [ExternRender("dot", "graph g{\na -- b -- c;\nb -- d;\n}")],
        "%%%dot\ngraph g{\na -- b -- c;\nb -- d;\n}\n%%%";  

      [ExternRender("foo", "%%% %%%")],
        "%%%foo \\%%% %%\\%%%%";
      
      [Paragraph [Plain "%%% dot"]],
      "%%% dot"
    ]

let comment _ =
  assert_equal [
      [CommentBlock"comment"],"<<<comment>>>";
      [CommentBlock"com\nment"],"<<<com\nment>>>";
      [CommentBlock"comment>>> "],"<<<comment\\>>> >>>";
    ]
                               
let suite = 
  "Suite" >:::
    ["Empty document" >:: empty;
     "Title" >:: title;
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
     "Comment" >:: comment;]

let _  =
  run_test_tt_main suite
