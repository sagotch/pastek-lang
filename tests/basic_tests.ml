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

let suite = 
  "Suite" >:::
    ["Title" >:: simple_title;
     "Paragraph" >:: simple_paragraph;
     "Block suit" >:: block_suit;
     "Emphasis text" >:: emphasis]

let () =
  run_test_tt_main suite
