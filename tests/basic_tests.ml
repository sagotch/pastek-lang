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


let suite = 
  "Suite" >:::
    ["Title test" >:: simple_title;
     "Paragraph test" >:: simple_paragraph;
     "Block Suit" >:: block_suit]

let () =
  run_test_tt_main suite
