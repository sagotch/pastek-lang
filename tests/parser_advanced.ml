open OUnit
open Type
open Lexer

let assert_equal = assert_equal ~printer:string_of_document
let parse str =
  let lexbuf = Lexing.from_string str in
  snd @@ Lexer.parse lexbuf

let emptyline = ["\n\n"]
let eol = "\n" :: emptyline
let any_sep = "" :: eol

let check expected str expected' str' separators =
  List.iter
    (fun d -> assert_equal (expected :: expected') (parse (str ^ d ^ str')))
    separators

let title_followers _ =

  let check =
    check (Title(1,[Plain "Lorem"])) "= Lorem =" in

  (* eof *)
  check [] "" any_sep;

  (* title *)
  check [Title(2,[Plain"Ipsum"])] " ==   Ipsum" eol;

  (* list *)
  check [List(false,[Item([Plain "Ipsum"],None)])] "-Ipsum" eol;
  check [List(true,[Item([Plain "Ipsum"],None)])] "#Ipsum" eol;

  (* code block *)
  check [CodeBlock "Ipsum"] "```Ipsum```" any_sep;

  (* source block *)
  check [SourceBlock "Ipsum"] "{{{Ipsum}}}" any_sep;

  (* table *)
  check [Table(None, [[[Plain"Ipsum"]]])] "|Ipsum|" eol;

  (* math block *)
  check [MathBlock[Plain"Ipsum"]] "$$$Ipsum$$$" any_sep;
  
  (* paragraph*)
  check [Paragraph[Plain"Ipsum"]] "Ipsum" emptyline;

  (* ext *)
  check [ExternRender("Lorem", "Ipsum")] "%%%Lorem\nIpsum%%%" any_sep;
  check [ExternRender("Lorem", " Ipsum")] "%%%Lorem Ipsum%%%" any_sep

let suite = 
  "Suite" >:::
    ["Title followers" >:: title_followers]

let _ =
  run_test_tt_main suite
