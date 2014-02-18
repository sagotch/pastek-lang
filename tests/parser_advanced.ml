open OUnit
open Type
open Lexer

(* The purpose of this file is to assert that two following blocks
 * are parsed as expected according to separating carriage returns *)

let assert_equal = assert_equal ~printer:string_of_document

let parse str =
  let lexbuf = Lexing.from_string str in
  snd @@ Lexer.parse lexbuf

let emptyline = ["\n\n"]
let eol = "\n" :: emptyline
let any_sep = "" :: eol

let title = "= Lorem ="
let eof = ""
let ulist = "-Lorem"
let olist = "#Lorem"
let code_block = "```Lorem```"
let source_block = "{{{Lorem}}}"
let table = "|Lorem|"
let math_block = "$$$Lorem$$$"
let paragraph = "Ispum"
let ext1 = "%%%Lorem\nLorem%%%"
let ext2 = "%%%Lorem Lorem%%%"


let assert_two_blocks str str' separators =
  List.iter
    (fun d -> assert_equal (parse str @ parse str') (parse (str ^ d ^ str')))
    separators

let title_followers _ =

  let assert_two_blocks = assert_two_blocks title in

  (* eof *)
  assert_two_blocks eof any_sep;

  (* title *)
  assert_two_blocks title eol;
  assert_equal [Title(1,[Plain"Lorem == Lorem"])] (parse("=Lorem == Lorem"));

  (* list *)
  assert_two_blocks ulist eol;
  assert_two_blocks olist eol;
  assert_equal [Title(1,[Plain"Lorem -Lorem"])] (parse("=Lorem -Lorem"));
  assert_equal [Title(1,[Plain"Lorem -Lorem"])] (parse("=Lorem -Lorem"));

  (* code block *)
  assert_two_blocks code_block any_sep;

  (* source block *)
  assert_two_blocks source_block any_sep;

  (* table *)
  assert_two_blocks table eol;

  (* math block *)
  assert_two_blocks math_block any_sep;
  
  (* paragraph*)
  assert_two_blocks paragraph emptyline;
  assert_equal [Title(1,[Plain"Lorem Lorem"])] (parse("=Lorem Lorem"));
  assert_equal [Title(1,[Plain"Lorem";Plain"Lorem"])] (parse("=Lorem\nLorem"));

  (* ext *)
  assert_two_blocks ext1 any_sep;
  assert_two_blocks ext2 any_sep

let list_followers _ =

  let common_assert assert_two_blocks =

    (* eof *)
    assert_two_blocks eof any_sep;

    (* title *)
    assert_two_blocks title eol;

    (* list *)
    assert_two_blocks ulist emptyline;
    assert_two_blocks olist emptyline;

    (* code block *)
    assert_two_blocks code_block any_sep;

    (* source block *)
    assert_two_blocks source_block any_sep;

    (* table *)
    assert_two_blocks table eol;

    (* math block *)
    assert_two_blocks math_block any_sep;
    
    (* paragraph*)
    assert_two_blocks paragraph emptyline;

    (* ext *)
    assert_two_blocks ext1 any_sep;
    assert_two_blocks ext2 any_sep

  in

  (* Unordered list *)
  common_assert (assert_two_blocks ulist);
  assert_equal [List(false,[Item([Plain"Lorem =Lorem"],None)])]
               (parse("-Lorem =Lorem"));
  assert_equal [List(false,[Item([Plain"Lorem"],
                                 Some(false,[Item([Plain"Lorem"],None)]))])]
               (parse("-Lorem\n--Lorem"));
  assert_equal [List(false,[Item([Plain"Lorem --Lorem"],None)])]
               (parse("-Lorem --Lorem"));

  assert_equal [List(false,[Item([Plain"Lorem Lorem"],None)])]
               (parse("-Lorem Lorem"));
  assert_equal [List(false,[Item([Plain"Lorem";Plain"Lorem"],None)])]
               (parse("-Lorem\nLorem"));

  (* Ordered list *)
  common_assert (assert_two_blocks olist)

let paragraph_followers _ =

  let assert_two_blocks = assert_two_blocks paragraph in

  (* eof *)
  assert_two_blocks eof any_sep;

  (* title *)
  assert_two_blocks title eol;
  assert_equal [Paragraph[Plain"Lorem == Lorem"]]
               (parse("Lorem == Lorem"));

  (* list *)
  assert_two_blocks ulist eol;
  assert_two_blocks olist eol;
  assert_equal [Paragraph[Plain"Lorem -Lorem"]] (parse("Lorem -Lorem"));
  assert_equal [Paragraph[Plain"Lorem -Lorem"]] (parse("Lorem -Lorem"));

  (* code block *)
  assert_two_blocks code_block any_sep;

  (* source block *)
  assert_two_blocks source_block any_sep;

  (* table *)
  assert_two_blocks table eol;

  (* math block *)
  assert_two_blocks math_block any_sep;
  
  (* paragraph*)
  assert_two_blocks paragraph emptyline;
  assert_equal [Paragraph[Plain"Lorem Lorem"]] (parse("Lorem Lorem"));
  assert_equal [Paragraph[Plain"Lorem";Plain"Lorem"]] (parse("Lorem\nLorem"));

  (* ext *)
  assert_two_blocks ext1 any_sep;
  assert_two_blocks ext2 any_sep

let suite = 
  "Suite" >:::
    ["Title followers" >:: title_followers;
     "List followers" >:: list_followers;
     "Paragraph followers" >:: paragraph_followers]

let _ =
  run_test_tt_main suite
