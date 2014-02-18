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

let check expected str expected' str' separators =
  List.iter
    (fun d -> assert_equal (expected @ expected') (parse (str ^ d ^ str')))
    separators

let title_followers _ =

  let title = "= Lorem =" in

  let check str =
    check (parse title) title (parse str) str in

  (* eof *)
  check "" any_sep;

  (* title *)
  check " ==   Ipsum" eol;
  assert_equal [Title(1,[Plain"Lorem == Ipsum"])]
               (parse("=Lorem == Ipsum"));

  (* list *)
  check "-Ipsum" eol;
  check "#Ipsum" eol;
  assert_equal [Title(1,[Plain"Lorem -Ipsum"])]
               (parse("=Lorem -Ipsum"));
  assert_equal [Title(1,[Plain"Lorem -Ipsum"])]
               (parse("=Lorem -Ipsum"));

  (* code block *)
  check "```Ipsum```" any_sep;

  (* source block *)
  check "{{{Ipsum}}}" any_sep;

  (* table *)
  check "|Ipsum|" eol;
  (* '|' is not allowed as a character *)

  (* math block *)
  check "$$$Ipsum$$$" any_sep;
  
  (* paragraph*)
  check "Ipsum" emptyline;
  assert_equal [Title(1,[Plain"Lorem Ipsum"])]
               (parse("=Lorem Ipsum"));
  assert_equal [Title(1,[Plain"Lorem";Plain"Ipsum"])]
               (parse("=Lorem\nIpsum"));

  (* ext *)
  check "%%%Lorem\nIpsum%%%" any_sep;
  check "%%%Lorem Ipsum%%%" any_sep

let list_followers _ =

  let list = "- Lorem" in

  let check str =
    check (parse list) list (parse str) str in

  (* eof *)
  check "" any_sep;

  (* title *)
  check "=Ipsum" eol;
  assert_equal [List(false,[Item([Plain"Lorem =Ipsum"],None)])]
               (parse("-Lorem =Ipsum"));

  (* list *)
  check "--Ipsum" emptyline;
  assert_equal [List(false,[Item([Plain"Lorem"],
                                 Some(false,[Item([Plain"Ipsum"],None)]))])]
               (parse("-Lorem\n--Ipsum"));
  assert_equal [List(false,[Item([Plain"Lorem --Ipsum"],None)])]
               (parse("-Lorem --Ipsum"));

  (* code block *)
  check "```Ipsum```" any_sep;

  (* source block *)
  check "{{{Ipsum}}}" any_sep;

  (* table *)
  check "|Ipsum|" eol;
  (* '|' is not allowed as a character *)

  (* math block *)
  check "$$$Ipsum$$$" any_sep;
  
  (* paragraph*)
  check "Ipsum" emptyline;
  assert_equal [List(false,[Item([Plain"Lorem Ipsum"],None)])]
               (parse("-Lorem Ipsum"));
  assert_equal [List(false,[Item([Plain"Lorem";Plain"Ipsum"],None)])]
               (parse("-Lorem\nIpsum"));

  (* ext *)
  check "%%%Lorem\nIpsum%%%" any_sep;
  check "%%%Lorem Ipsum%%%" any_sep

let suite = 
  "Suite" >:::
    ["Title followers" >:: title_followers;
     "List followers" >:: list_followers]

let _ =
  run_test_tt_main suite
