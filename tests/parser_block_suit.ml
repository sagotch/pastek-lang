open OUnit
open Type
open Lexer

(* The purpose of this file is to assert that two following blocks
 * are parsed as expected according to separating carriage returns *)

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
let comment = "<<<Lorem>>>"

let parse str =
  let lexbuf = Lexing.from_string str in
  snd @@ Lexer.parse lexbuf

let assert_not_equal str str' separators =
  List.iter
    begin
      fun d ->
      try
        let p1 = parse str @ parse str'
        and p2 = parse (str ^ d ^ str') in
        assert_bool
          (str ^ d ^ str' ^ ": equals " ^ string_of_document p1)
          (p1 <> p2)
      with Failure _ | Parser.Error-> assert_bool "" true
    end
    separators

let assert_equal str str' separators =  
  let assert_equal = assert_equal ~printer:string_of_document in
  List.iter
    (fun d -> assert_equal (parse str @ parse str') (parse (str ^ d ^ str')))
    separators

let delim_not_in l =
  List.filter (fun x -> not (List.mem x l)) any_sep

module type Test_battery = sig

    val testing : string

    val eof : string list
    val title : string list
    val ulist : string list
    val olist : string list
    val code_block : string list
    val source_block : string list
    val table : string list
    val math_block : string list
    val paragraph : string list
    val ext1 : string list
    val ext2 : string list
    val comment : string list

end

module Test = functor (B : Test_battery) ->
  struct
    let run_tests =
      let assert_equal = assert_equal B.testing 
      and assert_not_equal = assert_not_equal B.testing 
      in
      let test t d =
        assert_equal t d;
        assert_not_equal t (delim_not_in d) in
      test eof B.eof;
      test title B.title;
      test ulist B.ulist;
      test olist B.olist;
      test code_block B.code_block;
      test source_block B.source_block;
      test table B.table;
      test math_block B.math_block;
      test paragraph B.paragraph;
      test ext1 B.ext1;
      test ext2 B.ext2
  end

let title_followers _ =
  let module T =
    Test (struct
           let testing = title
                       
           let eof = any_sep
           let title = eol
           let ulist = eol
           let olist = eol
           let code_block = eol
           let source_block = eol
           let table = eol
           let math_block = eol
           let paragraph = emptyline
           let ext1 = eol
           let ext2 = eol
           let comment = eol
         end)
  in T.run_tests

let list_followers _ =
  let mk_tests testing = 
    let module T = Test(struct
                         let testing = testing
                                         
                         let eof = any_sep
                         let title = eol
                         let ulist = emptyline
                         let olist = emptyline
                         let code_block = eol
                         let source_block = eol
                         let table = eol
                         let math_block = eol
                         let paragraph = emptyline
                         let ext1 = eol
                         let ext2 = eol
                         let comment = eol
                       end)
    in T.run_tests
  in
  mk_tests ulist;
  mk_tests olist

let paragraph_followers _ =

  let module T =
    Test (struct
           let testing = paragraph
                       
           let eof = any_sep
           let title = eol
           let ulist = eol
           let olist = eol
           let code_block = eol
           let source_block = eol
           let table = eol
           let math_block = eol
           let paragraph = emptyline
           let ext1 = eol
           let ext2 = eol
           let comment = eol
         end)
  in T.run_tests

let delimited_blocks _ =

  let mk_tests testing = 
    let module T = Test(struct
                         let testing = testing
                                         
                         let eof = any_sep
                         let title = eol
                         let ulist = eol
                         let olist = eol
                         let code_block = eol
                         let source_block = eol
                         let table = eol
                         let math_block = eol
                         let paragraph = eol
                         let ext1 = eol
                         let ext2 = eol
                         let comment = eol
                       end)
    in T.run_tests
  in
  mk_tests code_block;
  mk_tests source_block;
  mk_tests math_block;
  mk_tests ext1;
  mk_tests ext2;
  mk_tests comment

let suite = 
  "Suite" >:::
    ["Title followers" >:: title_followers;
     "List followers" >:: list_followers;
     "Paragraph followers" >:: paragraph_followers;
     "Delimited blocks" >:: delimited_blocks]

let _ =
  run_test_tt_main suite
