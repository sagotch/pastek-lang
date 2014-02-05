open Lexer
open Render
open Render_html

let usage = "usage: pastek_core < input > output"

(* Quick and dirty fix to use Menhir with token list *)
let ast =
  let lexbuf = Lexing.from_channel stdin in
  Lexer.parse lexbuf

let _ =
  let module M = Render(Render_html) in
  M.render_ast ast;
  output_string stdout @@ M.get_buffer_content ()
