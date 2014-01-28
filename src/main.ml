open Parser
open Render
open Render_html

let usage = "usage: pastek_core < input > output"

(* Quick and dirty fix to use Menhir with token list *)
let ast =
  let lexbuf = Lexing.from_channel stdin in
  let tokens = ref @@ Lexer.line_beginning [] lexbuf in
  let token _ = 
    match !tokens with 
    | []     -> EOF 
    | h :: t -> tokens := t ; h 
  in Parser.document token @@ Lexing.from_string ""

let _ =
  let module M = Render(Render_html) in
  M.render_ast ast;
  output_string stdout @@ M.get_buffer_content ()
