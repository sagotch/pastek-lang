open Lexer
open Render_html

let usage = "usage: pastek_core < input > output"

let _ =
  let config, doc =
  let lexbuf = Lexing.from_channel stdin in
  Lexer.parse lexbuf in
  let r = new render_html config in
  r#render_doc doc;
  output_string stdout r#get_render
