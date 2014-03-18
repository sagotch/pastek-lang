open Lexer
open Render_html

let full_document = ref false

let options = Arg.align [
  "--full-document", Arg.Set full_document, 
  " Generate a full standalone document.";
  "--translate-only", Arg.Clear full_document, 
  " Translate into target language only (default behavior).";
]

let usage = "usage: pastek [options] < input > output"

let _ = Arg.parse options (fun s -> ()) usage

let _ =
  let config, doc =
    let lexbuf = Lexing.from_channel stdin in
    try Lexer.parse lexbuf
    with Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let msg =
        "Parsing: \"" ^ Lexing.lexeme lexbuf
        ^ "\" - Line " ^ string_of_int pos.pos_lnum
        ^ ", column " ^ string_of_int (pos.pos_cnum - pos.pos_bol) in
      failwith msg
  in
  let r =
    new render_html
        begin
          if !full_document
          then Render.GenerateFullDoc 
          else Render.TranslateOnly
        end
        config in
  r#render_doc doc;
  output_string stdout r#get_render                             
