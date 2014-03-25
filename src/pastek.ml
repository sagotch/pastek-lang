open Lexer
open Render_html
open Toml

let version = "2014.03.24"

let full_document = ref false
let no_default = ref false
let config_file = ref ""

let options = Arg.align [
  "--full-document", Arg.Set full_document, 
  " Generate a full standalone document.";

  "--translate-only", Arg.Clear full_document, 
  " Translate into target language only (default behavior).";

  "--no-default-config", Arg.Set no_default, 
  " Do not use default (~/.pastek/default_config.toml) file.";

  "--config", Arg.Set_string config_file, 
  "FILE Use FILE as configuration file.";

  "--version", Arg.Unit (fun () ->
                         print_endline ("pastek version: " ^ version);
                         exit 0), 
  " Print pastek version and exit.";
]

let usage = "usage: pastek [options] < input > output"

let _ = Arg.parse options (fun s -> ()) usage

let _ =
  
  let parsing_err lexbuf = 
    let pos = Lexing.lexeme_start_p lexbuf in
    let msg =
      "Parsing: \"" ^ Lexing.lexeme lexbuf
      ^ "\" - Line " ^ string_of_int pos.Lexing.pos_lnum
      ^ ", column " ^ string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
    in
    failwith msg
  
  in

  (* Parse stdin *)
  let local_config, doc =
    let lexbuf = Lexing.from_channel stdin in
    try Lexer.parse lexbuf
    with Parser.Error -> parsing_err lexbuf
  in

  (* Merge differents config tables. First load default configuration file, 
     then merge provided configuration file, and then merge local 
     configuration. (merging will erase previous values)
     NB: if file default_config.toml does not exists, it will produce
         failure. *)
  let config =
    if !no_default
    then Toml.create ()
    else Toml.from_filename
         @@ (Sys.getenv "HOME") ^ "/.pastek/default_config.toml" in

  if !config_file <> ""
  then Toml.rec_merge config
       @@ Toml.from_filename !config_file;

  Toml.rec_merge config local_config;

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
