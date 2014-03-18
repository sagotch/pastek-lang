open Type
open Render
open TomlType
open Unix

let greek_letters = [
  'a', "alpha";
  'b', "beta";
  'g', "gamma";
  'd', "delta";
  'e', "epsilon";
  'z', "zeta";
  't', "tau";
  'i', "iota";
  'k', "kappa";
  'l', "lambda";
  'm', "mu";
  'n', "nu";
  'o', "omicron";
  'p', "pi";
  'r', "rho";
  's', "sigma";
  'u', "upsilon";
  'c', "chi";
  'w', "omega";
  'y', "#968"
]

class render_html (mode : Render.mode) (config : TomlType.tomlTable) =
object(self)

  inherit render mode config as super

  method private add_char = Buffer.add_char buffer

  method private add_string = Buffer.add_string buffer

  method private esc_add_str str =
    let rep = [ "&", "&amp;"; "<", "&lt;"; ">", "&gt;";] in
    self#add_string
    @@ List.fold_left (fun str (reg, rep) ->
                       Str.global_replace (Str.regexp reg) rep str) str rep
                                     
  method private add_strings = List.iter self#add_string

  method private render_inlines = fun inlines ->
    let render_inline = function
      | Plain(str) -> self#esc_add_str str
      | InlineCode(str) ->
         self#add_string "<code>";
         self#esc_add_str str;
         self#add_string "</code>"
      | InlineSource(str) -> self#add_string str
      | InlineMath(inlines) ->
         self#add_string "<span class=\"inline_math\">";
         self#render_inlines inlines;
         self#add_string "</span>";
      | Bold(inlines) ->
         self#add_string "<b>";
         self#render_inlines inlines;
         self#add_string "</b>"
      | Italic(inlines) ->
         self#add_string "<i>";
         self#render_inlines inlines;
         self#add_string "</i>"
      | Underline(inlines) ->
         self#add_string "<u>";
         self#render_inlines inlines;
         self#add_string "</u>"
      | Strike(inlines) ->
         self#add_string "<del>";
         self#render_inlines inlines;
         self#add_string "</del>"
      | Sup(inlines) -> self#add_string "<sup>";
                        self#render_inlines inlines;
                        self#add_string "</sup>"
      | Sub(inlines) -> self#add_string "<sub>";
                        self#render_inlines inlines;
                        self#add_string "</sub>"
      | Image (url, txt) -> self#add_string "<img src=\"";
                            self#add_string url;
                            self#add_string "\" alt=\"";
                            self#add_string txt;
                            self#add_string "\" />"
      | Link (url, inlines) ->
         self#add_string "<a href=\"";
         self#add_string begin
           try Toml.get_string
                 (Toml.get_table config "__pastek_links_urls") url
           with Not_found -> url end;
         self#add_string "\">";
         if inlines = [] then self#add_string url
                         else self#render_inlines inlines;
         self#add_string "</a>"
      | HTMLEntitie e -> self#add_char '&';
                         self#add_string e;
                         self#add_char ';';
      | GreekLetter l ->
         self#add_char '&';
         let name =  List.assoc (Char.lowercase l) greek_letters in
         if Char.lowercase l = l
         then self#add_string name
         else self#add_string (String.capitalize name);
         self#add_char ';'
         
    in List.iter render_inline inlines

  method render_comment comment = ()

  method pre_render () = match mode with
    | Render.TranslateOnly -> ()
    | Render.GenerateFullDoc ->
       self#add_string "<!DOCTYPE html><html><head>";
       begin
         try List.iter
               (fun url ->
                self#add_strings
                       ["<link href=\""; url; "\" rel=\"stylesheet\">"])
             @@ Toml.get_string_list (Toml.get_table config "css") "urls"
         with Not_found -> ()
       end;
       self#add_string "</head><body>"

  method post_render () = match mode with
    | Render.TranslateOnly -> ()
    | Render.GenerateFullDoc -> self#add_string "</body></html>"

  method render_title lvl inlines =
    let lvl =
      if lvl > 6
      then (prerr_string
              ("HTML only supports 6 title levels, turning "
               ^ string_of_int lvl ^ " in <h6>."); 6)
      else lvl in
    self#add_strings ["<h"; string_of_int lvl; ">"];
    self#render_inlines inlines;
    self#add_strings ["</h"; string_of_int lvl; ">"]

  method render_paragraph inlines =
    self#add_string "<p>";
    self#render_inlines inlines;
    self#add_string "</p>"

  method render_math_block inlines =
    self#add_string "<p class=\"math_block\">";
    self#render_inlines inlines;
    self#add_string "</p>"

  method render_table headers content = 
    let render_table_line tag line =
      self#add_string "<tr>";
      List.iter (fun x -> self#add_string ("<" ^ tag ^ ">");
                          self#render_inlines x;
                          self#add_string ("</" ^ tag ^ ">")) line;
      self#add_string "</tr>" in
    self#add_string "<table>";
    (match headers with
     | None -> ()
     | Some header -> render_table_line "th" header);
    List.iter (fun x -> render_table_line "td" x) content;
    self#add_string "</table>"

  method render_list (ord, items) =
    self#add_string @@ if ord then "<ol>" else "<ul>";
    self#render_items items;
    self#add_string @@ if ord then "</ol>" else "</ul>"
                                                         
  method render_items items =
    let aux item =
      let Item(inlines, child) = item in
      self#render_inlines inlines;
      match child with
      | None -> ()
      | Some child -> self#render_list child in
    List.iter
      (fun x -> self#add_string "<li>"; aux x; self#add_string "</li>")
      items

  method render_code_block data =
    self#add_string "<pre><code>";
    self#esc_add_str data;
    self#add_string "</code></pre>"

  method render_source_block data =
    self#add_string data

  method render_extern cmd data =

    let input_to_string input =
      let rec loop buffer =
        match try Some ((input_line input) ^ "\n")
              with End_of_file -> None
        with
        | Some s -> Buffer.add_string buffer s; loop buffer
        | None -> Buffer.contents buffer
      in loop (Buffer.create 42) in

    let get_cmd_output cmd src =
      let (output, input) = Unix.open_process cmd in
      output_string input src;
      flush input;
      close_out input;
      let res = input_to_string output in
      close_in output;
      res in

    let cmd = Toml.get_string (Toml.get_table config "cmd") cmd
    in self#add_string @@ get_cmd_output cmd data

end
