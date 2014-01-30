open AST
open Render

module Render_html : Content_render = struct

  let buffer = Buffer.create 0

  let add_char = Buffer.add_char buffer

  let add_string = Buffer.add_string buffer
                                     
  let add_strings = List.iter add_string

  let rec render_inlines inlines =
    let render_inline = function
      | Plain(str) -> add_string str
      | InlineCode(str) -> add_string str
      | InlineSource(str) -> add_string str
      | InlineMath(inlines) ->
         add_string "<span class=\"inline_math\">";
         render_inlines inlines;
         add_string "<span class=\"inline_math\">";
      | Bold(inlines) ->
         add_string "<b>";
         render_inlines inlines;
         add_string "</b>"
      | Italic(inlines) ->
         add_string "<i>";
         render_inlines inlines;
         add_string "</i>"
      | Underline(inlines) ->
         add_string "<u>";
         render_inlines inlines;
         add_string "</u>"
      | Strike(inlines) ->
         add_string "<del>";
         render_inlines inlines;
         add_string "</del>"
      | Sup(c) -> add_string "<sup>"; add_char c; add_string "</sup>"
      | Sub(c) -> add_string "<sub>"; add_char c; add_string "</sub>"
    in List.iter render_inline inlines

  and render_title lvl inlines =
    let lvl =
      if lvl > 6
      then (prerr_string
              ("HTML only supports 6 title levels, turning "
               ^ string_of_int lvl ^ " in <h6>."); 6)
      else lvl in
    add_strings ["<h"; string_of_int lvl; ">\n"];
    render_inlines inlines;
    add_strings ["\n</h"; string_of_int lvl; ">\n"]

  and render_paragraph inlines =
    add_string "<p>\n";
    render_inlines inlines;
    add_string "\n</p>\n"

  and render_math_block inlines =
    add_string "<p class=\"math_block\">\n";
    render_inlines inlines;
    add_string "\n</p>\n"

  and render_table headers content = 
    let render_table_line =
    List.iter (fun x -> add_string "<td>\n";
                        render_inlines x;
                        add_string "\n</td>\n") in
    let render_table_lines =
      List.iter (fun x -> add_string "<tr>\n";
                          render_table_line x;
                          add_string "</tr>\n") in
    add_string "<table>\n";
    (match headers with
       | None -> ()
       | Some header -> add_string "<th>\n";
                        render_table_line header;
                        add_string "</th>\n");
    render_table_lines content;
    add_string "</table>\n"

  and render_list (ord, items) =
    add_string @@ if ord then "<ol>\n" else "<ul>\n";
    render_items items;
    add_string @@ if ord then "</ol>\n" else "</ul>\n"
                                                         
  and render_items items =
    let aux item =
      let Item(inlines, child) = item in
      render_inlines inlines;
      match child with
      | None -> ()
      | Some child -> render_list child in
    List.iter
      (fun x -> add_string "<li>\n"; aux x; add_string "\n</li>\n")
      items

  and render_code_block data =
    add_strings ["<pre>\n<code>\n"; data; "</pre>\n</code>\n"]

  and render_source_block data =
    add_string data

end
