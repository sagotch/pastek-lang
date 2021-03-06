open Type
open Buffer
open Toml

type mode = TranslateOnly | GenerateFullDoc

class virtual render (mode : mode) (config : Toml.table) =
  object (self)
    (** Functions of this object are expected to write rendering in
     *  the buffer nammed buffer *)
    val buffer = Buffer.create 0
    val config = config
    val mode = mode

    method virtual render_title : int -> inline list -> unit
    method virtual render_paragraph : inline list -> unit
    method virtual render_table :
             inline list list option -> inline list list list -> unit
    method virtual render_list : list_t -> unit
    method virtual render_math_block : inline list -> unit
    method virtual render_code_block : string -> unit
    method virtual render_source_block : string -> unit
    method virtual render_extern : string -> string -> unit
    method virtual render_comment : string -> unit

    method virtual pre_render : unit -> unit
    method virtual post_render : unit -> unit

    method render_doc doc =
      self#pre_render ();
      List.iter
        (function
          | Title(level, inlines) -> self#render_title level inlines
          | Paragraph(inlines) -> self#render_paragraph inlines
          | Table(headers, content) -> self#render_table headers content
          | List(li) -> self#render_list li
          | MathBlock(inlines) -> self#render_math_block inlines
          | CodeBlock(data) -> self#render_code_block data
          | SourceBlock(data) -> self#render_source_block data
          | ExternRender(cmd, data) -> self#render_extern cmd data
          | CommentBlock(comment) -> self#render_comment comment)
        doc;
      self#post_render ()

    method get_render = Buffer.contents buffer
  end
