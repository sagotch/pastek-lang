open AST
open Buffer

module type Content_render = sig
    (** Functions of this module are expected to write rendering in
     *  the buffer nammed buffer *)
    val buffer : Buffer.t
    val render_title : int -> inline list -> unit
    val render_paragraph : inline list -> unit
    val render_math_block : inline list -> unit
    val render_table : inline list option -> inline list list list -> unit
    val render_list : list_t -> unit
    val render_code_block : string -> unit
    val render_source_block : string -> unit
  end
  
module Render =
  functor (R : Content_render) ->
struct
  open R
  let render_ast =
    List.iter
      (function
        | Title(level, inlines) -> render_title level inlines
        | Paragraph(inlines) -> render_paragraph inlines
        | MathBlock(inlines) -> render_math_block inlines
        | Table(headers, content) -> render_table headers content
        | List(li) -> render_list li
        | CodeBlock(data) -> render_code_block data
        | SourceBlock(data) -> render_source_block data)

  let get_buffer_content = fun () -> Buffer.contents R.buffer
end
