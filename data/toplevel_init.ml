(* lock out stdout so that only the output from the toplevel can write there *)

module Pervasives = struct
  include Pervasives
  (* override stdout *)
  let stdout = stderr
  (* redefine everything that depended on it *)
  let print_char c = output_char stdout c
  let print_string s = output_string stdout s
  let print_int i = output_string stdout (string_of_int i)
  let print_float f = output_string stdout (string_of_float f)
  let print_endline s =
    output_string stdout s; output_char stdout '\n'; flush stdout
  let print_newline () = output_char stdout '\n'; flush stdout
  let read_line () = flush stdout; input_line stdin
  let read_int () = int_of_string(read_line())
  let read_float () = float_of_string(read_line())
end

open Pervasives

module Printf = struct
  include Printf
  let printf fmt = fprintf stdout fmt;;
end

module Format = struct
  include Format
  let std_formatter = formatter_of_out_channel Pervasives.stdout
  let open_hbox = pp_open_hbox std_formatter
  and open_vbox = pp_open_vbox std_formatter
  and open_hvbox = pp_open_hvbox std_formatter
  and open_hovbox = pp_open_hovbox std_formatter
  and open_box = pp_open_box std_formatter
  and close_box = pp_close_box std_formatter
  and open_tag = pp_open_tag std_formatter
  and close_tag = pp_close_tag std_formatter
  and print_as = pp_print_as std_formatter
  and print_string = pp_print_string std_formatter
  and print_int = pp_print_int std_formatter
  and print_float = pp_print_float std_formatter
  and print_char = pp_print_char std_formatter
  and print_bool = pp_print_bool std_formatter
  and print_break = pp_print_break std_formatter
  and print_cut = pp_print_cut std_formatter
  and print_space = pp_print_space std_formatter
  and force_newline = pp_force_newline std_formatter
  and print_flush = pp_print_flush std_formatter
  and print_newline = pp_print_newline std_formatter
  and print_if_newline = pp_print_if_newline std_formatter

  and open_tbox = pp_open_tbox std_formatter
  and close_tbox = pp_close_tbox std_formatter
  and print_tbreak = pp_print_tbreak std_formatter

  and set_tab = pp_set_tab std_formatter
  and print_tab = pp_print_tab std_formatter

  and set_margin = pp_set_margin std_formatter
  and get_margin = pp_get_margin std_formatter

  and set_max_indent = pp_set_max_indent std_formatter
  and get_max_indent = pp_get_max_indent std_formatter

  and set_max_boxes = pp_set_max_boxes std_formatter
  and get_max_boxes = pp_get_max_boxes std_formatter
  and over_max_boxes = pp_over_max_boxes std_formatter

  and set_ellipsis_text = pp_set_ellipsis_text std_formatter
  and get_ellipsis_text = pp_get_ellipsis_text std_formatter

  and set_formatter_out_channel =
    pp_set_formatter_out_channel std_formatter

  and set_formatter_output_functions =
    pp_set_formatter_output_functions std_formatter
  and get_formatter_output_functions =
    pp_get_formatter_output_functions std_formatter

  and set_all_formatter_output_functions =
    pp_set_all_formatter_output_functions std_formatter
  and get_all_formatter_output_functions =
    pp_get_all_formatter_output_functions std_formatter

  and set_formatter_tag_functions =
    pp_set_formatter_tag_functions std_formatter
  and get_formatter_tag_functions =
    pp_get_formatter_tag_functions std_formatter
  and set_print_tags =
    pp_set_print_tags std_formatter
  and get_print_tags =
    pp_get_print_tags std_formatter
  and set_mark_tags =
    pp_set_mark_tags std_formatter
  and get_mark_tags =
    pp_get_mark_tags std_formatter
  and set_tags =
    pp_set_tags std_formatter
end
