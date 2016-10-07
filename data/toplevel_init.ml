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
