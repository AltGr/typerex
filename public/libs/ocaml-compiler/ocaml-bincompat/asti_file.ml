(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open Bincompat

type t

let ast_magic_number_len = String.length Config.ast_intf_magic_number
let ast_magic_prefix = "Caml1999M"


(*
let versions =
  [
    V3_11_2, "Caml1999N011", v3112_input, v3112_output; (* 3.11.2 *)
    V3_12_0, "Caml1999N012", v3120_input, v3120_output; (* 3.12.0 - 3.12.1 *)
    V3_13_0, "Caml1999N013", v3130_input, v3130_output; (* 3.13.0 -  *)
  ]
*)

(*
val read : (string -> t)
*)
let read filename =
(*
  let ic = open_in_bin filename in
  let magic = String.create ast_magic_number_len in
  try
    really_input ic magic 0 ast_magic_number_len;
    let input_fun = try
                      StringMap.find magic input_map
      with Not_found ->
        if String.sub magic 0 (String.length ast_magic_prefix)
          = ast_magic_prefix then
          raise (Bincompat.Error(No_Such_Magic ("camlp4", magic)))
        else raise Not_found
    in
    let ast = input_fun ic in
    close_in ic;
    Printf.fprintf stderr "Ast_file.read done\n%!";
    Some ast
  with e ->
    close_in ic;
    Printf.fprintf stderr "Ast_file.read exn\n%!";
    None
*)
  failwith "Asti_file.read not implemented"

(*
val write : (string -> (t -> unit))
*)
let write string t = assert false


