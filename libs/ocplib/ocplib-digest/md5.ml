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

(*

open Maker

module Md5 = Make(struct
      let hash_length = 16
      let hash_name = "Md5"

      external unsafe_string : string -> string -> int -> unit = "md5_unsafe_string"  "noalloc"
      external unsafe_file : string -> string -> int64 -> unit = "md5_unsafe_file" "noalloc"
      external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
        "md5_unsafe64_fd" "noalloc"

      module Base = Base16
    end)

include Md5

*)

open MakeDigest

include MakeDigest.Make(struct
  let length = 16
  let name = "Md5"

  external context_size : unit -> int = "md4_context_size_ml" "noalloc"
  external context_init : context -> unit = "md4_context_init_ml" "noalloc"
  external context_append : context -> string -> int -> int -> unit =
    "md4_context_append_ml" "noalloc"
  external context_finish : string -> context -> unit =
    "md4_context_finish_ml" "noalloc"

end)
