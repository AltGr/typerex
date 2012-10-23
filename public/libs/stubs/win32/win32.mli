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

type os_type = WINDOWS | CYGWIN | UNIX

val os_type : os_type


external waitpids : int -> int array -> int * Unix.process_status
  = "win32_waitpids_ml"

val waitpid : int -> int

val command : string array -> int
val simulate_exec : string array -> 'a

external waitpid : Unix.wait_flag list -> int -> int * Unix.process_status
                 = "win32_waitpid"
