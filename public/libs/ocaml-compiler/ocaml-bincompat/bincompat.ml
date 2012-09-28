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

type error =
  | IncompatibleVersionFeature of string

exception Error of error

(*
type error =
    TODO of string
  | No_Such_Feature of string * string
  | No_Such_Version of string
  | No_Such_Magic of string * string
  | Corrupted_interface of string


type version =
  | V3_11_2
  | V3_12_0
  | V3_13_0


let default_version = V3_13_0
let supported_versions =
  [
    "3.11.2" , V3_11_2;
    "3.12.0" , V3_12_0;
    "3.12.1" , V3_12_0;
    "trunk" ,  V3_13_0;
  ]

module VersionMap = Map.Make(struct type t = version let compare = compare end)

let version_of_string =
  let map = ref StringMap.empty in
  List.iter (fun (sv,v) -> map := StringMap.add sv v !map) supported_versions;
  !map

let string_of_version =
  let map = ref VersionMap.empty in
  List.iter (fun (sv,v) -> map := VersionMap.add v sv !map) supported_versions;
  !map

let current_version = ref
  begin
    try
      let expected_version = Sys.getenv "OCAML_TARGET" in
      try
        StringMap.find expected_version version_of_string
      with Not_found ->
        if expected_version <> "" then begin
          Printf.fprintf stderr "ocaml-bincompat: version %s not supported\n%!"
            expected_version;
          exit 2
        end;
        default_version
    with Not_found -> default_version
  end

let report_error ppf = function
  | TODO funct -> fprintf ppf
      "Function %s@ still has to be implemented" funct
  | No_Such_Feature (version, feature) -> fprintf ppf
      "Feature %s is not available in version %s" feature version
  | No_Such_Version version -> fprintf ppf
      "Version %s@ is not supported by the compatibility layer" version
  | No_Such_Magic (kind, magic) -> fprintf ppf
      "Magic %s@ for %s is not supported by the compatibility layer"
    (String.escaped magic) kind
  | Corrupted_interface filename -> fprintf ppf
      "Corrupted compiled interface@ %s" filename

*)

open Format

let report_error ppf = function
  | IncompatibleVersionFeature s -> fprintf ppf
      "Incompatible Version Feature %s" s

module MakeReader(M: sig

  type t
  val magic_number : string
  val versions : (string * (string * (in_channel -> t))) list
  val error_bad_magic : (* filename: *) string -> 'a
  val error_corrupted_file :
    (* filename: *) string -> (* version: *) string -> 'a

end) : sig

  val input : string -> in_channel -> M.t
  val read : string -> M.t

end = struct


  let magic_length = String.length Config.cmi_magic_number

  let input filename ic magic_number =
    let version, inputer =
      try
        List.assoc magic_number M.versions
      with Not_found ->
        M.error_bad_magic filename
  in
  try
    inputer ic
  with e ->
    M.error_corrupted_file filename version

  let input filename ic =
    let buffer = String.create magic_length in
    really_input ic buffer 0 magic_length;
    let v = input filename ic buffer in
    v

let read filename =
  let ic = open_in_bin filename in
  try
    let v = input filename ic in
    close_in ic;
    v
  with e ->
    close_in ic;
    raise e

end