open Sexplib
open Ppx_sexp_conv_lib.Conv

type t = { name : string; url : string } [@@deriving sexp]

let empty = { name = ""; url = "" }

let load root =
  let path = Filename.concat root ".caliper" in
  if Sys.file_exists path then
    let content =
      In_channel.with_open_text path (fun ic -> In_channel.input_all ic)
    in
    let sexp = Sexp.of_string content in
    Some (t_of_sexp sexp)
  else None

let save root config =
  if not (Sys.file_exists root) then Unix.mkdir root 0o755;
  let path = Filename.concat root ".caliper" in
  let sexp = sexp_of_t config in
  Sexp.save_hum path sexp
