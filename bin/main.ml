open Cmdliner
open Caliper

let run_generate_html cache_dir output_dir =
  let t = Cache.load cache_dir in
  Html.generate output_dir t

let run_parse_cb_json cb_json cache_dir =
  let collections = Parse.read cb_json in
  let name = cb_json |> Filename.basename |> Filename.remove_extension in
  let project = { name; collections } in
  Cache.save cache_dir [ project ]

(* Cmdliner setup *)

let cache_dir_arg =
  let env =
    Cmd.Env.info "XDG_BENCHMARK_CACHE"
      ~doc:"Path to the benchmark cache directory from XDG environment variable"
  in
  let doc = "The directory containing the benchmark cache." in
  Arg.(
    value
    & opt dir
        (Sys.getenv_opt "XDG_BENCHMARK_CACHE" |> Option.value ~default:"./cache")
    & info [ "d"; "cache-dir" ] ~env ~docv:"CACHE_DIR" ~doc)

let output_dir_arg =
  let doc = "The directory where the HTML assets will be generated." in
  Arg.(
    value & opt string "./html-output"
    & info [ "o"; "output-dir" ] ~docv:"OUTPUT_DIR" ~doc)

let generate_html_cmd =
  let doc = "Generate an HTML report from the benchmark cache" in
  let term = Term.(const run_generate_html $ cache_dir_arg $ output_dir_arg) in
  let info =
    Cmd.info "generate-html" ~doc ~sdocs:"COMMON OPTIONS"
      ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let cb_json_dump_arg =
  let doc = "The path to the current bench JSON dump file." in
  Arg.(
    value & opt file "sample.json"
    & info [ "j"; "json-dump" ] ~docv:"JSON_DUMP" ~doc)

let parse_cb_json_cmd =
  let doc = "Parse Current bench JSON dump to a benchmark cache directory" in
  let term =
    Term.(const run_parse_cb_json $ cb_json_dump_arg $ cache_dir_arg)
  in
  let info =
    Cmd.info "parse-cb-json" ~doc ~sdocs:"COMMON OPTIONS"
      ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let default_cmd =
  let doc = "Caliper benchmarking toolchain" in
  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info =
    Cmd.info "caliper" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.group ~default:term info [ generate_html_cmd; parse_cb_json_cmd ]

let () = exit (Cmd.eval default_cmd)
