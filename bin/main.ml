open Cmdliner
open Caliper

let print_diff diff =
  match diff.Diff.result_diff with
  | None ->
      Printf.printf "Collection: %s has no latest results to diff\n" diff.name
  | Some rdiff ->
      Printf.printf "Collection: %s\n" diff.name;
      Printf.printf "From timestamp: %f\n" rdiff.from_timestamp;
      Printf.printf "To timestamp: %f\n" rdiff.to_timestamp;
      List.iter
        (fun (ediff : Diff.entry_diff) ->
          Printf.printf "Test: %s -> " ediff.test_name;
          match ediff.Diff.value_diff with
          | Int_diff i -> Printf.printf "Change: %d\n" i
          | Float_diff f -> Printf.printf "Change: %f\n" f
          | Bytes_diff b ->
              Printf.printf "Change in bytes: %s\n" (Bytes.to_string b)
          | No_change -> Printf.printf "No change\n")
        rdiff.Diff.entry_diffs;
      print_newline ()

let run_print_diff cache_dir collection_name =
  let t = Cache.load cache_dir in
  let filtered_t =
    match collection_name with
    | None -> t
    | Some name -> List.filter (fun collection -> collection.name = name) t
  in
  let diffs = Diff.diff_latest filtered_t in
  List.iter print_diff diffs

let run_generate_html cache_dir output_dir =
  let t = Cache.load cache_dir in
  Html.generate output_dir t

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

let collection_name_arg =
  let doc = "Name of the benchmark collection to filter by." in
  Arg.(
    value
    & opt (some string) None
    & info [ "c"; "collection" ] ~docv:"COLLECTION_NAME" ~doc)

let diff_cmd =
  let doc = "Display the diff in the benchmark history." in
  let term =
    Term.(const run_print_diff $ cache_dir_arg $ collection_name_arg)
  in
  let info =
    Cmd.info "print-diff" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let output_dir_arg =
  let doc = "The directory where the HTML assets will be generated." in
  Arg.(
    value & opt dir "./output"
    & info [ "o"; "output-dir" ] ~docv:"OUTPUT_DIR" ~doc)

let generate_html_cmd =
  let doc = "Generate an HTML report from the benchmark cache" in
  let term = Term.(const run_generate_html $ cache_dir_arg $ output_dir_arg) in
  let info =
    Cmd.info "generate-html" ~doc ~sdocs:"COMMON OPTIONS"
      ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let default_cmd =
  let doc = "Caliper benchmarking toolchain" in
  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info =
    Cmd.info "caliper" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.group ~default:term info [ diff_cmd; generate_html_cmd ]

let () = exit (Cmd.eval default_cmd)
