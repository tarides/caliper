open Sexplib
open Bench

module V0_1 = struct
  open Ppx_sexp_conv_lib.Conv

  type v0_1_value_type = Int | Float | Bytes [@@deriving sexp]

  type v0_1_entry = {
    test_name : string;
    value : string;
    value_type : v0_1_value_type;
  }
  [@@deriving sexp]

  type v0_1_t = {
    version : string;
    timestamp : float;
    entries : v0_1_entry list;
  }
  [@@deriving sexp]

  let v0_1_entry_of_entry (entry : entry) : v0_1_entry =
    let value, value_type =
      match entry.value with
      | Int v -> (Int.to_string v, Int)
      | Float v -> (Float.to_string v, Float)
      | Bytes v -> (Bytes.to_string v, Bytes)
    in
    { test_name = entry.test_name; value; value_type }

  let entry_of_v0_1_entry v0_1_entry : entry =
    let value : value =
      match v0_1_entry.value_type with
      | Int -> Int (int_of_string v0_1_entry.value)
      | Float -> Float (Float.of_string v0_1_entry.value)
      | Bytes -> Bytes (Bytes.of_string v0_1_entry.value)
    in
    { test_name = v0_1_entry.test_name; value }

  let of_sexp sexp : result =
    let v0_1 = v0_1_t_of_sexp sexp in
    {
      version = "0.1";
      timestamp = v0_1.timestamp;
      entries = List.map entry_of_v0_1_entry v0_1.entries;
    }

  let to_sexp (t : result) =
    let v0_1_t =
      {
        version = "0.1";
        timestamp = t.timestamp;
        entries = List.map v0_1_entry_of_entry t.entries;
      }
    in
    sexp_of_v0_1_t v0_1_t
end

type version = V0_1

let version_of_string = function
  | "0.1" -> V0_1
  | v -> failwith ("Unsupported version " ^ v)

let load_benchmark path =
  let content =
    In_channel.with_open_text path (fun ic -> In_channel.input_all ic)
  in
  let sexp = Sexp.of_string content in
  let version =
    match sexp with
    | Sexp.List (Sexp.List [ Sexp.Atom "version"; Sexp.Atom version ] :: _) ->
        version_of_string version
    | _ -> failwith "Invalid benchmark entry file format"
  in
  match version with V0_1 -> V0_1.of_sexp sexp

let save cache_root (projects : t) =
  if not (Sys.file_exists cache_root) then Unix.mkdir cache_root 0o755;
  let save_results root results =
    List.iter
      (fun result ->
        let result_fp =
          Filename.concat root (string_of_float result.timestamp)
        in
        V0_1.to_sexp result |> Sexp.save_hum result_fp)
      results
  in
  let save_tests collection_root tests =
    List.iter
      (fun (test : test) ->
        let test_root = Filename.concat collection_root test.name in
        if not (Sys.file_exists test_root) then Unix.mkdir test_root 0o755;
        save_results test_root test.results)
      tests
  in
  let save_collections project_root collections =
    List.iter
      (fun (collection : collection) ->
        let collection_root = Filename.concat project_root collection.name in
        if not (Sys.file_exists collection_root) then
          Unix.mkdir collection_root 0o755;
        save_tests collection_root collection.tests)
      collections
  in
  List.iter
    (fun project ->
      let project_root = Filename.concat cache_root project.name in
      if not (Sys.file_exists project_root) then Unix.mkdir project_root 0o755;
      save_collections project_root project.collections)
    projects

let load cache_root =
  let group_by_test_name results =
    let table = Hashtbl.create 10 in
    List.iter
      (fun result ->
        List.iter
          (fun entry ->
            match Hashtbl.find_opt table entry.test_name with
            | Some r -> Hashtbl.replace table entry.test_name (result :: r)
            | None -> Hashtbl.add table entry.test_name [ result ])
          result.entries)
      results;
    Hashtbl.fold (fun name results acc -> { name; results } :: acc) table []
  in
  let load_benchmarks_in_collection collection_root =
    let results =
      Sys.readdir collection_root
      |> Array.to_list
      |> List.map (fun fn ->
             let fp = Filename.concat collection_root fn in
             load_benchmark fp)
    in
    group_by_test_name results
  in
  Sys.readdir cache_root |> Array.to_list
  |> List.map (fun project_dir_name ->
         let project_dir_path = Filename.concat cache_root project_dir_name in
         {
           name = project_dir_name;
           collections =
             Sys.readdir project_dir_path
             |> Array.to_list
             |> List.map (fun dir_name ->
                    let collection_dir_path =
                      Filename.concat project_dir_path dir_name
                    in
                    {
                      name = dir_name;
                      tests = load_benchmarks_in_collection collection_dir_path;
                    });
         })
