open Sexplib
open Bench

module V0_1 = struct
  open Ppx_sexp_conv_lib.Conv

  type v0_1_value_type = Empty | Int | Float | List | Assoc [@@deriving sexp]

  type v0_1_entry = {
    group_name : string;
    test_name : string;
    value : string;
    value_type : v0_1_value_type;
  }
  [@@deriving sexp]

  type v0_1_result = { commit : string; timestamp : float; entry : v0_1_entry }
  [@@deriving sexp]

  type v0_1_t = {
    version : string;
    commit : string;
    timestamp : float;
    entries : v0_1_entry list;
  }
  [@@deriving sexp]

  let results_of_t t =
    List.map
      (fun entry -> { timestamp = t.timestamp; commit = t.commit; entry })
      t.entries

  let v0_1_entry_of_value ~group_name ~test_name (value : value) : v0_1_entry =
    let value, value_type =
      match value with
      | Empty -> ("", Empty)
      | Int v -> (Int.to_string v, Int)
      | Float v -> (Float.to_string v, Float)
      | List vs -> (List.map Float.to_string vs |> String.concat " ", List)
      | Assoc vs ->
          ( List.map (fun (x, y) -> Printf.sprintf "%s:%f" x y) vs
            |> String.concat " ",
            Assoc )
    in
    { test_name; group_name; value; value_type }

  let value_of_v0_1_entry v0_1_entry : value =
    match v0_1_entry.value_type with
    | Empty -> Empty
    | Int -> Int (int_of_string v0_1_entry.value)
    | Float -> Float (Float.of_string v0_1_entry.value)
    | List ->
        List
          (String.split_on_char ' ' v0_1_entry.value |> List.map Float.of_string)
    | Assoc ->
        Assoc
          (String.split_on_char ' ' v0_1_entry.value
          |> List.filter_map (fun kv ->
                 match String.split_on_char ':' kv with
                 | [ k; v ] -> Some (k, Float.of_string v)
                 | _ -> None))

  let of_sexp = v0_1_t_of_sexp
  let to_sexp = sexp_of_v0_1_t

  module To_bench = struct
    let convert_v0_1_results_to_results results =
      List.map
        (fun (r : v0_1_result) ->
          {
            timestamp = r.timestamp;
            commit = r.commit;
            value = value_of_v0_1_entry r.entry;
          })
        results

    let convert_group (group_name, test_entries_map) =
      let tests =
        Hashtbl.fold
          (fun test_name v0_1_results acc ->
            let results = convert_v0_1_results_to_results v0_1_results in
            { name = test_name; results } :: acc)
          test_entries_map []
      in
      { name = group_name; tests }

    let convert_v0_1_t_list_to_groups (v0_1_t_list : v0_1_t list) : group list =
      let aggregated_results = Hashtbl.create 100 in
      let v0_1_results = v0_1_t_list |> List.map results_of_t |> List.flatten in
      List.iter
        (fun (result : v0_1_result) ->
          let key = (result.entry.group_name, result.entry.test_name) in
          let results = Hashtbl.find_opt aggregated_results key in
          match results with
          | Some results ->
              Hashtbl.replace aggregated_results key (result :: results)
          | None -> Hashtbl.add aggregated_results key [ result ])
        v0_1_results;

      let groups_map = Hashtbl.create 10 in
      Hashtbl.iter
        (fun (group_name, test_name) results ->
          let group_data = Hashtbl.find_opt groups_map group_name in
          let test_results_map =
            match group_data with
            | Some test_results_map -> test_results_map
            | None -> Hashtbl.create 10
          in
          Hashtbl.replace test_results_map test_name results;
          Hashtbl.replace groups_map group_name test_results_map)
        aggregated_results;

      Hashtbl.fold
        (fun group_name test_results_map acc ->
          convert_group (group_name, test_results_map) :: acc)
        groups_map []

    let convert_v0_1_t_list_to_collection ~collection_name v0_1_t_list :
        collection =
      let groups = convert_v0_1_t_list_to_groups v0_1_t_list in
      { name = collection_name; groups }
  end

  module Of_bench = struct
    let convert_result_to_v0_1_result ~group_name ~test_name (result : result) :
        v0_1_result =
      let entry = v0_1_entry_of_value ~group_name ~test_name result.value in
      { timestamp = result.timestamp; commit = result.commit; entry }

    let convert_test_to_v0_1_results ~group_name (test : test) :
        v0_1_result list =
      List.map
        (convert_result_to_v0_1_result ~group_name ~test_name:test.name)
        test.results

    let convert_group_to_v0_1_t (group : group) : v0_1_t list =
      let results =
        List.concat_map
          (convert_test_to_v0_1_results ~group_name:group.name)
          group.tests
      in
      let grouped_by_timestamp =
        group_by (fun (t : v0_1_result) -> (t.timestamp, t.commit)) results
      in
      List.map
        (fun ((timestamp, commit), grouped_results) ->
          {
            entries = List.map (fun r -> r.entry) grouped_results;
            timestamp;
            commit;
            version = "0.1";
          })
        grouped_by_timestamp

    let convert_groups_to_v0_1_t_list (groups : group list) : v0_1_t list =
      let ts = List.concat_map convert_group_to_v0_1_t groups in
      let grouped_by_timestamp =
        group_by (fun t -> (t.timestamp, t.commit)) ts
      in
      List.map
        (fun ((timestamp, commit), ts) ->
          {
            entries = List.concat_map (fun r -> r.entries) ts;
            timestamp;
            commit;
            version = "0.1";
          })
        grouped_by_timestamp
  end
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
  let save_tests collection_root collection =
    let ts = V0_1.Of_bench.convert_groups_to_v0_1_t_list collection.groups in
    List.iter
      (fun (t : V0_1.v0_1_t) ->
        let run_fp =
          Filename.concat collection_root (string_of_float t.timestamp)
        in
        V0_1.to_sexp t |> Sexp.save_hum run_fp)
      ts
  in
  let save_collections project_root collections =
    List.iter
      (fun (collection : collection) ->
        let collection_root = Filename.concat project_root collection.name in
        if not (Sys.file_exists collection_root) then
          Unix.mkdir collection_root 0o755;
        save_tests collection_root collection)
      collections
  in
  List.iter
    (fun project ->
      let project_root = Filename.concat cache_root project.name in
      if not (Sys.file_exists project_root) then Unix.mkdir project_root 0o755;
      save_collections project_root project.collections)
    projects

let load cache_root =
  let load_benchmarks_in_collection ~collection_name collection_root =
    let ts =
      Sys.readdir collection_root
      |> Array.to_list
      |> List.map (fun fn ->
             let fp = Filename.concat collection_root fn in
             load_benchmark fp)
    in
    V0_1.To_bench.convert_v0_1_t_list_to_collection ~collection_name ts
  in
  Sys.readdir cache_root |> Array.to_list
  |> List.map (fun project_dir_name ->
         let project_dir_path = Filename.concat cache_root project_dir_name in
         let config =
           match Config.load project_dir_path with
           | Some config -> config
           | None -> { Config.empty with name = project_dir_name }
         in
         {
           name = config.name;
           url = config.url;
           collections =
             Sys.readdir project_dir_path
             |> Array.to_list
             |> List.filter (fun name ->
                    Filename.concat project_dir_path name |> Sys.is_directory)
             |> List.map (fun dir_name ->
                    let collection_dir_path =
                      Filename.concat project_dir_path dir_name
                    in
                    load_benchmarks_in_collection ~collection_name:dir_name
                      collection_dir_path);
         })
