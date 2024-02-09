open Sexplib
open Bench

module V0_1 = struct
  open Ppx_sexp_conv_lib.Conv

  type v0_1_value_type = Int | Float | Bytes [@@deriving sexp]

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
      | Int v -> (Int.to_string v, Int)
      | Float v -> (Float.to_string v, Float)
      | Bytes v -> (Bytes.to_string v, Bytes)
    in
    { test_name; group_name; value; value_type }

  let value_of_v0_1_entry v0_1_entry : value =
    match v0_1_entry.value_type with
    | Int -> Int (int_of_string v0_1_entry.value)
    | Float -> Float (Float.of_string v0_1_entry.value)
    | Bytes -> Bytes (Bytes.of_string v0_1_entry.value)

  let of_sexp = v0_1_t_of_sexp
  let to_sexp = sexp_of_v0_1_t

  module To_bench = struct
    let convert_entries_to_results entries timestamp commit =
      List.map
        (fun entry -> { timestamp; commit; value = value_of_v0_1_entry entry })
        entries

    let convert_group (group_name, test_entries_map, timestamp, commit) =
      let tests =
        Hashtbl.fold
          (fun test_name entries acc ->
            let results = convert_entries_to_results entries timestamp commit in
            { name = test_name; results } :: acc)
          test_entries_map []
      in
      { name = group_name; tests }

    let convert_v0_1_t_to_groups (v0_1_t : v0_1_t) : group list =
      let aggregated_entries = Hashtbl.create 100 in
      List.iter
        (fun entry ->
          let key = (entry.group_name, entry.test_name) in
          let entries = Hashtbl.find_opt aggregated_entries key in
          match entries with
          | Some entries -> Hashtbl.add aggregated_entries key (entry :: entries)
          | None -> Hashtbl.add aggregated_entries key [ entry ])
        v0_1_t.entries;

      let groups_map = Hashtbl.create 10 in
      Hashtbl.iter
        (fun (group_name, test_name) entries ->
          let test_entries_map = Hashtbl.create 10 in
          Hashtbl.add test_entries_map test_name entries;
          Hashtbl.add groups_map group_name test_entries_map)
        aggregated_entries;

      Hashtbl.fold
        (fun group_name test_entries_map acc ->
          convert_group
            (group_name, test_entries_map, v0_1_t.timestamp, v0_1_t.commit)
          :: acc)
        groups_map []

    let convert_v0_1_t_list_to_groups (v0_1_list : v0_1_t list) : group list =
      List.flatten (List.map convert_v0_1_t_to_groups v0_1_list)

    let _convert ~project_name ~collection_name v0_1_t_list : t =
      let groups = convert_v0_1_t_list_to_groups v0_1_t_list in
      [
        {
          name = project_name;
          collections = [ { name = collection_name; groups } ];
        };
      ]
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
  let load_benchmarks_in_collection collection_root =
    let ts =
      Sys.readdir collection_root
      |> Array.to_list
      |> List.map (fun fn ->
             let fp = Filename.concat collection_root fn in
             load_benchmark fp)
    in
    let results = List.concat_map V0_1.results_of_t ts in
    let load_result (r : V0_1.v0_1_result) =
      {
        value = V0_1.value_of_v0_1_entry r.entry;
        timestamp = r.timestamp;
        commit = r.commit;
      }
    in
    let results_by_group_name =
      group_by (fun (r : V0_1.v0_1_result) -> r.entry.group_name) results
    in
    List.map
      (fun (name, group_results) ->
        let results_by_test_name =
          group_by
            (fun (r : V0_1.v0_1_result) -> r.entry.test_name)
            group_results
        in
        let tests =
          List.map
            (fun (name, test_results) ->
              { name; results = List.map load_result test_results })
            results_by_test_name
        in
        { tests; name })
      results_by_group_name
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
                      groups = load_benchmarks_in_collection collection_dir_path;
                    });
         })
