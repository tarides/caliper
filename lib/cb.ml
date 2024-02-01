open Yojson.Safe.Util
open Bench

exception Invalid_JSON
exception Not_Number

module Parse = struct
  type metric = {
    value : Bench.value;
    timestamp : float;
    test_name : string;
    group_name : string;
    collection_name : string;
  }

  let mean = function
    | [] -> 0.
    | (xs : float list) ->
        let n = List.length xs |> float_of_int in
        let sum = List.fold_left (fun acc x -> acc +. x) 0. xs in
        sum /. n

  let extract_number = function
    | `Float v -> v
    | `Int v -> v |> float_of_int
    | _ -> raise Not_Number

  let to_value = function
    (* FIXME: Maybe better to support a list type in Bench.value?? *)
    | `List v ->
        let value = v |> List.map extract_number |> mean in
        Bench.Float value
    | `Float v -> Bench.Float v
    | `Int v -> Bench.Int v
    | _ -> raise Invalid_JSON

  let load_cb_metric metric =
    let test_name = metric |> member "name" |> to_string in
    let value = metric |> member "value" |> to_value in
    (test_name, value)

  let merge_metrics_as_collections metrics =
    let merge_metrics_as_test (name, metrics) =
      let results =
        List.map
          (fun m -> { Bench.value = m.value; timestamp = m.timestamp })
          metrics
      in
      { Bench.name; results }
    in
    let merge_metrics_as_group (name, metrics) =
      let test_metrics = group_by (fun m -> m.test_name) metrics in
      let tests = List.map merge_metrics_as_test test_metrics in
      { Bench.name; tests }
    in
    let merge_metrics_as_collection (name, metrics) =
      let group_metrics = group_by (fun m -> m.group_name) metrics in
      let groups = List.map merge_metrics_as_group group_metrics in
      { Bench.name; groups }
    in
    let collection_metrics = group_by (fun m -> m.collection_name) metrics in
    List.map merge_metrics_as_collection collection_metrics

  let load_cb_result test =
    let collection_name = test |> member "benchmark_name" |> to_string in
    let group_name = test |> member "test_name" |> to_string in
    let ts = test |> member "run_at" |> to_string in
    let timestamp = ts |> ISO8601.Permissive.datetime in
    let cb_metrics = test |> member "metrics" |> to_list in
    List.map
      (fun m ->
        let test_name, value = load_cb_metric m in
        { value; timestamp; test_name; group_name; collection_name })
      cb_metrics

  let load_cb_benchmarks groups =
    let metrics = List.concat_map load_cb_result groups in
    merge_metrics_as_collections metrics

  let read path =
    let open Yojson.Safe in
    let json = from_file path in
    json |> to_list |> load_cb_benchmarks
end
