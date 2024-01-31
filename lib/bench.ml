type value = Int of int | Float of float | Bytes of bytes
type entry = { test_name : string; value : value }
type result = { version : string; timestamp : float; entries : entry list }
type test = { name : string; results : result list }
type collection = { name : string; tests : test list }
type project = { name : string; collections : collection list }
type t = project list

module Diff = struct
  type value_diff =
    | Int_diff of int
    | Float_diff of float
    | Bytes_diff of bytes
    | No_change

  type entry_diff = { test_name : string; value_diff : value_diff }

  type result_diff = {
    from_timestamp : float;
    to_timestamp : float;
    entry_diffs : entry_diff list;
  }

  type collection_diff = { name : string; result_diff : result_diff option }

  let compute_value_diff old_value new_value =
    match (old_value, new_value) with
    | Int old_int, Int new_int -> Int_diff (new_int - old_int)
    | Float old_float, Float new_float -> Float_diff (new_float -. old_float)
    | Bytes old_bytes, Bytes new_bytes ->
        Bytes_diff (Bytes.sub new_bytes 0 (Bytes.length old_bytes))
    | _ -> No_change

  let diff_results older newer =
    let diff_entry (e1 : entry) (e2 : entry) =
      {
        test_name = e1.test_name;
        value_diff = compute_value_diff e1.value e2.value;
      }
    in
    {
      from_timestamp = older.timestamp;
      to_timestamp = newer.timestamp;
      entry_diffs = List.map2 diff_entry older.entries newer.entries;
    }

  let diff_latest_in_test test =
    match
      List.sort (fun r1 r2 -> compare r2.timestamp r1.timestamp) test.results
    with
    | newest :: second_newest :: _ -> Some (diff_results second_newest newest)
    | _ -> None

  let diff_latest (t : collection) =
    List.map
      (fun (test : test) ->
        { name = test.name; result_diff = diff_latest_in_test test })
      t.tests
end

module Json_conv = struct
  let entry_to_json (entry : entry) =
    `O
      [
        ("test_name", `String entry.test_name);
        (match entry.value with
        | Int v -> ("value", `Float (float_of_int v))
        | Float v -> ("Float", `Float v)
        | Bytes v -> ("Bytes", `String (Bytes.to_string v)));
      ]

  let result_to_json (result : result) =
    `O
      [
        ("version", `String result.version);
        ("timestamp", `Float result.timestamp);
        ("entries", `A (List.map entry_to_json result.entries));
      ]

  let test_to_json (test : test) =
    `O
      [
        ("name", `String test.name);
        ("results", `A (List.map result_to_json test.results));
      ]

  let collection_to_json (collection : collection) =
    `O
      [
        ("name", `String collection.name);
        ("tests", `A (List.map test_to_json collection.tests));
      ]

  let project_to_json (project : project) =
    `O
      [
        ("name", `String project.name);
        ("collections", `A (List.map collection_to_json project.collections));
      ]

  let to_json projects =
    `O [ ("projects", `A (List.map project_to_json projects)) ]
end
