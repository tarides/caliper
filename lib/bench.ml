type value = Int of int | Float of float | Bytes of bytes
type entry = { test_name : string; value : value }
type result = { version : string; timestamp : float; entries : entry list }
type collection = { name : string; results : result list }
type t = collection list

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

  let diff_latest_in_collection collection =
    match
      List.sort
        (fun r1 r2 -> compare r2.timestamp r1.timestamp)
        collection.results
    with
    | newest :: second_newest :: _ -> Some (diff_results second_newest newest)
    | _ -> None

  let diff_latest (t : t) =
    List.map
      (fun (collection : collection) ->
        {
          name = collection.name;
          result_diff = diff_latest_in_collection collection;
        })
      t
end
