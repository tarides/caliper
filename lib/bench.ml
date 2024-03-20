type value =
  | Empty
  | Int of int
  | Float of float
  | List of float list
  (* FIXME: Current bench supports {avg; max; min} But, it's better to remove
     support for this, and use list of floats?? But, converting current-bench
     data wouldn't be possible.*)
  | Assoc of (string * float) list

type result = { commit : string; timestamp : float; value : value }
type test = { name : string; results : result list }
type group = { name : string; tests : test list }
type collection = { name : string; groups : group list }
type project = { name : string; collections : collection list; url : string }
type t = project list

let group_by f lst =
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun item ->
      let key = f item in
      match Hashtbl.find_opt tbl key with
      | Some group -> Hashtbl.replace tbl key (item :: group)
      | None -> Hashtbl.add tbl key [ item ])
    lst;
  Hashtbl.fold (fun key group acc -> (key, List.rev group) :: acc) tbl []

module Process = struct
  let extract_collection_commits collection =
    let commit_set = Hashtbl.create 100 in
    List.iter
      (fun group ->
        List.iter
          (fun test ->
            List.iter
              (fun result ->
                Hashtbl.replace commit_set (result.timestamp, result.commit) ())
              test.results)
          group.tests)
      collection.groups;
    Hashtbl.fold (fun ts_commit _ acc -> ts_commit :: acc) commit_set []
    |> List.sort (fun (a_ts, _) (b_ts, _) -> compare a_ts b_ts)

  let find_latest_commits collection =
    let commits = extract_collection_commits collection in
    commits |> List.rev |> List.hd

  let filter_active_tests collection =
    let _, commit = find_latest_commits collection in
    let groups =
      List.filter_map
        (fun group ->
          let tests =
            List.filter_map
              (fun test ->
                match
                  List.find_opt
                    (fun result -> result.commit = commit)
                    test.results
                with
                | Some _ -> Some test
                | _ -> None)
              group.tests
          in
          if List.length tests > 0 then Some { group with tests } else None)
        collection.groups
    in
    { collection with groups }

  let align_test_results ~commits test =
    let aligned_results =
      List.map
        (fun (timestamp, commit) ->
          match
            List.find_opt (fun result -> result.commit = commit) test.results
          with
          | Some result -> result
          | None -> { value = Empty; timestamp; commit })
        commits
    in
    { test with results = aligned_results }

  let align_commits ?(active_only = true) project =
    let collections =
      List.map
        (fun collection ->
          let filtered_collection =
            if active_only then filter_active_tests collection else collection
          in
          let commits = extract_collection_commits filtered_collection in
          let groups =
            List.map
              (fun group ->
                let tests =
                  List.map (align_test_results ~commits) group.tests
                in
                { group with tests })
              filtered_collection.groups
          in
          { filtered_collection with groups })
        project.collections
    in
    { project with collections }
end

module Stats = struct
  let mean = function
    | [] -> 0.
    | (xs : float list) ->
        let n = List.length xs |> float_of_int in
        let sum = List.fold_left (fun acc x -> acc +. x) 0. xs in
        sum /. n
end

module Json = struct
  let of_value = function
    | Empty -> `Null
    | Int value -> `Float (float_of_int value)
    | Float value -> `Float value
    | List values -> `Float (Stats.mean values)
    | Assoc values -> (
        match List.find_opt (fun (x, _) -> x = "avg") values with
        | Some (_, v) -> `Float v
        | None -> `Null)

  let of_result result =
    `Assoc
      [
        ("timestamp", `Float result.timestamp);
        ("commit", `String result.commit);
        ("value", of_value result.value);
      ]

  let of_test (test : test) =
    `Assoc
      [
        ("name", `String test.name);
        ( "results",
          `List
            (test.results
            |> List.sort (fun a b -> compare a.timestamp b.timestamp)
            |> List.map of_result) );
      ]

  let of_group (group : group) =
    `Assoc
      [
        ("name", `String group.name);
        ("tests", `List (List.map of_test group.tests));
      ]

  let of_collection (collection : collection) =
    `Assoc
      [
        ("name", `String collection.name);
        ("groups", `List (List.map of_group collection.groups));
      ]

  let of_project project =
    `Assoc
      [
        ("name", `String project.name);
        ("groups", `List (List.map of_collection project.collections));
      ]
end
