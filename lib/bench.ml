type value = Empty | Int of int | Float of float | Bytes of bytes
type result = { commit : string; timestamp : float; value : value }
type test = { name : string; results : result list }
type group = { name : string; tests : test list }
type collection = { name : string; groups : group list }
type project = { name : string; collections : collection list }
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

let extract_all_commits project =
  let commit_set = Hashtbl.create 100 in
  List.iter
    (fun collection ->
      List.iter
        (fun group ->
          List.iter
            (fun test ->
              List.iter
                (fun result ->
                  Hashtbl.replace commit_set
                    (result.timestamp, result.commit)
                    ())
                test.results)
            group.tests)
        collection.groups)
    project.collections;
  Hashtbl.fold (fun ts_commit _ acc -> ts_commit :: acc) commit_set []
  |> List.sort (fun (a_ts, _) (b_ts, _) -> compare a_ts b_ts)

let align_commits project =
  let all_commits = extract_all_commits project in
  let align_test_results ~all_commits test =
    let aligned_results =
      List.map
        (fun (timestamp, commit) ->
          match
            List.find_opt (fun result -> result.commit = commit) test.results
          with
          | Some result -> result
          | None -> { value = Empty; timestamp; commit })
        all_commits
    in
    { test with results = aligned_results }
  in
  let collections =
    List.map
      (fun collection ->
        let groups =
          List.map
            (fun group ->
              let tests =
                List.map (align_test_results ~all_commits) group.tests
              in
              { group with tests })
            collection.groups
        in
        { collection with groups })
      project.collections
  in
  { project with collections }

module Json = struct
  let of_value = function
    | Empty -> `Null
    | Int value -> `Float (float_of_int value)
    | Float value -> `Float value
    | Bytes _ -> `Float 0. (* FIXME: Bytes to Float?! *)

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
