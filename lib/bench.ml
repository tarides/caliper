type value = Int of int | Float of float | Bytes of bytes
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

module Json = struct
  let of_value = function
    | Int value -> float_of_int value
    | Float value -> value
    | Bytes _ -> 0. (* FIXME: Bytes to Float?! *)

  let of_result result =
    `Assoc
      [
        ("timestamp", `Float result.timestamp);
        ("value", `Float (of_value result.value));
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
