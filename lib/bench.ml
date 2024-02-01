type value = Int of int | Float of float | Bytes of bytes
type result = { timestamp : float; value : value }
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
