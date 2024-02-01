type value = Int of int | Float of float | Bytes of bytes
type result = { timestamp : float; value : value }
type test = { name : string; results : result list }
type group = { name : string; tests : test list }
type collection = { name : string; groups : group list }
type project = { name : string; collections : collection list }
type t = project list
