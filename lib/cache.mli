(** The cache is versionned. The individual benchmark entries must start with a
    [version] stanza that contains the version of the benchmark entry format. *)

val save : string -> Bench.t -> unit
(** [save path bench] saves the benchmark results [bench] at [path]. *)

val load : string -> Bench.t
(** [load path] reads the benchmark history at [path] and returns benchmark
    results from it. *)
