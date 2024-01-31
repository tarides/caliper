let template_index_html = Mustache.of_string Asset.index_html
let template_project_html = Mustache.of_string Asset.project_html
let template_collection_html = Mustache.of_string Asset.collection_html

module Index_mustache = struct
  type t = { projects : string list }

  let of_bench (bench_t : Bench.t) =
    { projects = List.map (fun p -> p.Bench.name) bench_t }

  let to_json { projects } : Mustache.Json.t =
    `O [ ("projects", `A (List.map (fun name -> `String name) projects)) ]
end

module Project_mustache = struct
  type t = {
    projects : string list;
    project_name : string;
    collections : string list;
  }

  let of_bench (bench_t : Bench.t) (project : Bench.project) =
    {
      projects = List.map (fun p -> p.Bench.name) bench_t;
      project_name = project.Bench.name;
      collections =
        List.map
          (fun (c : Bench.collection) -> c.Bench.name)
          project.collections;
    }

  let to_json { projects; project_name; collections } : Mustache.Json.t =
    `O
      [
        ("projects", `A (List.map (fun name -> `String name) projects));
        ("project_name", `String project_name);
        ("collections", `A (List.map (fun name -> `String name) collections));
      ]
end

module Collection_mustache = struct
  type t = {
    projects : string list;
    project_name : string;
    collection_name : string;
    benchmarks : string list;
  }

  let of_bench (bench_t : Bench.t) (project : Bench.project)
      (collection : Bench.collection) =
    {
      projects = List.map (fun p -> p.Bench.name) bench_t;
      project_name = project.Bench.name;
      collection_name = collection.Bench.name;
      benchmarks =
        List.map (fun (t : Bench.test) -> t.Bench.name) collection.tests;
    }

  let to_json { projects; project_name; collection_name; benchmarks } :
      Mustache.Json.t =
    `O
      [
        ("projects", `A (List.map (fun name -> `String name) projects));
        ("project_name", `String project_name);
        ("collection_name", `String collection_name);
        ("benchmarks", `A (List.map (fun name -> `String name) benchmarks));
      ]
end

let render_index_html t =
  Mustache.render template_index_html Index_mustache.(to_json (of_bench t))

let render_project_html t project =
  Mustache.render template_project_html
    Project_mustache.(to_json (of_bench t project))

let render_collection_html t project collection =
  Mustache.render template_collection_html
    Collection_mustache.(to_json (of_bench t project collection))

let generate root t =
  if not (Sys.file_exists root) then Unix.mkdir root 0o755;
  Out_channel.with_open_text (Filename.concat root "chart.min.js") (fun oc ->
      Out_channel.output_string oc Asset.chart_js);
  Out_channel.with_open_text (Filename.concat root "alpine.min.js") (fun oc ->
      Out_channel.output_string oc Asset.alpine_js);
  Out_channel.with_open_text (Filename.concat root "logo-with-name.svg")
    (fun oc -> Out_channel.output_string oc Asset.logo_with_name_svg);
  Out_channel.with_open_text (Filename.concat root "main.css") (fun oc ->
      Out_channel.output_string oc Asset.main_css);
  let index_html = render_index_html t in
  Out_channel.with_open_text (Filename.concat root "index.html") (fun oc ->
      Out_channel.output_string oc index_html);

  (* Generate projects *)
  let project_dir_root = Filename.concat root "_" in
  if not (Sys.file_exists project_dir_root) then Unix.mkdir project_dir_root 0o755;
  List.iter
    (fun (project : Bench.project) ->
      let filepath =
        Filename.concat project_dir_root (project.name ^ ".html")
      in
      Out_channel.with_open_text filepath (fun oc ->
          Out_channel.output_string oc (render_project_html t project));
      (* Generate collections *)
      List.iter
        (fun (collection : Bench.collection) ->
          let filepath =
            Filename.concat project_dir_root
              (project.name ^ "--" ^ collection.name ^ ".html")
          in
          Out_channel.with_open_text filepath (fun oc ->
              Out_channel.output_string oc
                (render_collection_html t project collection)))
        project.collections)
    t
