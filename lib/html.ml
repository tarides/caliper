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
  type group = { name : string; tests : string list }

  type t = {
    projects : string list;
    project_name : string;
    project_url : string;
    collection_name : string;
    groups : group list;
  }

  let of_bench (bench_t : Bench.t) (project : Bench.project)
      (collection : Bench.collection) =
    {
      projects = List.map (fun p -> p.Bench.name) bench_t;
      project_name = project.Bench.name;
      project_url = project.Bench.url;
      collection_name = collection.Bench.name;
      groups =
        List.map
          (fun (g : Bench.group) ->
            {
              name = g.name;
              tests = List.map (fun (t : Bench.test) -> t.name) g.tests;
            })
          collection.groups;
    }

  let to_json { projects; project_name; project_url; collection_name; groups } :
      Mustache.Json.t =
    `O
      [
        ("projects", `A (List.map (fun name -> `String name) projects));
        ("project_name", `String project_name);
        ("project_url", `String project_url);
        ("collection_name", `String collection_name);
        ( "groups",
          `A
            (List.map
               (fun group ->
                 `O
                   [
                     ("name", `String group.name);
                     ( "tests",
                       `A (List.map (fun test -> `String test) group.tests) );
                   ])
               groups) );
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

let write_collection_data ~project_name project_dir
    (collection : Bench.collection) =
  let filename = project_name ^ "--" ^ collection.name ^ ".json" in
  let filepath = Filename.concat project_dir filename in
  let json = Bench.Json.of_collection collection in
  let pp = Yojson.Safe.pretty_to_channel ~std:false in
  Out_channel.with_open_text filepath (fun oc -> pp oc json)

let copy_html_assets root =
  Out_channel.with_open_text (Filename.concat root "uPlot.iife.min.js")
    (fun oc -> Out_channel.output_string oc Asset.uPlot_js);
  Out_channel.with_open_text (Filename.concat root "uPlot.min.css") (fun oc ->
      Out_channel.output_string oc Asset.uPlot_css);
  Out_channel.with_open_text (Filename.concat root "alpine.min.js") (fun oc ->
      Out_channel.output_string oc Asset.alpine_js);
  Out_channel.with_open_text (Filename.concat root "plot.js") (fun oc ->
      Out_channel.output_string oc Asset.plot_js);
  Out_channel.with_open_text (Filename.concat root "logo-with-name.svg")
    (fun oc -> Out_channel.output_string oc Asset.logo_with_name_svg);
  Out_channel.with_open_text (Filename.concat root "main.css") (fun oc ->
      Out_channel.output_string oc Asset.main_css);
  ()

let generate root t =
  if not (Sys.file_exists root) then Unix.mkdir root 0o755;
  copy_html_assets root;

  let index_html = render_index_html t in
  Out_channel.with_open_text (Filename.concat root "index.html") (fun oc ->
      Out_channel.output_string oc index_html);

  (* Generate projects *)
  let project_dir_root = Filename.concat root "_" in
  if not (Sys.file_exists project_dir_root) then
    Unix.mkdir project_dir_root 0o755;
  List.iter
    (fun (project : Bench.project) ->
      let project = Bench.Process.align_commits ~active_only:true project in
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
          write_collection_data ~project_name:project.name project_dir_root
            collection;
          Out_channel.with_open_text filepath (fun oc ->
              Out_channel.output_string oc
                (render_collection_html t project collection)))
        project.collections)
    t
