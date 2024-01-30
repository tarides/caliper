open Bench

let template = Mustache.of_string Asset.index_html

let json_of_benchmark_history (t : Bench.t) =
  `O
    [
      ("collections", `A (List.map (fun c -> `O [ ("name", `String c.name) ]) t));
    ]

let render_html t = Mustache.render template (json_of_benchmark_history t)

let generate root t =
  if not (Sys.file_exists root) then Unix.mkdir root 0o755;
  Out_channel.with_open_text (Filename.concat root "chart.js") (fun oc ->
      Out_channel.output_string oc Asset.chart_js);
  Out_channel.with_open_text (Filename.concat root "main.css") (fun oc ->
      Out_channel.output_string oc Asset.main_css);
  let index_html = render_html t in
  Out_channel.with_open_text (Filename.concat root "index.html") (fun oc ->
      Out_channel.output_string oc index_html)
