-- Display output in tuples only mode
\t on
-- Set output format to unaligned
\pset format unaligned

-- Dump dune default benchmark data
WITH t AS (
     SELECT commit,run_at,pull_number,benchmark_name,test_name,metrics,repo_id FROM benchmarks
     WHERE
        repo_id = 'ocaml/dune' AND
        worker = 'autumn' AND
        docker_image = 'bench.Dockerfile' AND
        (branch = 'main' OR pull_number = NULL)
     ORDER BY run_at DESC
)
SELECT json_agg(t) FROM t \g 'dune--default.json'

-- Dump dune monorepo benchmark data
WITH t AS (
     SELECT commit,run_at,pull_number,'monorepo' AS benchmark_name,test_name,metrics,repo_id FROM benchmarks
     WHERE
        repo_id = 'ocaml/dune' AND
        worker = 'fermat' AND
        docker_image = 'bench/monorepo/bench.Dockerfile' AND
        (branch = 'main' OR pull_number = NULL)
     ORDER BY run_at DESC
)
SELECT json_agg(t) FROM t \g 'dune--monorepo.json'
