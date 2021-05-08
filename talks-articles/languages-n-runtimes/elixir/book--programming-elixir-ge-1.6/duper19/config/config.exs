use Mix.Config

config :duper19, results: :dup_results
config :duper19, pathfinder: :dup_pathfinder
config :duper19, worker_supervisor: :dup_worker_sup
config :duper19, worker: :dup_worker
config :duper19, gatherer: :dup_gatherer

config :duper19, dirwalker: :dup_dirwalker
config :duper19, mystash: :dup_stash
