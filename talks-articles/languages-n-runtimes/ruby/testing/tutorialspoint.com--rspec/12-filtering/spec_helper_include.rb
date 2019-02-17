RSpec.configure do |cfg|
  # filter_run short of filter_run_including, to run focused set
  cfg.filter_run suspicious: true
end
