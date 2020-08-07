rm(list = ls())

# setup
default <- list(
  # I/O
  FILE_header = "header.R",  # header file to source from
  FILE_planning = "planning_funs.R",  # source planning functions from
  PLANNING_FUN = "planning", # planning function to use, either planning or planning_mkd
  DIR_planning_results = "planning_results",  # directory of planning_results to import from
  FILE_id_results = "id_results.csv",  # id table for planning_results
  FILE_lock = "planning.lock",  # auxiliary file to flag blocking concurrent access
  FILE_seasonality = NULL,  # import seasonal factors from FILE_seasonality
  # economic inputs
  Q = 300,  # initial inv level
  T = 100,  # maximal planning horizon
  h = 0.01,  # variable holding cost (per unit time per unit inventory)
  h0 = 0,  # fixed holding cost (per unit time if inventory level > 0)
  gamma = log(1/(1 - 0.15))/365,  # discounting factor
  # decision inputs
  price_base = 1,
  price_floor = 0,
  price_liq = 0,  # liquidation price
  price_seq_by = 0.05,  # generate price sequence of increment = 0.05
  inv_seq_by = 10,  # generate next-day inventory sequence of increment = 10
  # minimal distances of parameters (for hashing)
  log_demand_base_min_dist = 0.05,
  beta_min_dist = 0.05,
  time_trend_min_dist = 0.05,
  # planning Logit demand model parameter (log_demand_base, beta, time_trend)
  log_demand_base = 1,
  beta = 1,
  time_trend = 0,
  # other setups
  demand_asm = "STC",  # demand assumption when planning
  n_core = 1,  # number of cores
  verbose = TRUE  # if TRUE, printing details
)
inputs <- names(default)

# import setup from command line --args
args <- commandArgs(trailingOnly = T); args
for(input in inputs) {
  id <- grep(paste0("^", input, "="), args)
  if(length(id)) { # input from args
    eval(parse(text = args[id]))
  } else { # set as default
    assign(input, default[[input]])
  }
  print(paste0(input, " = ", get(input)))
}

source(FILE_header)

hash_fun(c(log_demand_base, beta, time_trend))
