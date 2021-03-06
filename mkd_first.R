rm(list = ls())

# setup
default <- list(
  # I/O
  FILE_header = "header.R",  # header file to source from
  FILE_planning = "planning_funs.R",  # source planning functions from
  PLANNING_FUN = "planning_mkd", # planning function to use, either planning or planning_mkd
  DIR_planning_results = "planning_mkd_results",  # directory of planning_results to export to and import from
  FILE_id_results = "id_mkd_results.csv",  # id table for planning_results
  FILE_lock = "planning_mkd.lock",  # auxiliary file to flag blocking concurrent access
  FILE_seasonality = NULL,  # import seasonal factors from FILE_seasonality
  FILE_export = "mkd_first.RData",  # saving result to FILE_export
  # economic inputs
  Q = 300,  # initial inv level
  T_mkd_first = 100,  # maximal markdown first duration
  T_mkd = 100,  # maximal markdown duration
  h = 0.01,  # variable holding cost (per unit time per unit inventory)
  h0 = 0,  # fixed holding cost (per unit time if inventory level > 0)
  gamma = log(1/(1 - 0.15))/365,  # discounting factor
  # decision inputs
  price_base = 1,
  price_floor = 0,
  price_liq = 0,  # liquidation price
  price_seq_by = 0.05,  # generate price sequence of increment = 0.05
  # prior specification of Logit demand model parameters (log_demand_base, beta, time_trend)
  PRIOR = "unif",  # type of prior, "unif" or "norm"
  ### normal prior
  log_demand_base_mean = 0,
  log_demand_base_sd = 0,
  beta_mean = 1,
  beta_sd = 1,
  time_trend_mean = 0,
  time_trend_sd = 0,
  ### uniform prior
  log_demand_base_low = 0,
  log_demand_base_upp = 0,
  beta_low = 0,
  beta_upp = 5,
  time_trend_low = 0,
  time_trend_upp = 0,
  # minimal distances of parameters (for hashing)
  log_demand_base_min_dist = 0.05,
  beta_min_dist = 0.05,
  time_trend_min_dist = 0.05,
  # simulation setting inputs
  ### oracle (data generating) Logit demand model parameters
  log_demand_base_orcl = 0,
  beta_orcl = 1,
  time_trend_orcl = 0,
  ### other setups
  demand_asm = "STC", # demand assumption when planning
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

# oracle parameters
param_orcl <- c(log_demand_base = log_demand_base_orcl, beta = beta_orcl, time_trend = time_trend_orcl)

# oracle demand function (for testing data generation)
demand_orcl <- demand(param_orcl)

result_mkd_first <- mkd_first(demand_orcl)(Q, T_mkd_first, T_mkd)
result_mkd_first[c("value", "clearance_time", "recovery_rate")]

result <- list(result = result_mkd_first, param_oracle = param_orcl, demand_oracle = demand_orcl)
save(result, file = FILE_export)
