library(dplyr)  # filter
library(rlang)  # as_function
library(filelock)  # lock
library(mcmc)  # metrop
library(lbfgs)  # lbfgs
library(doParallel)  # parallelized

default <- list(
  # I/O
  FILE_planning = "planning_funs.R",  # source planning functions from
  PLANNING_FUN = "planning", # planning function to use, either planning or planning_mkd
  DIR_planning_results = "planning_results",  # directory of planning_results to export to and import from
  FILE_id_results = "id_results.csv",  # id table for planning_results
  FILE_lock = "planning.lock",  # auxiliary file to flag blocking concurrent access
  FILE_seasonality = NULL,  # import seasonal factors from FILE_seasonality
  FILE_export = "mkd_first.RData",  # saving result to FILE_export
  # economic inputs
  Q = 300,  # initial inv level
  T = 100,  # maximal planning horizon
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
  inv_seq_by = 10,  # generate next-day inventory sequence of increment = 10
  # implementation inputs
  prob0 = 1,  # base randomization probability for prices; day-t pricing randomly with probability prob0 * 2/(1+exp(t * variance of prices up to day t))
  delta0 = 0.1,  # base distance of price; prices satisfying distance(price, historical mean of prices) > delta0 * (price_base - price_floor) are considered for randomized pricing
  regret_factor = 2,  # screening eligible randomized prices by regret_factor * minimal quantile regret
  regret_prop = 0.3,  # screening eligible randomized prices by regret_prop if there is no eligible price for regret_factor * minimal quantile regret
  alpha = 0.1,  # confidence level for regret
  n_sim = 1000,  # size of MCMC sample
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
  log_demand_base_orcl = 1,
  beta_orcl = 1,
  time_trend_orcl = 0,
  ### other setups
  demand_asm = "STC", # demand assumption when planning
  n_core = 1,  # number of cores
  n_rep = 1,  # number of testing replications
  verbose = TRUE  # if TRUE, printing details
)
inputs <- names(default)

# setup from default
for(input in inputs) {
  if(!exists(input)) {
    assign(input, default[[input]])
  }
}
if(!is.numeric(T)) {
  T <- default[["T"]]
}
if(!is.numeric(gamma)) {
  gamma <- default[["gamma"]]
}

# source planning functions
source(FILE_planning)
planning_fun <- get(PLANNING_FUN)

# indices
id.t <- function(t) { t+1 }
id.q <- function(q) { q+1 }

# guardrail
guard <- function(u, low, upp) { pmin(pmax(u, low), upp) }

# make parallel cluster
registerDoParallel(cores = n_core)

# prior specification of Logit demand model parameters (log_demand_base, beta, time_trend): param_mean, param_sd, param_min_dist
param_names <- c("log_demand_base", "beta", "time_trend")
for(STAT in c("mean", "sd", "low", "upp", "min_dist")) {
  param_STAT <- sapply(param_names, function(PARAM) get(paste0(PARAM, "_", STAT)))
  names(param_STAT) <- param_names
  assign(paste0("param_", STAT), param_STAT)
}
if(PRIOR == "unif") {
  # parameters to learn (should at least contain beta): upper - lower >= minimal distance / 4
  param_learn <- names(which(abs(param_upp - param_low) >= param_min_dist/2))
  param_mean <- (param_upp + param_low) / 2
} else if(PRIOR == "norm") {
  # parameters to learn (should at least contain beta): sd >= minimal distance / 4
  param_learn <- names(which(param_sd >= param_min_dist/4))
  param_low <- c(log_demand_base = -Inf, beta = 0, time_trend = -Inf)
  param_upp <- c(log_demand_base = Inf, beta = Inf, time_trend = Inf)
}
# import seasonal factors season = a length-(T+1) vector from FILE_seasonality
if(is.null(FILE_seasonality)) {
  season <- rep(1, T+1)
} else {
  load(FILE_seasonality)
  if(!exists("season")) {
    season <- rep(1, T+1)
  } else if(length(season) < T+1) { # extending season
    season <- replace(season, (length(season)+1):(T+1), season[length(season)])
  }
  season <- season[1:(T+1)]
}
names(season) <- 0:T

# demand function of Logit model
DEMAND <- function(t, p, ...) {
  inputs <- c("demand_base", "beta", "time_trend", "season_factor")
  for(input in inputs) {
    assign(input, list(...)[[input]])
  }
  season_factor * demand_base * exp(- beta * p - time_trend * t)
}

# demand function class given input parameters
demand <- function(param) {
  # param = a named vector of Logit demand model parameters (log_demand_base, beta, time_trend)
  param <- as.numeric(param)[1:3]
  if(is.null(names(param))) {
    names(param) <- c("log_demand_base", "beta", "time_trend")
  }
  function(t, p)
    DEMAND(t, p, 
           demand_base = exp(param[["log_demand_base"]]),
           beta = param[["beta"]], 
           time_trend = param[["time_trend"]], 
           season_factor = season[pmin(id.t(t), length(season))])  # if id.t(t) > length(season), then seasonality factor = the last element in season
}

# generate price and next-day inventory sequence
price_seq <- seq(price_floor, price_base, price_seq_by)
inv_seq <- seq(0, Q, inv_seq_by)

# setup planning_results, planning_mkd_results directories and corresponding id tables
if(!dir.exists(DIR_planning_results)) {
  dir.create(DIR_planning_results)
}
if(!file.exists(FILE_id_results)) {
  id_table <- data.frame(matrix(nrow = 0, ncol = 4, dimnames = list(NULL, c("log_demand_base", "beta", "time_trend", "id"))))
  write.table(id_table, file = FILE_id_results, sep = ",", quote = FALSE, row.names = FALSE, col.names = TRUE)
}
# functions for hashing
### round parameter to a grid
round_param <- function(param) {
  # param = a vector, matrix or data.frame of Logit demand model parameters (log_demand_base, beta, time_trend)
  param_names <- c("log_demand_base", "beta", "time_trend")
  if(is.matrix(param) | is.data.frame(param)) { # param is a matrix or a data.frame
    if(nrow(param) > 1) {
      param <- param[,1:3] 
      colnames(param) <- param_names
      for(PARAM in param_names) {
        param[,PARAM] <- round(param[,PARAM] / param_min_dist[PARAM]) * param_min_dist[PARAM]
      }
      return(param)
    }
  } 
  # param is a vector
  param <- as.numeric(param)[1:3]
  names(param) <- param_names
  param <- round(param / param_min_dist) * param_min_dist
  return(param)
}
### hash function: round params to grids -> look up ids from FILE_id_results -> planning if id not found -> output id
hash_fun <- function(param) {
  # param = a vector, matrix or data.frame of Logit demand model parameters (log_demand_base, beta, time_trend)
  param <- round_param(param)  # round param to grids
  if(is.matrix(param) | is.data.frame(param)) { # param is a matrix or a data.frame
    PARAM <- data.frame(param) %>% mutate_all(as.character)  # convert to character for matching
    PARAM_group <- group_by_all(data.frame(PARAM))
    PARAM_unique <- summarise_all(PARAM_group, first) %>% ungroup %>% mutate(id_order = row_number())
    freq <- group_size(PARAM_group)  # repeated times
    
    # read the most recent id table
    id_table <- read.csv(FILE_id_results, colClasses = "character")
    # look up parameter from id_table
    unplanned <- anti_join(PARAM_unique, id_table, by = param_names)
    # planning for unplanned
    if(nrow(unplanned) > 0) {
      foreach(id_param = 1:nrow(unplanned)) %dopar% { # parallelized
        PARAM0 <- unplanned[id_param, param_names]  # data.frame, character
        param <- as.numeric(PARAM0)  # numeric for planning
        # planning
        if(verbose) {
          print(paste0("Markdown + Liquidation Planning for ", 
                       "log_demand_base = ", param[1], "; ",
                       "beta = ", param[2], "; ",
                       "time_trend = ", param[3]))
        }
        ### planning for demand function = demand(param)
        planning_result <- planning_fun(demand(param))(
          price_seq = price_seq, price_liq = price_liq, inv_seq = inv_seq,
          Q = Q, T = T, h = h, h0 = h0, gamma = gamma,
          demand_asm = demand_asm, verbose = verbose)
        ### export planning_result to DIR_planning_results and id_table to FILE_id_results
        ##### block concurrent access
        LOCK <- lock(FILE_lock)
        ##### read the most recent id table
        id_table <- read.csv(FILE_id_results, colClasses = "character")
        # look up parameter from id_table
        if(nrow(semi_join(PARAM0, id_table, by = param_names)) == 0) {
          ##### generate a new id
          id <- nrow(id_table) + 1
          ##### update planning_results
          save(planning_result, file = paste0(DIR_planning_results, "/planning_result_", id, ".RData"))
          ##### add new id record to id table
          id_record <- data.frame(PARAM0, id = id)
          write.table(id_record, file = FILE_id_results, sep = ",", quote = FALSE,
                      row.names = FALSE, col.names = FALSE, append = TRUE)
          ##### unblock concurrent access
          unlock(LOCK)
        }
      }
    }
    # read the most recent id table
    id_table <- read.csv(FILE_id_results, colClasses = "character")
    # look up parameter from id_table
    look_up <- distinct_at(id_table, param_names, .keep_all = TRUE) %>% 
      right_join(PARAM_unique, by = param_names) %>% 
      arrange(by = id_order)
    return(rep(look_up$id, freq))
  } else { # param is a vector
    # read the most recent id table
    id_table <- read.csv(FILE_id_results, colClasses = "character")
    # look up parameter from id_table
    PARAM0 <- data.frame(t(param)) %>% mutate_all(as.character)
    look_up <- inner_join(PARAM0, id_table, by = param_names)
    if(nrow(look_up) == 0) { # not planned yet
      # planning
      if(verbose) {
        print(paste0("Markdown + Liquidation Planning for ", 
                     "log_demand_base = ", param[1], "; ",
                     "beta = ", param[2], "; ",
                     "time_trend = ", param[3]))
      }
      ### planning for demand function = demand(param)
      planning_result <- planning_fun(demand(param))(
        price_seq = price_seq, price_liq = price_liq, inv_seq = inv_seq,
        Q = Q, T = T, h = h, h0 = h0, gamma = gamma,
        demand_asm = demand_asm, verbose = verbose)
      ### export planning_result to DIR_planning_results and id_table to FILE_id_results
      ##### block concurrent access
      LOCK <- lock(FILE_lock)
      ##### read the most recent id table
      id_table <- read.csv(FILE_id_results, colClasses = "character")
      # look up parameter from id_table
      if(nrow(semi_join(PARAM0, id_table, by = param_names)) == 0) {
        ##### generate a new id
        id <- nrow(id_table) + 1
        ##### update planning_results
        save(planning_result, file = paste0(DIR_planning_results, "/planning_result_", id, ".RData"))
        ##### add new id record to id table
        id_record <- data.frame(PARAM0, id = id)
        write.table(id_record, file = FILE_id_results, sep = ",", quote = FALSE,
                    row.names = FALSE, col.names = FALSE, append = TRUE)
        ##### unblock concurrent access
        unlock(LOCK)
      }
      return(id)
    } else { # id for param found from id table
      return(look_up$id[1])
    }
  }
}

# log-unnormalized-posterior density for MCMC
if(PRIOR == "unif") {
  log_prior <- function(param0) {
    if(any(param0 < param_low[param_learn] - 1e-5 | param0 > param_upp[param_learn] + 1e-5)) {
      return(-Inf)
    } else {
      return(0)
    }
  }
  grad_log_prior <- function(param0) {
    return(rep(0, length(param0)))
  }
} else if(PRIOR == "norm") {
  log_prior <- function(param0) {
    if(any(param0 < param_low[param_learn] | param0 > param_upp[param_learn])) {
      return(-Inf)
    } else {
      return(sum(log(dnorm(param0, mean = param_mean[param_learn], sd = param_sd[param_learn]))))
    }
  }
  grad_log_prior <- function(param0) {
    return(- (param0 - param_mean[param_learn]) / param_sd[param_learn]^{2})
  }
} else {
  stop("PRIOR type should be specified!")
}
log_posterior <- function(price_demand = NULL) function(param0) {
  # price_demand: either NULL or a matrix of three columns: ts = price_demand[,"t"], prices = price_demand[,"price"], demands = price_demand[,"demand"]
  # param0: a vector of Logit demand model parameters to learn, length(param0) = length(param_learn)
  LOG_PRIOR <- log_prior(param0)
  if(LOG_PRIOR == -Inf) {
    return(-Inf)
  }
  param <- replace(param_mean, param_learn, param0)
  if(is.null(price_demand)) {
    log_likelihood <- 0
  } else {
    ts <- price_demand[,"t"]
    prices <- price_demand[,"price"]
    demands <- price_demand[,"demand"]
    
    demands_mean <- demand(param)(ts, prices)
    log_likelihood <- sum(ifelse(demands == 0, 0, demands * log(demands_mean)) - demands_mean)
  }
  return(LOG_PRIOR + log_likelihood)
}
# gradient of log-posterior density
if(length(param_learn) == 1) {
  grad_log_posterior <- function(price_demand = NULL) function(param0) {
    # price_demand: either NULL or a matrix of three columns: ts = price_demand[,"t"], prices = price_demand[,"price"], demands = price_demand[,"demand"]
    # param0: a vector of Logit demand model parameters to learn, length(param0) = length(param_learn)
    param <- replace(param_mean, param_learn, param0)
    if(is.null(price_demand)) {
      grad_log_likelihood <- 0
    } else {
      ts <- price_demand[,"t"]
      prices <- price_demand[,"price"]
      demands <- price_demand[,"demand"]
      
      grad_log_demands_mean <- cbind(log_demand_base = 1, beta = - prices, time_trend = ts)[,param_learn]
      demands_mean <- demand(param)(ts, prices)
      grad_demands_mean <- demands_mean * grad_log_demands_mean
      grad_log_likelihood <- sum(demands * grad_log_demands_mean - grad_demands_mean)
    }
    return(grad_log_prior(param0) + grad_log_likelihood)
  }
} else { # length(param_learn) > 1
  grad_log_posterior <- function(price_demand = NULL) function(param0) {
    # price_demand: either NULL or a matrix of three columns: ts = price_demand[,"t"], prices = price_demand[,"price"], demands = price_demand[,"demand"]
    # param0: a vector of Logit demand model parameters to learn, length(param0) = length(param_learn)
    param <- replace(param_mean, param_learn, param0)
    if(is.null(price_demand)) {
      grad_log_likelihood <- rep(0, length(param0))
    } else {
      ts <- price_demand[,"t"]
      prices <- price_demand[,"price"]
      demands <- price_demand[,"demand"]
      
      grad_log_demands_mean <- cbind(log_demand_base = 1, beta = - prices, time_trend = ts)[,param_learn]
      demands_mean <- demand(param)(ts, prices)
      grad_demands_mean <- demands_mean * grad_log_demands_mean
      if(nrow(price_demand) > 1) {
        grad_log_likelihood <- colSums(demands * grad_log_demands_mean - grad_demands_mean)
      } else {
        grad_log_likelihood <- demands * grad_log_demands_mean - grad_demands_mean
      }
    }
    return(grad_log_prior(param0) + grad_log_likelihood)
  }
}

# markdown first prototype
mkd_first <- function(DEMAND = function(t, p) { return(0) }) 
  function(Q, T, T_mkd, price_demand = NULL, param0 = param_mean[param_learn], t_ref = 0) {
    # DEMAND: demand function for data generation
    # T_mkd: maximal markdown duration
    # price_demand: price-demand history, either NULL or a matrix of three columns: ts = price_demand[,"t"], prices = price_demand[,"price"], demands = price_demand[,"demand"]
    # param0: initial param0 for lbfgs
    # t_ref: reference day
    print(paste0("Markdown First: reference day ", t_ref))
    T_maxes <- price_floors <- sales <- revs <- invs <- numeric()
    HIT_FLOOR <- logical()
    params <- data.frame(log_demand_base = numeric(), beta = numeric(), time_trend = numeric())
    value <- 0
    
    invs[id.t(0)] <- q <- Q
    for(t in 0:T) {
      if(verbose) {
        print(paste0("Markdown First t = ", t))
      }
      
      # estimation based on maximal posterior mode
      param0 <- lbfgs(call_eval = function(param0) { pmin(- log_posterior(price_demand)(param0), 1e8) },
                      call_grad = function(param0) { - grad_log_posterior(price_demand)(param0) },
                      vars = param0, invisible = 1)$par
      param0 <- guard(param0, param_low[param_learn], param_upp[param_learn])
      params[id.t(t),] <- param <- replace(param_mean, param_learn, param0)
      
      # decision planning
      ### maximal markdown duration
      if(is.null(price_demand)) {
        T_maxes[id.t(t)] <- T_max <- T
      } else {
        T_maxes[id.t(t)] <- T_max <- pmin(Q/mean(price_demand[,"demand"]), T)
      }
      ### price floor
      price_floors[id.t(t)] <- price_floor_t <- price_base - (price_base - price_floor)/T_max * t
      ### pricing
      SUM <- sum(sapply(t:T_max, function(s) {
        season[id.t(s)] * exp(-1 + param[["beta"]] * h * (T_max - s + 1) + param[["time_trend"]] * s)
      }))
      lambda <- (1/param[["beta"]]) * log(exp(param[["log_demand_base"]]) * SUM / q)
      p <- pmin((1/param[["beta"]]) + lambda + (T_max - t + 1) * h, price_base)
      if(p <= price_floor_t) {
        HIT_FLOOR[id.t(t)] <- TRUE
        p <- price_floor_t
      } else { # p > price_floor_t
        HIT_FLOOR[id.t(t)] <- FALSE
      }
      
      # observations
      D <- rpois(1, DEMAND(t, p))
      ### updating historical price-demand observations
      price_demand <- rbind(price_demand, c(t = t_ref + t, price = p, demand = D))
      
      # state transitions and payoffs
      sales[id.t(t)] <- sale <- pmin(D, q)
      revs[id.t(t)] <- p * sale
      if(t < T) {
        invs[id.t(t+1)] <- q_next <- q - sale
        value <- value + exp(-gamma*t) * (p * sale - h * q_next - h0 * (q_next > 0))
        if((q <- q_next) == 0) { 
          MKD <- FALSE
          clearance_time <- t
          recovery_rate <- sum(revs) / Q
          q_liq <- 0
          break
        }
      } else { # t == T
        value <- value + exp(-gamma*t) * p * sale
        if((q_remain <- q - sale) > 0) { # there is remaining inventory, conversion to markdown or proceed with liquidation
          if(all(HIT_FLOOR[id.t((t-13):T)])) { # remains at price floor for >= 14 days
            # liquidate
            MKD <- FALSE
            value <- value + exp(-gamma*T) * price_liq * q_remain
            clearance_time <- T
            recovery_rate <- (sum(revs) + price_liq * q_remain) / Q
            q_liq <- q_remain
          } else {
            # markdown sale
            MKD <- TRUE
            mkd_result <- mkd_sale(DEMAND)(Q = q_remain, T = T_mkd, price_demand = price_demand, param0 = param0, t_ref = t_ref + T + 1)
            value <- value + exp(-gamma*T) * mkd_result$value
            clearance_time <- T + 1 + mkd_result$clearance_time
            recovery_rate <- (sum(revs) + mkd_result$recovery_rate * q_remain) / Q
            price_demand <- mkd_result$price_demand
            sales <- c(sales, mkd_result$sales)
            invs <- c(invs, mkd_result$invs)
            q_liq <- mkd_result$liq
            params <- rbind(params, mkd_result$params)
          }
        } else { # q_remain = 0, inventory cleared at day T
          MKD <- FALSE
          clearance_time <- T
          recovery_rate <- sum(revs) / Q
          q_liq <- 0
        }
      }
    }
    
    return(list(value = value, clearance_time = clearance_time, recovery_rate = recovery_rate,
                price_demand = price_demand, T_maxes = T_maxes, price_floors = price_floors, sales = sales, invs = invs, liq = q_liq, 
                HIT_FLOOR = HIT_FLOOR, MKD = MKD,
                params = params))
  }

# markdown sale prototype
mkd_sale <- function(DEMAND = function(t, p) { return(0) }) 
  function(Q, T, price_demand = NULL, param0 = param_mean[param_learn], t_ref = 0) {
    # DEMAND: demand function for data generation
    # price_demand: price-demand history, either NULL or a matrix of three columns: ts = price_demand[,"t"], prices = price_demand[,"price"], demands = price_demand[,"demand"]
    # param0: initial param0 for lbfgs
    # t_ref: reference day
    print(paste0("Markdown Sale: reference day ", t_ref))
    sales <- revs <- invs <- numeric()
    params <- data.frame(log_demand_base = numeric(), beta = numeric(), time_trend = numeric())
    value <- 0
    
    invs[id.t(0)] <- q <- Q
    for(t in 0:T) {
      if(verbose) {
        print(paste0("Markdown Sale t = ", t))
      }
      
      # estimation based on maximal posterior mode
      param0 <- lbfgs(call_eval = function(param0) { pmin(- log_posterior(price_demand)(param0), 1e8) },
                      call_grad = function(param0) { - grad_log_posterior(price_demand)(param0) },
                      vars = param0, invisible = 1)$par
      param0 <- guard(param0, param_low[param_learn], param_upp[param_learn])
      params[id.t(t),] <- param <- replace(param_mean, param_learn, param0)
      
      # decision planning
      ### extract planning_result
      id <- hash_fun(param)
      load(paste0(DIR_planning_results, "/planning_result_", id, ".RData"))  # import planning_result
      p <- planning_result$price_opt[id.t(t + planning_result$T - T), id.q(q)]
      
      # observations
      D <- rpois(1, DEMAND(t, p))
      ### updating historical price-demand observations
      price_demand <- rbind(price_demand, c(t = t_ref + t, price = p, demand = D))
      
      # state transitions and payoffs
      sales[id.t(t)] <- sale <- pmin(D, q)
      revs[id.t(t)] <- p * sale
      if(t < T) {
        invs[id.t(t+1)] <- q_next <- q - sale
        value <- value + exp(-gamma*t) * (p * sale - h * q_next - h0 * (q_next > 0))
        if((q <- q_next) == 0) { 
          clearance_time <- t
          q_liq <- 0
          break
        }
      } else { # t == T
        q_liq <- q - sale
        value <- value + exp(-gamma*T) * (p * sale + price_liq * q_liq)
        clearance_time <- T
      }
    }
    recovery_rate <- (sum(revs) + price_liq * q_liq) / Q
    
    return(list(value = value, clearance_time = clearance_time, recovery_rate = recovery_rate,
                price_demand = price_demand, sales = sales, invs = invs, liq = q_liq,
                params = params))
  }

# markdown first RL
mkd_first_RL <- function(DEMAND = function(t, p) { return(0) }) 
  function(Q, T, price_demand = NULL, param0 = param_mean[param_learn], t_ref = 0) {
    # DEMAND: demand function for data generation
    # price_demand: price-demand history, either NULL or a matrix of three columns: ts = price_demand[,"t"], prices = price_demand[,"price"], demands = price_demand[,"demand"]
    # param0: initial param0 for metrop
    # t_ref: reference day
    print(paste0("Markdown First RL: reference day ", t_ref))
    template <- rep(0, T+1)
    names(template) <- 0:T
    price_opt <- inv_opt <- probs <- liqs <- sales <- revs <- invs <- template
    params <- list()
    value <- 0
    
    invs[id.t(0)] <- q <- Q
    for(t in 0:T) {
      if(verbose) {
        print(paste0("Markdown First RL t = ", t))
      }
      
      # generating param sample
      ### initial param0 for MCMC such that log_posterior(price_demand)(param0) > -Inf
      while(log_posterior(price_demand)(param0) == -Inf) { 
        param0 <- rnorm(length(param_learn), mean = param_mean[param_learn], sd = param_sd[param_learn])
      }
      mcmc <- metrop(log_posterior(price_demand), param0, n_sim)
      param0 <- mcmc$final
      param_sim <- t(replicate(n_sim, param_mean))
      param_sim[, param_learn] <- mcmc$batch
      params[[id.t(t)]] <- param_sim
      
      # decision planning
      ### extract planning_results
      ids <- hash_fun(param_sim)
      ids_unique <- unique(ids)
      ids_freq <- table(ids)
      planning_results <- foreach(id = ids_unique, .combine = c) %dopar% {
        load(paste0(DIR_planning_results, "/planning_result_", id, ".RData"))  # import planning_result
        list(planning_result)
      }
      if(t == T) {
        q_opt <- 0  # liquidate by the end
        ### compute price regret
        regret_price <- foreach(planning_result = planning_results, .combine = rbind) %dopar% {
          price_value(planning_result, T, q, 0, price_seq)$regret_price
        }
        ### compute price quantile-regret
        regret_quan_price <- apply(regret_price, 2, function(regrets) {
          quantile(rep(regrets, ids_freq), prob = 1-alpha)
        })
        id_price <- which.min(regret_quan_price)
        price_opt[id.t(t)] <- p_opt <- price_seq[id_price]
      } else { # t < T
        ### compute next-day-inventory regret
        regret_inv <- foreach(planning_result = planning_results, .combine = rbind) %dopar% {
          inv_value(planning_result, t, q, inv_seq)$regret_inv
        }
        ### compute next-day-inventory quantile-regret
        regret_quan_inv <- apply(regret_inv, 2, function(regrets) {
          quantile(rep(regrets, ids_freq), prob = 1-alpha)
        })
        ### optimize for next-day-inventory
        id_inv <- which.min(regret_quan_inv)
        inv_opt[id.t(t)] <- q_opt <- inv_seq[id_inv]
        ### compute price regret
        regret_price <- foreach(planning_result = planning_results, .combine = rbind) %dopar% {
          price_value(planning_result, t, q, q_opt, price_seq)$regret_price
        }
        ### compute price quantile-regret
        regret_quan_price <- apply(regret_price, 2, function(regrets) {
          quantile(rep(regrets, ids_freq), prob = 1-alpha)
        })
        id_price <- which.min(regret_quan_price)
        price_opt[id.t(t)] <- p_opt <- price_seq[id_price]
      }
      
      # implementation
      ### randomized price
      ##### compute price_mean and price_var
      if(is.null(price_demand)) {
        price_mean <- price_base
        price_var <- 0
      } else if(nrow(price_demand) == 1) {
        price_mean <- price_demand[,"price"]
        price_var <- 0
      } else {
        price_mean <- mean(price_demand[,"price"])
        price_var <- var(price_demand[,"price"])
      }
      ##### screening eligible prices
      id_dist <- abs(price_seq - price_mean) > delta0 * (price_base - price_floor)  # screening prices by distance from price_mean
      id_regret_min <- regret_quan_price <= regret_factor * regret_quan_price[id_price]  # screening prices by regret_factor * minimal quantile regret
      id_regret_quan <- regret_quan_price <= quantile(regret_quan_price, prob = regret_prop)  # screening prices by regret_prop if there is no eligible price for regret_factor * minimal quantile regret rule
      if(any(id_dist & id_regret_min)) {
        price_set <- price_seq[id_dist & id_regret_min]
      } else { # no eligible price for regret_factor * minimal quantile regret rule
        price_set <- price_seq[id_dist & id_regret_quan]
      }
      ##### randomization probability
      probs[id.t(t)] <- prob <- 2*prob0/(1+exp(t*price_var))
      ##### generate randomized price
      p <- ifelse(rbinom(1, 1, prob), sample(price_set, 1), p_opt)
      
      # observations
      D <- rpois(1, DEMAND(t, p))
      ### updating time-price-demand observations
      price_demand <- rbind(price_demand, c(t = t_ref + t, price = p, demand = D))
      
      # state transitions and payoffs
      sales[id.t(t)] <- sale <- pmin(D, q)
      revs[id.t(t)] <- p * sale
      if(t < T) {
        q_next <- q - sale
        if(q_next >= q_opt) {
          liqs[id.t(t)] <- q_liq <- q_next - q_opt
          invs[id.t(t+1)] <- q_next <- q_opt
        } else {
          liqs[id.t(t)] <- q_liq <- 0
          invs[id.t(t+1)] <- q_next
        }
        value <- value + exp(-gamma*t) * (p * sale + price_liq * q_liq - h * q_next - h0 * (q_next > 0))
        if((q <- q_next) == 0) { 
          clearance_time <- t
          price_opt[id.t((t+1):T)] <- p_opt
          break
        }
      } else { # t == T
        liqs[id.t(T)] <- q_liq <- q - sale
        value <- value + exp(-gamma*T) * (p * sale + price_liq * q_liq)
        clearance_time <- T
      }
    }
    recovery_rate <- (sum(revs) + price_liq * sum(liqs)) / Q
    
    return(list(value = value, clearance_time = clearance_time, recovery_rate = recovery_rate,
                price_demand = price_demand, price_opt = price_opt, inv_opt = inv_opt, prob = probs, liq = liqs, sale = sales, inv = invs,
                params = params))
  }
