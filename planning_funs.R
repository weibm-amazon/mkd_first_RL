# required inputs:
#   demand function: demand
#   Q: initial inv
#   T: maximal planning horizon
#   h: variable holding cost per unit inv per unit time
#   h0: fixed holding cost per unit time
#   gamma: discounting factor, defaulted to be annual 15%
#   demand_asm: assumption on demand, deterministic ("DET") or stochastic ("STC")

# indices
id.t <- function(t) { t+1 }
id.q <- function(q) { q+1 }

# approximating expectation of some function of Poisson(lambda) <= K
pois_exp <- function(K, lambda, by_base = 0.5, TRUNC_base = 3) {
  by <- pmax(1, floor(by_base * lambda))
  TRUNC <- pmin(ceiling(TRUNC_base * lambda), K)
  grids <- seq(0, TRUNC, by)
  K <- length(grids)
  ks <- c(0, floor(grids[-K] + (by + 1)/2), grids[K]+1)
  probs <- c(dpois(0, lambda), diff(ppois(grids, lambda)), 1 - ppois(grids[K], lambda))
  return(list(k = ks, prob = probs))
}

# planning function with early liquidation
planning <- function(demand = function(t, p) { return(0) })
  function(price_seq = NULL, price_liq = min(price_seq), inv_seq = 0:Q,
           Q = 0, T = 0, h = 0, h0 = 0, gamma = log(1/(1 - 0.15))/365,
           demand_asm = "STC", verbose = F, ...) {
    # demand: demand function of (time t, price p)
    # price_seq: a sequence of price decisions
    # price_liq: liquidation price
    # inv_seq: a sequence of next-day inventory decisions
    # Q: initial inv
    # T: maximal planning horizon
    # h: variable holding cost per unit inv per unit time
    # h0: fixed holding cost per unit time
    # gamma: discounting factor, defaulted to be annual 15%
    # demand_asm: assumption on demand, deterministic ("DET") or stochastic ("STC")
    # verbose: whether to print details
    # output: 
    ### value = value table at state (time t, inventory q)
    ### price_opt = optimal price table at state (time t, inventory q)
    ### inv_opt = optimal inventory at time t
    
    # initialize
    inv_opt <- rep(0, T+1); names(inv_opt) <- 0:T
    value <- price_opt <- matrix(0, nrow = T+1, ncol = Q+1, dimnames = list(0:T,0:Q))
    for(t in T:0) {
      if(verbose) { 
        print(paste0("Markdown + Liquidation Planning t = ", t))
      }
      # markdown sale
      if(t == T) {
        for(q in 1:Q) {
          # compute action value of every price
          payoff <- function(p, D) { exp(-gamma*T) * (p * pmin(D, q) + price_liq * pmax(0, q - D)) }
          if(demand_asm == "STC") { # stochastic demand
            value_price <- sapply(price_seq, function(p) {
              pois_exp_sample <- pois_exp(q, demand(T, p))
              sum(pois_exp_sample$prob * payoff(p, pois_exp_sample$k))
            })
          } else { # demand_asm == "DET", deterministic demand
            value_price <- sapply(price_seq, function(p) {
              payoff(p, demand(T, p))
            })
          }
          id_max <- which.max(value_price)
          price_opt[id.t(T), id.q(q)] <- price_seq[id_max]
          value[id.t(T), id.q(q)] <- value_price[id_max]
        }
        price_opt[, id.q(0)] <- price_opt[id.t(T), id.q(1)]
      } else { # t < T
        # optimal next-day inventory level
        ### compute action value of every next-day inventory
        value_inv <- sapply(inv_seq, function(q_next) {
          exp(-gamma*t) * (price_liq * (- q_next) - h * q_next - h0 * (q_next > 0)) + value[id.t(t+1), id.q(q_next)]
        })
        id_max <- which.max(value_inv)
        inv_opt[id.t(t)] <- q_opt <- inv_seq[id_max]
        
        # markdown sale
        for(q in 1:Q) {
          # compute action value of every price
          payoff <- function(p, D) { 
            sale <- pmin(q, D)
            q_after <- q - sale
            ifelse(q_after >= q_opt,
                   exp(-gamma*t) * (p * sale + price_liq * (q_after - q_opt) - h * q_opt - h0 * (q_opt > 0)) + value[id.t(t+1), id.q(q_opt)],
                   exp(-gamma*t) * (p * sale - h * q_after - h0 * (q_after > 0)) + value[id.t(t+1), id.q(q_after)])
          }
          if(demand_asm == "STC") { # stochastic demand
            value_price <- sapply(price_seq, function(p) {
              pois_exp_sample <- pois_exp(q, demand(t, p))
              sum(pois_exp_sample$prob * payoff(p, pois_exp_sample$k))
            })
          } else { # demand_asm == "DET", deterministic demand
            value_price <- sapply(price_seq, function(p) {
              payoff(p, demand(t, p))  # demand(t, p) is implicitly ceiled to an integer
            })
          }
          id_max <- which.max(value_price)
          price_opt[id.t(t), id.q(q)] <- price_seq[id_max]
          value[id.t(t), id.q(q)] <- value_price[id_max]
        }
      }
    }
    return(list(value = value, price_opt = price_opt, inv_opt = inv_opt,
                demand = demand, price_seq = price_seq, price_liq = price_liq, inv_seq = inv_seq,
                Q = Q, T = T, h = h, h0 = h0, gamma = gamma, demand_asm = demand_asm))
  }

inv_value <- function(planning_result, t = 0, q = planning_result$Q,
                      inv_seq = planning_result$inv_seq) {
  # planning_result: planning result from planning function
  # input: state (time t, inventory q)
  # outputs: 
  ### value_inv = next-day-inventory-value table of inv_seq
  ### regret_inv = value_inv(inv_opt) - value_inv
  if(t >= planning_result$T | q > planning_result$Q) {
    stop("time or inventory overflows!")  # no inventory planning at dat T
  } 
  h <- planning_result$h
  h0 <- planning_result$h0
  gamma <- planning_result$gamma
  price_liq <- planning_result$price_liq
  value <- planning_result$value
  
  # compute action-value of every next-day inventory from inv_seq
  value_inv <- sapply(inv_seq, function(q_next) {
    q_liq <- pmax(0, q - q_next)
    q_next <- pmin(q, q_next)
    exp(-gamma*t) * (price_liq * q_liq - h * q_next - h0 * (q_next > 0)) + value[id.t(t+1), id.q(q_next)]
  })
  id_max <- which.max(value_inv)
  inv_opt <- inv_seq[id_max]
  value_inv_opt <- value_inv[id_max]
  regret_inv <- value_inv_opt - value_inv
  names(value_inv) <- names(regret_inv) <- inv_seq
  return(list(
    value_inv = value_inv,
    regret_inv = regret_inv,
    value_inv_opt = value_inv_opt, inv_opt = inv_opt,
    planning_result = planning_result, t = t, inv_seq = inv_seq
  ))
}

price_value <- function(planning_result, t = 0, q = planning_result$Q, q_next = planning_result$inv_opt[id.t(t)],
                        price_seq = planning_result$price_seq) {
  # planning_result: planning result from planning function
  # inputs: state (time t, inventory q), next-day target inventory q_next
  # outputs: 
  ### value_price = price-value table of price_seq
  ### regret_price = value_price(price_opt) - value_price
  if(t > planning_result$T | q > planning_result$Q) {
    stop("time or inventory overflows!")
  }
  T <- planning_result$T
  h <- planning_result$h
  h0 <- planning_result$h0
  gamma <- planning_result$gamma
  price_liq <- planning_result$price_liq
  value <- planning_result$value
  demand <- planning_result$demand
  demand_asm = planning_result$demand_asm
  
  # compute action-value of every price from price_seq
  if(t == T) { # liquidate all remaining by the end
    payoff <- function(p, D) { exp(-gamma*T) * (p * pmin(D, q) + price_liq * pmax(0, q - D)) }
  } else {
    payoff <- function(p, D) { 
      sale <- pmin(q, D)
      q_after <- q - sale
      ifelse(q_after >= q_next,
             exp(-gamma*t) * (p * sale + price_liq * (q_after - q_next) - h * q_next - h0 * (q_next > 0)) + value[id.t(t+1), id.q(q_next)],
             exp(-gamma*t) * (p * sale - h * q_after - h0 * (q_after > 0)) + value[id.t(t+1), id.q(q_after)])
    }
  }
  if(demand_asm == "STC") { # stochastic demand
    value_price <- sapply(price_seq, function(p) {
      pois_exp_sample <- pois_exp(q, demand(t, p))
      sum(pois_exp_sample$prob * payoff(p, pois_exp_sample$k))
    })
  } else { # demand_asm == "DET", deterministic demand
    value_price <- sapply(price_seq, function(p) {
      payoff(p, demand(t, p))
    })
  }
  id_max <- which.max(value_price)
  price_opt <- price_seq[id_max]
  value_price_opt <- value_price[id_max]
  regret_price <- value_price_opt - value_price
  names(value_price) <- names(regret_price) <- price_seq
  
  return(list(
    value_price = value_price,
    regret_price = regret_price,
    value_price_opt = value_price_opt, price_opt = price_opt,
    planning_result = planning_result, t = t, q = q, price_seq = price_seq
  ))
}

# planning function without early liquidation
planning_mkd <- function(demand = function(t, p) { return(0) })
  function(price_seq = NULL, price_liq = min(price_seq),
           Q = 0, T = 0, h = 0, h0 = 0, gamma = log(1/(1 - 0.15))/365,
           demand_asm = "STC", verbose = F, ...) {
    # demand: demand function of (time t, price p)
    # price_seq: a sequence of prespecified implementable prices
    # price_liq: liquidation price
    # Q: initial inv
    # T: maximal planning horizon
    # h: variable holding cost per unit inv per unit time
    # h0: fixed holding cost per unit time
    # gamma: discounting factor, defaulted to be annual 15%
    # demand_asm: assumption on demand, deterministic ("DET") or stochastic ("STC")
    # verbose: whether to print details
    # output: 
    ### value = value table at state (time t, inventory q)
    ### price_opt = optimal price table at state (time t, inventory q)
    
    # initialize
    value <- price_opt <- matrix(0, nrow = T+1, ncol = Q+1, dimnames = list(0:T,0:Q))
    for(t in T:0) {
      if(verbose) { 
        print(paste0("Markdown Planning t = ", t))
      }
      # markdown sale
      if(t == T) {
        for(q in 1:Q) {
          # compute action value of every price
          payoff <- function(p, D) { exp(-gamma*T) * (p * pmin(D, q) + price_liq * pmax(0, q - D)) }
          if(demand_asm == "STC") { # stochastic demand
            value_price <- sapply(price_seq, function(p) {
              pois_exp_sample <- pois_exp(q, demand(T, p))
              sum(pois_exp_sample$prob * payoff(p, pois_exp_sample$k))
            })
          } else { # demand_asm == "DET", deterministic demand
            value_price <- sapply(price_seq, function(p) {
              payoff(p, demand(T, p))
            })
          }
          id_max <- which.max(value_price)
          price_opt[id.t(T), id.q(q)] <- price_seq[id_max]
          value[id.t(T), id.q(q)] <- value_price[id_max]
        }
        price_opt[, id.q(0)] <- price_opt[id.t(T), id.q(1)]
      } else { # t < T
        for(q in 1:Q) {
          # compute action value of every price
          payoff <- function(p, D) { exp(-gamma*t) * (p * pmin(D, q) - h * pmax(0, q - D) - h0 * (D < q)) + value[id.t(t+1), id.q(pmax(0, q - D))] }
          if(demand_asm == "STC") { # stochastic demand
            value_price <- sapply(price_seq, function(p) {
              pois_exp_sample <- pois_exp(q, demand(t, p))
              sum(pois_exp_sample$prob * payoff(p, pois_exp_sample$k))
            })
          } else { # demand_asm == "DET", deterministic demand
            value_price <- sapply(price_seq, function(p) {
              payoff(p, demand(t, p))
            })
          }
          id_max <- which.max(value_price)
          price_opt[id.t(t), id.q(q)] <- price_seq[id_max]
          value[id.t(t), id.q(q)] <- value_price[id_max]
        }
      }
    }
    return(list(value = value, price_opt = price_opt,
                demand = demand, price_seq = price_seq, price_liq = price_liq, 
                Q = Q, T = T, h = h, h0 = h0, gamma = gamma, demand_asm = demand_asm))
  }

predict.planning <- function(demand = function(t, p) { return(0) })
  function(planning_result, Q = planning_result$Q, T = planning_result$T, 
           demand_asm = "STC", n = ifelse(demand_asm == "STC", 1000, 1)) {
    # planning_result: planning result from planning function
    # Q: predicting initial inv
    # T: predicting maximal planning horizon
    # demand_asm: assumption on demand, deterministic ("DET") or stochastic ("STC")
    # n: number of sample if demand_asm = "STC"
    if(Q > planning_result$Q | T > planning_result$T) {
      stop("initial inventory or planning horizon overflows!")
    }
    t_end <- planning_result$T
    t_begin <- t_end - T
    h <- planning_result$h
    h0 <- planning_result$h0
    gamma <- planning_result$gamma
    price_liq <- planning_result$price_liq
    
    prices <- invs <- liqs <- sales <- matrix(0, nrow = n, ncol = T+1, dimnames = list(1:n, 0:T))
    values <- rep(0, n)
    clearance_times <- numeric()
    for(id_rep in 1:n) {
      invs[id_rep, id.t(0)] <- q <- Q
      for(t in t_begin:t_end) {
        # decisions
        prices[id_rep, id.t(t)] <- p <- planning_result$price_opt[id.t(t), id.q(q)]
        if(is.null(planning_result$inv_opt)) {
          q_opt <- Q
        } else {
          q_opt <- planning_result$inv_opt[id.t(t)]
        }
        
        # state transitions and payoffs
        D <- ifelse(
          demand_asm == "DET",
          demand(t, p),
          rpois(1, demand(t, p))  # demand_asm == "STC"
        )
        sales[id_rep, id.t(t)] <- sale <- pmin(D, q)
        if(t < t_end) {
          q_next <- q - sale
          if(q_next >= q_opt) {
            liqs[id_rep, id.t(t)] <- q_liq <- q_next - q_opt
            invs[id_rep, id.t(t+1)] <- q_next <- q_opt
          } else {
            liqs[id_rep, id.t(t)] <- q_liq <- 0
            invs[id_rep, id.t(t+1)] <- q_next
          }
          values[id_rep] <- values[id_rep] + exp(-gamma*t) * (p * sale + price_liq * q_liq - h * q_next - h0 * (q_next > 0))
          if((q <- q_next) == 0) { 
            clearance_times[id_rep] <- t
            prices[id_rep, id.t((t+1):T)] <- p
            break
          }
        } else { # t == t_end
          liqs[id_rep, id.t(t)] <- q_liq <- q - sale
          values[id_rep] <- values[id_rep] + exp(-gamma*t) * (p * sale + price_liq * q_liq)
          clearance_times[id_rep] <- t_end
        }
      }
    }
    # recovery rate
    recovery_rates <- rowSums(prices * sales + price_liq * liqs)/Q
    if(n > 1) {
      for(name in c("prices", "invs", "liqs", "sales")) {
        MEAN <- apply(get(name), 2, mean)
        SE <- apply(get(name), 2, rlang::as_function(~ sd(.)/sqrt(length(.))))
        assign(name, cbind(mean = MEAN, se = SE))
      }
      for(name in c("values", "clearance_times", "recovery_rates")) {
        MEAN <- mean(get(name))
        SE <- sd(get(name))/sqrt(length(get(name)))
        assign(name, c(mean = MEAN, se = SE))
      }
    } else { # n == 1
      for(name in c("prices", "invs", "liqs", "sales")) {
        assign(name, as.vector(get(name)))
      }
    }
    return(list(value = values, clearance_time = clearance_times, recovery_rate = recovery_rates,
                price = prices, inv = invs, inv_opt = planning_result$inv_opt, liq = liqs, sale = sales))
  }
