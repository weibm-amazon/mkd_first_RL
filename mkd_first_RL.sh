input1=${1:-1}
input2=${2:-1}

# implementation inputs
prob0=0  # base randomization probability for prices
delta0=0.1  # base distance of price
regret_factor=2  # screening eligible randomized prices
regret_prop=0.3  # screening eligible randomized prices
alpha=0.1  # confidence level for regret
# prior specification of Logit demand model parameters (log_demand_base, beta, time_trend)
PRIOR="norm"  # type of prior, "unif" or "norm"
### normal prior
log_demand_base_mean=0
log_demand_base_sd=1
beta_mean=1
beta_sd=1
### uniform prior
log_demand_base_low=-3
log_demand_base_upp=5
beta_low=0
beta_upp=5
### other setups
n_core=1  # number of cores
# n_core="parallel::detectCores()"  # number of cores

counter=1
for beta in `seq 0 0.5 3`
do
  if [ $counter == $input1 ]
  then
    break
  fi
  ((counter++))
done
log_demand_base_orcl=$beta
beta_orcl=$beta

DIR0_export="RData/mkd_first_RL"
DIR1_export="${DIR0_export}/log_demand_base=${log_demand_base_orcl}_beta=${beta_orcl}"
[ ! -d $DIR0_export ] && mkdir $DIR0_export
[ ! -d $DIR1_export ] && mkdir $DIR1_export

FILE_export="${DIR1_export}/mkd_first_RL_${input2}.RData"  # saving result to FILE_export
R CMD BATCH --no-save --no-restore "--args 
  FILE_header='header.R'
  FILE_planning='planning_funs.R'
  PLANNING_FUN='planning'
  DIR_planning_results='planning_results'
  FILE_id_results='id_results.csv'
  FILE_lock='planning.lock' 
  FILE_export='${FILE_export}'
  prob0=$prob0
  delta=$delta
  regret_factor=${regret_factor}
  regret_prop=${regret_prop}
  alpha=$alpha
  PRIOR='$PRIOR'
  log_demand_base_mean=${log_demand_base_mean}
  log_demand_base_sd=${log_demand_base_sd}
  beta_mean=${beta_mean}
  beta_sd=${beta_sd}
  log_demand_base_low=${log_demand_base_low}
  log_demand_base_upp=${log_demand_base_upp}
  beta_low=${beta_low}
  beta_upp=${beta_upp}
  log_demand_base_orcl=${log_demand_base_orcl}
  beta_orcl=${beta_orcl} 
  n_core=${n_core}" mkd_first_RL.R Rout/mkd_first_RL_${input1}_${input2}.Rout
