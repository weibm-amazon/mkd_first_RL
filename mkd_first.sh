input1=${1:-1}
input2=${2:-1}

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

DIR0_export="RData/mkd_first"
DIR1_export="${DIR0_export}/log_demand_base=${log_demand_base_orcl}_beta=${beta_orcl}"
[ ! -d $DIR0_export ] && mkdir $DIR0_export
[ ! -d $DIR1_export ] && mkdir $DIR1_export

FILE_export="${DIR1_export}/mkd_first_${input2}.RData"  # saving result to FILE_export
R CMD BATCH --no-save --no-restore "--args 
  FILE_header='header.R'
  FILE_planning='planning_funs.R'
  PLANNING_FUN='planning_mkd'
  DIR_planning_results='planning_mkd_results'
  FILE_id_results='id_mkd_results.csv'
  FILE_lock='planning_mkd.lock' 
  FILE_export='${FILE_export}'
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
  n_core=${n_core}" mkd_first.R Rout/mkd_first_${input1}_${input2}.Rout
