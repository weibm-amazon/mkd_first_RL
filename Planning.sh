input=${1:-1}

FILE_param="candid_next.csv"  # param to plan for
# other setups
n_core=1  # number of cores
# n_core="parallel::detectCores()"  # number of cores

counter=0
FLAG=0
while IFS=, read log_demand_base beta time_trend
do
  if [ $counter == $input ]
  then
    FLAG=1
    break
  fi
  ((counter++))
done < $FILE_param
if [ $FLAG == 0 ]
then
  echo "${FILE_param} EOF!"
  exit 1
fi

R CMD BATCH --no-save --no-restore "--args 
  FILE_header='header.R' 
  FILE_planning='planning_funs.R'
  PLANNING_FUN='planning'
  DIR_planning_results='planning_results' 
  FILE_id_results='id_results.csv' 
  FILE_lock='planning.lock' 
  log_demand_base=${log_demand_base}
  beta=${beta}
  time_trend=${time_trend}
  n_core=${n_core}" Planning.R Rout/Planning_${input}.Rout
