# mkd_first_RL
header.R defines inputs in default and will be overriden if input from command line
  mkd_first_RL: function for an experiment of markdown first RL
  mkd_first: function for an experiment of a prototye of existing markdown first
  mkd_sale: function for an experiment of a prototye of existing markdown sale
planning_funs.R incorporates functions for planning
  planning: markdown + intermediate liquidations
  planning_mkd: markdown + terminal liquidation
  price_value: compute price value, regret functions from a planning_result
  regret_value: compute next-day-inventory value, regret functions
mkd_first_RL.R implement an experiment of markdown first RL
mkd_first.R implemnt an experiment of a prototye of existing markdown first
mkd_first_RL.sh: call mkd_first_RL.R from command line with desire inputs
mkd_first.sh: call mkd_first.R from command line with desire inputs
