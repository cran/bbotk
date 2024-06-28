## ----setup, include = FALSE---------------------------------------------------
set.seed(1)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(bbotk)
opts()

## -----------------------------------------------------------------------------
fun = function(xs) {
  c(y = - (xs[[1]] - 2)^2 - (xs[[2]] + 3)^2 + 10)
}

## -----------------------------------------------------------------------------
library(paradox)

domain = ps(
  x1 = p_dbl(-10, 10),
  x2 = p_dbl(-5, 5)
)
codomain = ps(
  y = p_dbl(tags = "maximize")
)
obfun = ObjectiveRFun$new(
  fun = fun,
  domain = domain,
  codomain = codomain,
  properties = "deterministic" # i.e. the result always returns the same result for the same input.
)

## -----------------------------------------------------------------------------
trms()

## -----------------------------------------------------------------------------
terminators = list(
  evals = trm("evals", n_evals = 20),
  run_time = trm("run_time")
)
terminators

## -----------------------------------------------------------------------------
terminators$run_time$param_set$values$secs = 10

## -----------------------------------------------------------------------------
term_combo = TerminatorCombo$new(terminators = terminators)

## -----------------------------------------------------------------------------
instance = OptimInstanceBatchSingleCrit$new(objective = obfun, terminator = term_combo)
instance

## -----------------------------------------------------------------------------
optimizer = opt("gensa")
optimizer

## -----------------------------------------------------------------------------
optimizer$optimize(instance)

## -----------------------------------------------------------------------------
# result as a data.table
instance$result
# result as a list that can be passed to the Objective
instance$result_x_domain
# result outcome
instance$result_y

## -----------------------------------------------------------------------------
as.data.table(instance$archive)

## -----------------------------------------------------------------------------
fun_volume = function(xs) {
  c(y = - (xs$h * xs$w * xs$d))
}
domain = ps(
  h = p_dbl(lower = 0),
  w = p_dbl(lower = 0),
  d = p_dbl(lower = 0)
)
obj = ObjectiveRFun$new(
  fun = fun_volume,
  domain = domain
)

## -----------------------------------------------------------------------------
search_space = ps(
  h = p_dbl(lower = 0, upper = 1),
  w = p_dbl(lower = 0, upper = 1),
  .extra_trafo = function(x, param_set){
    x = unlist(x)
    x["d"] = 2 - sum(x) # model d in dependence of h, w
    x = x/sum(x) # ensure that h+w+d = 1
    as.list(x)
  }
)

## -----------------------------------------------------------------------------
inst = OptimInstanceBatchSingleCrit$new(
  objective = obj,
  search_space = search_space,
  terminator = trm("evals", n_evals = 30)
)
optimizer = opt("gensa")
lg = lgr::get_logger("bbotk")$set_threshold("warn") # turn off console output
optimizer$optimize(inst)
lg = lgr::get_logger("bbotk")$set_threshold("info") # turn on console output

## -----------------------------------------------------------------------------
inst$result_x_search_space

## -----------------------------------------------------------------------------
inst$result_x_domain
obj$eval(inst$result_x_domain)

## -----------------------------------------------------------------------------
library(data.table)

inst$terminator = trm("none")
xvals = data.table(h = c(0.6666, 0.6667), w = c(0.6666, 0.6667))
inst$eval_batch(xdt = xvals)
tail(as.data.table(instance$archive))

## -----------------------------------------------------------------------------
inst$archive$best()

## ----eval = FALSE, include = FALSE--------------------------------------------
#  # TODO: Write the following sections:
#  
#  ## Implementing your own Objective
#  
#  ### Storing extra output in the archive
#  
#  ## Implementing your own Optimizer
#  
#  ### Storing extra tuner information in the archive
#  
#  ## Implement your own Terminator
#  
#  # TODO: Fix intro after more sections are added

