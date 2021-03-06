#' @title Terminator that stops after a number of evaluations
#'
#' @name mlr_terminators_evals
#' @include Terminator.R
#'
#' @description
#' Class to terminate the optimization depending on the number of evaluations.
#' An evaluation is defined by one resampling of a parameter value.
#'
#' @templateVar id evals
#' @template section_dictionary_terminator
#'
#' @section Parameters:
#' \describe{
#' \item{`n_evals`}{`integer(1)`\cr
#' Number of allowed evaluations, default is 100L.}
#' }
#'
#' @family Terminator
#' @template param_archive
#' @export
#' @examples
#' TerminatorEvals$new()
#' trm("evals", n_evals = 5)
TerminatorEvals = R6Class("TerminatorEvals",
  inherit = Terminator,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(ParamInt$new("n_evals", lower = 1L, default = 100L,
        tags = "required")))
      ps$values = list(n_evals = 100L)
      super$initialize(param_set = ps, properties = c("single-crit", "multi-crit"))
      self$unit = "evaluations"
    },

    #' @description
    #' Is `TRUE` iff the termination criterion is positive, and `FALSE`
    #' otherwise.
    #' @return `logical(1)`.
    is_terminated = function(archive) {
      assert_r6(archive, "Archive")
      archive$n_evals >= self$param_set$values$n_evals
    }
  ),

  private = list(
    .status = function(archive) {
      max_steps = self$param_set$values$n_evals
      current_steps =  archive$n_evals
      c("max_steps" = max_steps, "current_steps" = current_steps)
    }
  )
)

mlr_terminators$add("evals", TerminatorEvals)
