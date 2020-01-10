#' Hyperopt via reticulate
#'
#' \code{hyperopt} is a popular python package for automated hyperparameter
#' tuning.
#'
#' This package, \code{hopticulate}, is a minimal wrapper for using \code{hyperopt}
#' through the most excellent \code{reticulate} package.
#'
#' @name hopticulate
#' @docType package
#' @aliases hyperopt fmin space_eval Trials pyll.stochastic.sample
#'
#' @examples
#' \dontshow{ if(!is.null(fmin))   }
#'   fmin(function(x) (x - 1)**2, hp.normal("mu", 0, 1), algo=tpe.suggest, max_evals=10)
#'
#' @references \url{https://github.com/hyperopt/hyperopt}
NULL

### Main package
delayedAssign("hyperopt", tryCatch(reticulate::import("hyperopt"),
                                   error=function(x) NULL))



delayedAssign("fmin", hyperopt[["fmin"]])
delayedAssign("space_eval", hyperopt[["space_eval"]])
delayedAssign("Trials", hyperopt[["Trials"]])
delayedAssign("pyll.stochastic.sample", hyperopt[["pyll"]][["stochastic"]][["sample"]])

#' Search Space Definitions
#'
#' Declare a search space using the following:
#'
#' \itemize{
#'
#'   \item Discrete choices
#'   \itemize{
#'     \item \code{hp.choice}
#'     \item \code{hp.pchoice}
#'   }
#'
#'   \item Integers
#'   \itemize{
#'     \item \code{hp.randint}
#'     \item \code{hp.uniformint}
#'   }
#'
#'   \item Normal
#'   \itemize{
#'     \item \code{hp.normal}
#'     \item \code{hp.lognormal}
#'     \item \code{hp.qnormal}
#'     \item \code{hp.qlognormal}
#'   }
#'
#'   \item Uniform
#'   \itemize{
#'     \item \code{hp.uniform}
#'     \item \code{hp.loguniform}
#'     \item \code{hp.quniform}
#'     \item \code{hp.qloguniform}
#'   }
#'
#' }
#'
#' @name search-space
#' @aliases hp hp.choice hp.pchoice hp.randint hp.uniformint hp.normal hp.lognormal hp.qnormal hp.qlognormal hp.uniform hp.loguniform hp.quniform hp.qloguniform
delayedAssign("hp", hyperopt[["hp"]]) # Not actually exported, make below nicer

delayedAssign("hp.choice", hp[["choice"]])
delayedAssign("hp.pchoice", hp[["pchoice"]])

delayedAssign("hp.randint", hp[["randint"]])
delayedAssign("hp.uniformint", hp[["uniformint"]])


delayedAssign("hp.normal", hp[["normal"]])
delayedAssign("hp.lognormal", hp[["lognormal"]])
delayedAssign("hp.qnormal", hp[["qnormal"]])
delayedAssign("hp.qlognormal", hp[["qlognormal"]])

delayedAssign("hp.uniform", hp[["uniform"]])
delayedAssign("hp.loguniform", hp[["loguniform"]])
delayedAssign("hp.quniform", hp[["quniform"]])
delayedAssign("hp.qloguniform", hp[["qloguniform"]])




#' Search strategies
#'
#' The following search strategies are available:
#'
#' \describe{
#' \item{ \code{tpe.suggest}}{ Tree of Parzen Estimator}
#' \item{ \code{rand.suggest}}{Random Search}
#' \item{ \code{anneal.suggest}}{Simulated Annealing}
#' \item{ \code{mix.suggest}}{Mixed strategy (given a sequence of probability / pure-strategy pairs in \code{...}). }
#' }
#'
#' @name search-strategies
#' @aliases tpe.suggest rand.suggest anneal.suggest mix.suggest
delayedAssign("tpe.suggest", hyperopt[["tpe"]][["suggest"]])
delayedAssign("rand.suggest", hyperopt[["rand"]][["suggest"]])
delayedAssign("anneal.suggest", hyperopt[["anneal"]][["suggest"]])


mix.suggest <- function(...) {
  p_suggest <- list(...)
  p_suggest <- mapply(reticulate::tuple,
                      p_suggest[seq(1, length(p_suggest), 2)],
                      p_suggest[seq(2, length(p_suggest), 2)])
  function(...) hyperopt$mix$suggest(..., p_suggest = p_suggest)
}


Ops.hyperopt.pyll.base.Apply <- function(e1, e2) {
  left  <- inherits(e1, "hyperopt.pyll.base.Apply")
  right <- inherits(e2, "hyperopt.pyll.base.Apply")

  if(!left && !right){
    stop("This should be impossible")
  }
  else if(left) {
    m <- switch(
      .Generic,
      "+"="__add__",
      "-"="__sub__",
      "*"="__mul__",
      "/"="__div__",
      stop("Unsupported operator for hyperopt:", .Generic))

    x <- e1
    y <- e2
  }
  else if (right) {
    m <- switch(
      .Generic,
      "+"="__radd__",
      "-"="__rsub__",
      "*"="__rmul__",
      "/"="__rdiv__",
      stop("Unsupported operator for hyperopt:", .Generic))
    x <- e2
    y <- e1
  }

  m <- as.name(m)

  eval(substitute(x$m(y)))
}
