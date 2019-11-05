#' Hyperopt via reticulate
#'
#' `hyperopt` is a popular python package for automated hyperparameter
#' tuning.
#'
#' This package, `hopticulate`, is a minimal wrapper for using hyperopt
#' through the most excellent reticulate package.
#'
#' @name hopticulate
#' @docType package
#' @aliases hyperopt fmin space_eval
#'
#' @examples
#' \dontshow{ if(!is.null(fmin))   }
#'   fmin(function(x) (x - 1)**2, hp.normal("mu", 0, 1), algo=tpe.suggest, max_evals=10)
#'
#' @references \url{https://github.com/hyperopt/hyperopt}
NULL

### Main package
delayedAssign("hyperopt", tryCatch(reticulate::import("hyperopt"),
                                   error=function(x) {
                                     message("Could not import hyperopt.\n")
                                     NULL
                                   }))



delayedAssign("fmin", hyperopt[["fmin"]])
delayedAssign("space_eval", hyperopt[["space_eval"]])


#' Search Space Definitions
#'
#'
#'
#' @name search-space
#' @aliases hp hp.choice hp.pchoice hp.randint hp.uniformint
#' @aliases hp.normal hp.lognormal hp.qnormal hp.qlognormal
#' @aliases hp.uniform hp.loguniform hp.quniform hp.qloguniform
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
