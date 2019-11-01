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
#' @aliases hyperopt
#'
#'
NULL


delayedAssign("hyperopt", reticulate::import("hyperopt", delay_load = TRUE))


for(i in c('hp', 'fmin', 'tpe'))
  delayedAssign(i, hyperopt[[i]], eval.env = list2env(list(i=i)))

rm(i)


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
