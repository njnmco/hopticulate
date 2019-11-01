

delayedAssign("hyperopt", reticulate::import("hyperopt", delay_load = TRUE))


for(i in c('hp', 'fmin', 'tpe'))
  delayedAssign(i, hyperopt[[i]], eval.env = list2env(list(i=i)))


rm(i)


Ops.hyperopt.pyll.base.Apply <- function(e1, e2) {
  left  <- inherits(e1, "hyperopt.pyll.base.Apply")
  right <- inherits(e2, "hyperopt.pyll.base.Apply")

  if(!left && ! right){
    stop("This should be impossible")
  }
  else if(left) {
    m <- switch(
      .Generic,
      "+"=as.name("__add__"),
      "-"=as.name("__sub__"),
      "*"=as.name("__mul__"),
      "/"=as.name("__div__"),
      stop("Unsupported operator for hyperopt:", .Generic))

    x <- e1
    y <- e2
  }
  else if (right) {
    m <- switch(
      .Generic,
      "+"=as.name("__radd__"),
      "-"=as.name("__rsub__"),
      "*"=as.name("__rmul__"),
      "/"=as.name("__rdiv__"),
      stop("Unsupported operator for hyperopt:", .Generic))
    x <- e2
    y <- e1
  }

  eval(substitute(x$m(y)))
}
