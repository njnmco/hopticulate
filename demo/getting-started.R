#' ## Getting Started
#'
#' This is a literal transcription of the getting started example from
#' http://hyperopt.github.io/hyperopt/
#'

# Load the package:
library(hopticulate)

# define an objective function
objective <- function(args){
  case <- args[[1]]; val <- args[[2]]
  if (case == 'case 1')
    return(val)
  else
    return(val ** 2)
}

# Define a search space.
space = hp.choice('a',
                    list(
                      list('case 1', 1 + hp.lognormal('c1', 0, 1)),
                      list('case 2', hp.uniform('c2', -10, 10))
                    )
                  )

best = fmin(objective, space, algo=tpe.suggest, max_evals=100)

print(best)
# -> {'a': 1, 'c2': 0.01420615366247227}
print(space_eval(space, best))
