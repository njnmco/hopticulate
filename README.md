
# hopticulate

<!-- badges: start -->
<!-- badges: end -->

The goal of hopticulate is to wrap the most excellent [hyperopt python package](https://github.com/hyperopt/hyperopt)
so that it can be easily used from R.

## Installation

<!--
You can install the released version of hopticulate from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hopticulate")
```
-->


You can install the development version of hopticulate from GitHub with:

``` r
remotes::install_github("njnmco/hopticulate")
```

### Python package installation

You will also need to install the python package, at least until CRAN approves some way of doing so automatically:

```r
reticulate::py_install("hyperopt")
```


## Example

Please see [the getting started demo](demo/getting-started.R)
