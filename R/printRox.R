#' @title Print method for class Rttest
#'
#' @param funObject object made when myttest() is run
#' @param ... Additional arguments as needed
#' @author Jennifer Vance
#'
#' @return a table with the data set, the confidence interval, and hypothesis choice
#' @importFrom kableExtra kable kable_styling
#' @export print.Rttest
#'
#' @export
#'
#' @examples
#' \dontrun{print(funObject)}
print.Rttest = function(funObject, ...){
  #For the check function
  kableExtra = NULL

  library(kableExtra)

  #Call data and CI through kable
  #kFO = kable(c(funObject$Data, funObject$CI), booktabs = TRUE,
  #            col.names = c("x", "y", "low", "high"))
  kFO = kable(funObject$Data, booktabs = TRUE, col.names = c("x", "y"))
  ciprint = funObject$CI

  list(kFO, CI = ciprint)
}
