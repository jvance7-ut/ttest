#' Print method for class Rttest
#'
#' @param funObject
#' @author Jennifer Vance
#'
#' @return a table with the data set
#' @importFrom kableExtra kable
#' @export print.Rttest
#'
#' @export
#'
#' @examples
#' \dontrun{print(funObject)}
print.Rttest = function(funObject, ...){
  require(kableExtra)
  #kFO = kable(c(funObject$Data, funObject$CI))
  kFO = kable(funObject$Data)
  kCI = kable(funObject$CI)
  z = NextMethod(kFO)
  class(z) = c("Rttest", class(z))
  list("data frame" = kFO, "interval" = kCI)
}
