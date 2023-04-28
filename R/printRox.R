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
  kFO = kable(funObject$Data)
  kCI = kable(funObject$CI)
  z = NextMethod(c(kFO, kCI))
  class(z) = c("Rttest", class(z))
  list(df = kFO, ci = kCI)
}
