#' Plot Rttest
#' @importFrom ggplot2 ggplot geom_point labs xlab ylab
#'
#' @param funObject
#' @param i x vector from object index
#' @param j y vector from object index
#' @param k The test type (i.e. paired, t-test, or Welch)
#'
#' @return dot plot of x and y with test type as the title

#' @export plot.Rttest
#'
#' @export
#'
#' @examples
#' \dontrun{plot(funObject)}
plot.Rttest = function(funObject, i=5, j=6, k = 3){
  require(ggplot2)
  par(mar = c(1, 1, 1, 1))
  Y = funObject[[j]]
  X = funObject[[i]]
  df = data.frame(x = X, y = Y)
  plotT <- plot(Y~X,
                main = funObject[[k]], col = "blue",
                pch=19 )
  gplotT = ggplot(df, aes(x=X,y=Y)) +
    geom_point(color = "green") +
    labs(title = funObject[[k]]) +
    xlab("X") +
    ylab("Y")
  list(baseR = plotT, ggplot2 = gplotT)

}
