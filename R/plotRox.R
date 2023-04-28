#' @title Plot Rttest
#' @importFrom ggplot2 ggplot geom_boxplot labs xlab
#' @importFrom reshape2 melt
#'
#' @param funObject name used when function is run
#' @param i x vector from object index
#' @param j y vector from object index
#' @param k The test type (i.e. paired, t-test, or Welch)
#' @param l TRUE/FALSE value for paired
#'
#' @return box plot of x and y with test type as the title
#'
#' @export plot.Rttest
#'
#' @export
#'
#' @examples
#' \dontrun{plot(funObject)}
plot.Rttest = function(funObject, i=5, j=6, k = 3, l = 7){

  boxplot = NULL
  aes = NULL
  variable = NULL
  value = NULL

  library(reshape2)

  library(ggplot2)
  #set margins to be safe
  #par(mar = c(1, 1, 1, 1))
  Y = funObject[[j]]
  X = funObject[[i]]

  #data manipulation for ggplot
  df = data.frame(x = X, y = Y)
  df_long = melt(df)

  if(funObject[[l]]==FALSE){
  #base R plot
    plotT <- boxplot(X, Y, names= c("x", "y"),
                main = funObject[[k]], col = "blue",
                pch=19 )
  #ggplot
    gplotT = ggplot(df_long, aes(x=variable, y=value)) +
      geom_boxplot(color = "green") +
      labs(title = funObject[[k]])
  }
  else{
    diff = X-Y
    diff2 = data.frame(diff = diff)
    diff_long = melt(diff2)
    #base R plot
    plotT <- boxplot(diff,
                     main = funObject[[k]], col = "blue",
                     pch=19 )
    #ggplot
    gplotT = ggplot(diff_long, aes(x=variable, y=value)) +
      xlab("Difference") +
      geom_boxplot(color = "green") +
      labs(title = funObject[[k]])
  }


  #return the plots
  list(baseR = plotT, ggplot2 = gplotT)

}
