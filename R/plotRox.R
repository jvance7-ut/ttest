#' @title Plot Rttest
#'
#' @param funObject name used when function is run
#' @param i x vector from object index
#' @param j y vector from object index
#' @param k The test type (i.e. paired, t-test, or Welch)
#' @param l TRUE/FALSE value for paired
#' @param ... Additional arguments as needed
#'
#' @author Jennifer Vance
#'
#' @return box plot of x and y with test type as the title
#' @importFrom ggplot2 ggplot geom_boxplot labs xlab annotate
#' @importFrom reshape2 melt
#'
#' @export plot.Rttest
#'
#' @export
#'
#' @examples
#' \dontrun{plot(funObject)}
plot.Rttest = function(funObject, i=5, j=6, k = 3, l = 7, ...){
  #for the check function
  boxplot = NULL
  aes = NULL
  variable = NULL
  value = NULL
  reshape2 = NULL
  ggplot2 = NULL

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
                main = funObject[[k]], col = "blue"  )
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
                     main = funObject[[k]], col = "blue")
    text(x = 2, y = 10, labels = funObject$CI)

    #ggplot
    gplotT = ggplot(diff_long, aes(x=variable, y=value)) +
      xlab("Difference") +
      geom_boxplot(color = "green") +
      labs(title = funObject[[k]])+
      annotate(geom = "text", x=2, y= 10, label = funObject$CI)
  }


  #return the plots
  list(baseR = plotT, ggplot2 = gplotT)

}
