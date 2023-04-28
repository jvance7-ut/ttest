#' @title Dynamic output using shiny
#'
#'
#'@desccription Widgets, plot and run ttest on x and y
#'
#' @return Plots dataframe CI
#'
#' @import shiny
#'
#' @section shiny: The shiny web server will be used.
#'
#' @export
#'
#' @examples
#' \dontrun{shinyOutput(funObject$x, funObject$y, ...)}
shinyOutput<-function(){
  shiny::runApp(system.file("shiny1/example", package = "ttest"),
                launch.browser = TRUE)
}
