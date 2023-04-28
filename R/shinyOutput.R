#' @title DynamicShiny
#' @description
#' Use widgets to plot and run ttest on x and y.
#'
#' @return Plots CI
#'
#' @import shiny
#'
#' @section shiny: The shiny web server will be used.
#'
#' @export
#'
#' @examples
#' \dontrun{shinyOutput()}
shinyOutput<-function(){
  shiny::runApp(system.file("shiny1/example", package = "ttest"),
                launch.browser = TRUE)
}
