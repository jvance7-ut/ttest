#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if (interactive()){
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ttest"),

    # Sidebar with a slider input for the alpha level
    sidebarLayout(
        sidebarPanel(
            sliderInput("alpha",
                        "alpha Level:",
                        min = 0.001,
                        max = 0.500,
                        value = 0.05),

            checkboxInput("Independent",
                          "Independent Sample:",
                          value = TRUE),


            checkboxInput("eqVar",
                          "Equal Variance:",
                          value = TRUE)
        ),


        #sidebarPanel(
        #    checkboxInput("Independent",
        #                "Independent Sample:",
        #                value = "TRUE")
        #),

        #sidebarPanel(
        #  checkboxInput("eqVar",
        #              "Equal Variance:",
        #              value = "TRUE")
        #),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("ci"),
           verbatimTextOutput("data")
        )
    )
)

# Define server logic required to draw a plot, and print out df and CI
server <- function(X,Y, output) {

    output$distPlot <- renderPlot({
        # generate graph based on variables
        x    <- X
        y    <- Y
        Independent <- c(TRUE, FALSE)
        eqVar <- c(TRUE, FALSE)


        # draw the plot with the specified input
        plot(y~x,
            main = ifelse(Independent == FALSE, "Paired t-test",
                          ifelse(eqVar == TRUE, "Independent, Equal Var t-test",
                                "Independent, Unequal var t-test")), col = "blue",
                      pch=19 )
    })




    #Confidence Interval
    output$ci <- renderText({

      x    <- input$x
      y    <- input$y
      alpha <- seq(0.000, 0.500, by = 0.05)
      Independent <- c(TRUE, FALSE)
      eqVar <- c(TRUE, FALSE)

    if(Independent==TRUE && eqVar == TRUE){
      #run the t-test with equal var
      ttest = t.test(x, y, var.equal = TRUE)
    }
    else if(Independent==TRUE && eqVar==FALSE){
      #run the t-test with unequal var
      ttest = t.test(x, y, var.equal = FALSE)
    }
    else{
      ttest = t.test(x, y, paired = TRUE)
    }
    ttest$conf.int

    })
    #output$ci <- ttest$conf.int


    #Data
    output$data<- renderDataTable({

      x    <- input$x
      y    <- input$y
      alpha <- seq(0.000, 0.500, by = 0.05)
      Independent <- c(TRUE, FALSE)
      eqVar <- c(TRUE, FALSE)

    if(length(x)==length(y)){
      df = data.frame(x = x, y = y)
    }

    else if(length(x)<length(y)){
      xtemp = rep(NA, length(y))
      for (i in 1:length(x)) {
        xtemp[i] = x[i]
      }
      df = data.frame(x = xtemp, y = y)
    }

    else{
      ytemp = rep(NA, length(x))
      for (i in 1:length(y)) {
        ytemp[i] = y[i]
      }
      df = data.frame(x = x, y = ytemp)
    }
      df
    })
    #output$data <- df


}

# Run the application
shinyApp(ui = ui, server = server)

}
