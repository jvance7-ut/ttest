#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

set.seed(32)
x=rnorm(30,mean=10,sd=15)
set.seed(35)
y=rnorm(30,mean=8,sd=15)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ttest"),

    # Sidebar with a slider input for the alpha level
    sidebarLayout(
        sidebarPanel(
            sliderInput("alpha",
                        "alpha Level:",
                        min = 0.000,
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
           textOutput("ci")
        )
    )
)

# Define server logic required to draw a plot and print out the CI
server <- function(input, output) {

    output$distPlot <- renderPlot({
        #use provided data
        x    <- x
        y    <- y

        # generate graph based on variables
        Independent <- c(TRUE, FALSE)
        eqVar <- c(TRUE, FALSE)


        # draw the plot with the specified input
        if(input$Independent==FALSE){
          difference = x-y
          boxplot(difference, main = "Paired Sample", col = "blue")
        }
        else{
        boxplot(x,y,
            main = ifelse(input$eqVar == TRUE, "Independent, Equal Var t-test",
                          "Independent, Unequal var t-test"), col = "blue")
        }
    })




    #Confidence Interval
    output$ci <- renderText({
      #use provided data
      x    <- x
      y    <- y

      # generate CI based on variables
      alpha <- seq(0.000, 0.500, by = 0.05)
      Independent <- c(TRUE, FALSE)
      eqVar <- c(TRUE, FALSE)

    if(input$Independent==TRUE && input$eqVar == TRUE){
      #run the t-test with equal var
      ttest = t.test(x, y, var.equal = TRUE, conf.level = 1-input$alpha)
    }
    else if(input$Independent==TRUE && input$eqVar==FALSE){
      #run the t-test with unequal var
      ttest = t.test(x, y, var.equal = FALSE, conf.level = 1-input$alpha)
    }
    else{
      #run the t-test as paired
      ttest = t.test(x, y, paired = TRUE, conf.level = 1-input$alpha)
    }
    ttest$conf.int

    })


}

# Run the application
shinyApp(ui = ui, server = server)

