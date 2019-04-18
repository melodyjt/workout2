#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Three Modes of Investing"),
   
   # Sidebar with a slider input for number of bins 
   flowLayout(
         column(12, 
                sliderInput("init",
                     "Initial Amount",
                     min = 1,
                     max = 100000,
                     step = 500,
                     value = 1000), 
                sliderInput("annual",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     step = 500,
                     value = 2000)),
         column(12, 
                sliderInput("rate",
                     "Return Rate (in %)",
                     min = 0,
                     max = 20, 
                     step = 0.1,
                     value = 5), 
                sliderInput("growth",
                     "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     step = 0.1,
                     value = 2)), 
         column(12, 
                sliderInput("years",
                     "Years",
                     min = 0,
                     max = 50,
                     step = 1,
                     value = 20),
                selectInput("facet",
                     "Facet?",
                     c("No", "Yes")))
      ),
      
        mainPanel(
          h4("Timelines"),
          plotOutput("timeline"),
          h4("Balances"),
          verbatimTextOutput("balance")
    )
)

# Define server logic required to draw a plot
server <- function(input, output) {
   
   output$timeline <- renderPlot({
     #' @title Future Value Function
     #' @description compute the future value
     #' @param amount initial invested amount (numeric)
     #' @param rate annual rate of return (numeric)
     #' @param years number of years (numeric)
     #' @return numeric future value
     
     future_value <- function(amount, rate, years) {
       fv <- amount * (1 + rate) ^ years
       return(fv)
     }
     
     #' @title Future Value of Annuity
     #' @description compute the future value of annuity
     #' @param contrib contributed amount (numeric)
     #' @param rate annual rate of return (numeric)
     #' @param years number of years (numeric)
     #' @return numeric future value of annuity
     
     annuity <- function( contrib, rate, years) {
       an <- contrib * ((1 + rate) ^ years - 1) / rate
       return(an)
     }
     
     #' @title Future Value of Growing Annuity
     #' @description compute the future value of growing annuity
     #' @param contrib contributed amount (numeric)
     #' @param rate annual rate of return (numeric)
     #' @param growth annual growth rate (numeric)
     #' @param years number of years (numeric)
     #' @return numeric future value of growing annuity
     
     growing_annuity <- function(contrib, rate, growth, years) {
       ga <- contrib * ((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth)
       return(ga)
     }
     
     no_contrib <- c()
     fixed_contrib <- c()
     growing_contrib <- c()
     for (i in 0:input$years) {
       no_contrib <- c(no_contrib, future_value(amount = input$init, rate = input$rate / 100, years = i))
       fixed_contrib <- c(fixed_contrib, (future_value(amount = input$init, rate = input$rate / 100, years = i) + annuity(contrib = input$annual, rate = input$rate / 100, years = i)))
       growing_contrib <- c(growing_contrib, (future_value(amount = input$init, rate = input$rate / 100, years = i) + growing_annuity(contrib = input$annual, rate = input$rate / 100, growth = input$growth / 100, years = i)))
     }
     
     year <- c(0:input$years)
     modalities <- data.frame(year = year, no_contrib = no_contrib, fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
     
     timeline <- ggplot(modalities, aes(x = year, colour = key)) + 
       geom_line(aes(y = no_contrib, colour = "no contrib"), show.legend = TRUE) +
       geom_point(aes(y = no_contrib, colour = "no contrib")) +
       geom_line(aes(y = fixed_contrib, colour = "fixed contrib"), show.legend = TRUE) +
       geom_point(aes(y = fixed_contrib, colour = "fixed contrib")) +
       geom_line(aes(y = growing_contrib, colour = "growing contrib"), show.legend = TRUE) + 
       geom_point(aes(y = growing_contrib, colour = "growing contrib")) +
       labs(y = "Growth ($)", x = "Year", title = "Three Modes of investing") +
       scale_colour_manual(values = c("no contrib" = "red", "fixed contrib"= "green", "growing contrib" = "blue"), name = "modality")
     
     modals <- c(rep("no_contrib", input$years + 1), rep("fixed_contrib", input$years + 1), rep("growing_contrib", input$years + 1))
     balances <- c(no_contrib, fixed_contrib, growing_contrib)
     y <- c(rep(0:input$years, 3))
     modalitiesfacet <- data.frame(year = y,
                                   modalities = modals,
                                   balances = balances)
     
     modalitiesfacet$modalities <- factor(modalitiesfacet$modalities, c("no_contrib", 
                                                                        "fixed_contrib",
                                                                        "growing_contrib"))
     timelinefacet <- ggplot(modalitiesfacet, aes(x = y, 
                                                  y = balances)) + 
       geom_line(aes(color = modalities)) + 
       geom_point(aes(color = modalities)) +
       geom_area(aes(fill = modalities, alpha = 1)) + 
       scale_alpha(guide = "none") +
       facet_grid(~ modalities) + 
       labs(y = "Growth ($)", x = "Year", title = "Three Modes of investing")
     
     data <- switch(input$facet,
                    "No" = timeline,
                    "Yes" = timelinefacet)
     data
   })
   
   output$balance <- renderPrint({
     future_value <- function(amount, rate, years) {
       fv <- amount * (1 + rate) ^ years
       return(fv)
     }
     
     annuity <- function( contrib, rate, years) {
       an <- contrib * ((1 + rate) ^ years - 1) / rate
       return(an)
     }
    
     growing_annuity <- function(contrib, rate, growth, years) {
       ga <- contrib * ((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth)
       return(ga)
     }
     
     no_contrib <- c()
     fixed_contrib <- c()
     growing_contrib <- c()
     for (i in 0:input$years) {
       no_contrib <- c(no_contrib, future_value(amount = input$init, rate = input$rate / 100, years = i))
       fixed_contrib <- c(fixed_contrib, (future_value(amount = input$init, rate = input$rate / 100, years = i) + annuity(contrib = input$annual, rate = input$rate / 100, years = i)))
       growing_contrib <- c(growing_contrib, (future_value(amount = input$init, rate = input$rate / 100, years = i) + growing_annuity(contrib = input$annual, rate = input$rate / 100, growth = input$growth / 100, years = i)))
     }
     
     year <- c(0:input$years)
     modalities <- data.frame(year = year, no_contrib = no_contrib, fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
     
     modals <- c(rep("no_contrib", input$years + 1), rep("fixed_contrib", input$years + 1), rep("growing_contrib", input$years + 1))
     balances <- c(no_contrib, fixed_contrib, growing_contrib)
     y <- c(rep(0:input$years, 3))
     modalitiesfacet <- data.frame(year = y,
                                   modalities = modals,
                                   balances = balances)
     
     tab <- switch(input$facet,
                   "No" = modalities,
                   "Yes" = modalitiesfacet)
     tab
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

