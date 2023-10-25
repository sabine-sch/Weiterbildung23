library(shiny)
library(gamlss)

load("src/Gapminder/gapminder_2018.rda")

dat <- read.csv("src/Gapminder/gapminder_2018.csv", sep = ";", header = TRUE) 

ui <- fluidPage(titlePanel("Gapminder Data"),
                #"Here there could be some text explaining this App", 
                sidebarLayout(
                  
                  sidebarPanel(
                # select imput
                selectInput(inputId = "x", label = "x-achse     ",
                            choices = c("pro_kopf_gdp_2018", "co2_per_person", "rate_angestellt_frauen")),
                selectInput(inputId = "y", label = "x-achse     ",
                            choices = c("pro_kopf_gdp_2018", "co2_per_person", "rate_angestellt_frauen"))
                  ),
                
                mainPanel(
                ##plot output 
                plotOutput("plot"))
                )
            )



server <- function(input, output){
  # MFM total score
  output$plot <- renderPlot({
      #validate(
      #  need(input$age != "", "Please select age")
      #)
      # get group
    x <- input$x
    y <- input$y
      # load data
      dat <- read.csv("src/Gapminder/gapminder_2018.csv", sep = ";", header = TRUE)  
      plot(y ~ x, data = dat)
  })
}


shinyApp(ui = ui, server = server) 

