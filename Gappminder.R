library(shiny)

load("src/Gapminder/gapminder_2018.rda")

dat <- read.csv("src/gapminder_2018.csv", sep = ";", header = TRUE) 
ui <- fluidPage(titlePanel("Gapminder Data"),
                #"Here there could be some text explaining this App", 
                sidebarLayout(
                  
                  sidebarPanel(
                    # select imput
                    selectInput(inputId = "x", label = "x-achse",
                                choices = c("pro_kopf_gdp_2018",
                                            "co2_per_person",
                                            "rate_angestellt_frauen",
                                            "rate_angestellt_maenner",
                                            "lebenserwartung",                
                                            "kindersterblichkeit_0_5_pro_1000",
                                            "kinder_pro_frau")),
                    selectInput(inputId = "y", label = "y-achse",
                                choices = c("pro_kopf_gdp_2018",
                                            "co2_per_person", 
                                            "rate_angestellt_frauen",
                                            "rate_angestellt_maenner",
                                            "lebenserwartung",              
                                            "kindersterblichkeit_0_5_pro_1000",
                                            "kinder_pro_frau"))
                  ),
                  
                  mainPanel(
                    ##plot output 
                    plotOutput("plot"))
                )
)



server <- function(input, output){
  # MFM total score
  output$plot <- renderPlot({
    # get variables
    x <- input$x
    y <- input$y
    # load data
    dat <- read.csv("src/gapminder_2018.csv", sep = ";", header = TRUE)  
    plot(dat[, x] ~ dat[, y], xlab = x, ylab = y, main = "Gappminder-Daten 2018")
  })
}

shinyApp(ui = ui, server = server) 

