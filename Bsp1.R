## lese die Daten ein

dataLungCap <- read.table("src/LungCapData2.csv", sep = ",", header = TRUE)

library(shiny)

ui <- fluidPage(titlePanel("Lungenkapazität"),
                #"Here there could be some text explaining this App", 
                sidebarLayout(
                  sidebarPanel(
                    # select imput
                    selectInput(inputId = "x", label = "x-achse",
                                choices = c("Smoke", "Age", "LungCap", "Height", "Gender")),
                    selectInput(inputId = "y", label = "y-achse",
                                choices = c("LungCap", "Age", "Height")),
                    selectInput(inputId = "t", label = "statistischer test",
                                choices = c("T-test", "lineares Regressionsmodell")),
                    radioButtons(inputId = "c1", label = "Gender",
                                 choices = c("", "Gender"),
                                 selected = NULL),
                    #checkboxInput(inputId = "c1", label = "Gender"),
                    radioButtons(inputId = "c2", label = "Age",
                                 choices = c("", "Age"),
                                 selected = NULL),
                    radioButtons(inputId = "c3", label = "Height",
                                   choices = c("", "Height"),
                                   selected = NULL)
                  ),
                  mainPanel(
                    ##plot output 
                    plotOutput("plot"),
                    tableOutput('table')),
                )
)



server <- function(input, output){
  # MFM total score
  output$plot <- renderPlot({
    # load data
    dat <- read.csv("src/LungCapData2.csv", sep = ",", header = TRUE)  
    # get group
    x <- dat[, input$x]
    y <- dat[, input$y]
    if(input$x %in% c("Smoke", "Gender")){
      boxplot(y ~ x, data = dat, xlab = input$x, ylab = input$y)
    }
    if(!input$x %in% c("Smoke", "Gender")){
      plot(y ~ x, data = dat, xlab = input$x, ylab = input$y)
    }
    if(input$x %in% c("Smoke", "Gender")){
      if(input$t == "T-test"){
        test <- t.test(y ~ x, data = dat, var.equal = TRUE)
        tab <- data.frame(mean_1 = sprintf("%.2f", test$estimate[1]), 
                          mean_2 = sprintf("%.2f", test$estimate[2]), 
                          difference = sprintf("%.2f", test$estimate[2] - test$estimate[1]),
                          p = format.pval(test$p.value, eps = 0.01, dig = 2))
      }
      if(input$t == "lineares Regressionsmodell"){
        my.formula <- as.formula(paste("y ~ ", paste(input$c1, input$c2, input$c3, sep = "+"), "+ Smoke"))
        ##
        mod <- lm(my.formula, data = dat)
        variablenames <- gsub("Gendermale", "male (vs. female)", 
                              gsub("Smokeyes", "Smoke yes (vs. no)",
                                   rownames(summary(mod)$coef)))
        tab <- cbind("Variable" = variablenames, as.data.frame(summary(mod)$coef))
      }
    }
    if(!input$x %in% c("Smoke", "Gender")){
      tab = NULL
    }
    output$table <- renderTable(tab)
  })    
}


shinyApp(ui = ui, server = server) 



