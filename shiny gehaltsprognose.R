### SHINY
library(tidyverse)
library(shinyWidgets)
library(shiny)

#shinyWidgets::shinyWidgetsGallery()

## App starts here
ui <- fluidPage(
  titlePanel("Dynamische modellbasierte Gehaltsprognose in Shiny"),
  ## Mittelwert
  sliderInput(inputId="CODY", 
              label="Wieviele Jahre Coding-Erfahrung bringst du mit?", min = 1, max = 25, value = 5),
  ## YearsCodeProf
  sliderInput(inputId="CODPROF", 
              label="Und wie lange davon codest du schon professionell?", min = 1, max = 25, value = 5),
  sliderInput(inputId="ORG", 
              label="Wie gross ist der Betrieb, in dem du arbeitest?", min = 5, max = 1000, value = 500),
  ## W�hrung
  radioGroupButtons(
    inputId = "CURR",
    label = "W�hle deine W�hrung", choices = c("EUR", "USD", "OTH"),
    status = "success"
  ),
  numericInput(inputId="AGE", label="Verr�tst du uns noch, wie viele Jahre jung du bist?", min = 20, max = 60, step = 1, value = 25),
  # Add text output
  uiOutput(outputId = "predictor1"),
  uiOutput(outputId = "predictor2")
)

server <- function(input, output, session){
  ## calc NV:
  gehaltpredict_rf <- reactive({
    vorhersager <- data.frame(ConvertedCompYearly = 50000, 
                              Currency = input$CURR, YearsCode = input$CODY, 
                              YearsCodePro = input$CODPROF,
                              Age = input$AGE, OrgSize = input$ORG)
    bla <- predict(easy_model2, vorhersager)$predictions
    round(bla, 0)
  })
  gehaltpredict_gbm <- reactive({
    vorhersager <- data.frame(ConvertedCompYearly = 50000, 
                              Currency = input$CURR, YearsCode = input$CODY, 
                              YearsCodePro = input$CODPROF,
                              Age = input$AGE, OrgSize = input$ORG)
    bla <- predict(easy_model_gbm, vorhersager)
    round(bla, 0)
  })
  ## Render NV plot
  output$predictor1 <- renderText({
    blabla <- paste0("Die ", "<b>", "RANDOM-FOREST-KRISTALLKUGEL", "</b>", " sagt, dein prognostiziertes Gehalt betr�gt: ",
                     "<b>", gehaltpredict_rf(), "</b>", " Euro - Gl�ckwunsch!") 
  })
  output$predictor2 <- renderText({
    HTML(paste0("Der ","<font color=\"#800000\"> <b>","ALTERNATIVE ALGORITHMUS GBM","</b></font>", " hingegen gibt dir: ",
                "<font color=\"#800000\"> <b>", gehaltpredict_gbm(), " Euro", "</b></font>", "- gef�llt dir das besser?") )
  }) 
}

shinyApp(ui = ui, server = server)

