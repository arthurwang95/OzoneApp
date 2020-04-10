library(shiny)
library(plotly)
load("ozone.RData")

ui <- fluidPage(
      titlePanel("Predict Ozone Level from Wind Speed and Temperature"),
      sidebarLayout(
            sidebarPanel(
                  h3("Weather Parameters"),
                  br(),
                  sliderInput("Slider1", "Wind Speed (MPH)", min = 0, max = 20, value = 10, step = 0.5),
                  sliderInput("Slider2", " Tempeature (\u00B0C)", min =55, max = 100, value = 78, step = 1),
                  br(),
                  submitButton("Submit!")
            ),
            mainPanel(
                  h4("Documentation", align = "center"),
                  p("This application predicts the ozone level based on two parameters you entered:", 
                    span("wind speed", style = "color:blue"), 
                    "in miles per hour (MPH) and", 
                    span("temperture (\u00B0C)", style = "color:blue"), "."),
                  p("The prediction model used here is calculated from the ", 
                    span("Ozone", style = "color:blue"),
                    "data set from the ElemStatLearn R package Version 2015.6.26:"),
                  a("https://cran.ma.imperial.ac.uk/src/contrib/Archive/ElemStatLearn/"),
                  br(),
                  h5("By Arthur"),
                  br(),
                  
                  tabsetPanel( type = "tabs", 
                               tabPanel("Predicted by Wind", 
                                        br(), 
                                        plotlyOutput("plot1"),
                                        h3("A simple linear regression model is used for prediction"),
                                        h4("Predicted Ozone level:"),
                                        span(textOutput("Ozone1_out"), style = "color:red"),
                                        br(),
                                        h4("Prediction Interval:"),
                                        span(textOutput("PredInt1"), style = "color:red"),
                                        br()
                               ),
                               tabPanel("Predicted by Temperatue", 
                                        br(), 
                                        plotlyOutput("plot2"),
                                        h3("A simple linear regression model is used for prediction"),
                                        h4("Predicted Ozone level:"),
                                        span(textOutput("Ozone2_out"), style = "color:red"),
                                        br(),
                                        h4("Prediction Interval:"),
                                        span(textOutput("PredInt2"), style = "color:red"),
                                        br()
                               )
                  )
            )
            
      )
)


server <- function(input, output) {
      
      
      mod1 <- lm(ozone ~ wind, data = ozone)
      mod2 <- lm(ozone ~ temperature, data = ozone)
      
      modPred1 <- reactive({
            windInput <- input$Slider1
            predict(mod1, newdata = data.frame(wind = windInput),interval = "prediction")
      })
      
      modPred2 <- reactive({
            tempInput <- input$Slider2
            predict(mod2, newdata = data.frame(temperature = tempInput),interval = "prediction")
      })
      
      output$plot1 <- renderPlotly({
            
            windInput <- input$Slider1
            
            plot_ly(x = ~wind, y = ~ozone, data = ozone, type = "scatter", mode = "markers", size = 2) %>%
                  add_trace(x = ~wind, y = ~predict(mod1), mode = "lines", line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% 
                  add_trace(x = windInput, y = modPred1()[1], type = "scatter", size = 4, text = "Predicted Ozone") %>% 
                  layout(showlegend = F, title = "Ozone Level by Wind Speed")
      })
      
      output$Ozone1_out <- renderText(round(modPred1()[1],2))
      
      output$PredInt1 <- renderText({
            paste("(",round(modPred1()[2],2), "," , round(modPred1()[3],2), ")")
      })
      
      output$plot2 <- renderPlotly({
            
            tempInput <- input$Slider2
            
            plot_ly(x = ~temperature, y = ~ozone, data = ozone, type = "scatter", mode = "markers", size = 2) %>%
                  add_trace(x = ~temperature, y = ~predict(mod2), mode = "lines", line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% 
                  add_trace(x = tempInput, y = modPred2()[1], type = "scatter", size = 4, text = "Predicted Ozone") %>% 
                  layout(showlegend = F, title = "Ozone Level by Temperature")
      })
      
      output$Ozone2_out <- renderText(round(modPred2()[1],2))
      
      output$PredInt2 <- renderText({
            paste("(",round(modPred2()[2],2), "," , round(modPred2()[3],2), ")")
      })
      
}

shinyApp(ui = ui, server = server)