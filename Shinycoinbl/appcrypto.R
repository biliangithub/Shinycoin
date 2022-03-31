library(shiny)
library(tidyverse)
library(tidyquant)
library(rvest)
library(readxl)
library(timetk)
library(plotly)
library(feasts)
library(tsibble)
library(ggHoriPlot)
library(ggplot2)
library(ggthemes)
library(modeltime)

# Import top 30 cryptocurrencies symbols downloaded from Yahoo Finance
top30 <- read_xlsx("data/top30coins.xlsx")
crypto <- read_csv("data/crypto.csv")

symbols <- top30 %>%
  select(`Symbol`)

top30symbol <- as.vector(symbols$Symbol)
from_date = "2020-01-01"
to_date = "2022-02-28"
period_type = "days"  # daily prices chosen


# Define UI for application
ui <- fluidPage (
      navbarPage("ShinyCoin",
        tabPanel("Summary"),
        tabPanel("Exploratory",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(inputId = "coins",
                                 label = "Select a coin:",
                                 choices = c(top30symbol),
                                 selected = ""),
                     dateRangeInput(inputId = "date", 
                                    label = "Date range",
                                    start = "2020-01-01",
                                    end = Sys.Date(),
                                    max = Sys.Date()),
                     submitButton("View")),
                   mainPanel(plotOutput("hori"))
                 )      
                 
                 ),
        
        #Statistical Analysis 
          navbarMenu("Statistical",
                     
            tabPanel("Time Series Decomposition",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "coins",
                              label = "Select a coin:",
                              choices = c(top30symbol),
                              selected = ""),
                  dateRangeInput(inputId = "date", 
                                 label = "Date range",
                                 start = "2020-01-01",
                                 end = Sys.Date(),
                                 max = Sys.Date()),
                  submitButton("View")),
                mainPanel(plotlyOutput("tsstl"), dataTableOutput("coins"))
                            )
                      ), 
            
            tabPanel("Seasonality Diagnostics",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "coins",
                              label = "Select a coin:",
                              choices = c(top30symbol),
                              selected = ""),
                  dateRangeInput(inputId = "date", 
                              label = "Date range",
                              start = "2020-01-01",
                              end = Sys.Date(),
                              max = Sys.Date()),
                  submitButton("Analyse")),
                mainPanel(plotlyOutput("tsdiag"), plotlyOutput("tscorr"), plotOutput("tsauto"))
                              )
                      ), 
            tabPanel("Correlation",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "coins",
                              label = "Select coin:",
                              choices = c(top30symbol),
                              selected = ""),
                  selectInput(inputId = "compare",
                              label = "Compare with:",
                              choices = c(top30symbol),
                              selected = ""),
                  dateRangeInput(inputId = "date", 
                              label = "Date range",
                              start = "2020-01-01",
                              end = Sys.Date(),
                              max = Sys.Date()),
                  submitButton("Compare")),
                mainPanel(plotlyOutput("tsccf"))
                             )
                     )
            ),
        
        #Forecasting (XX)
        tabPanel("Prediction")
               )
)

# Define server logic

server <- function(input, output, session) {
                coins <- reactive({
                  crypto %>%
                    #filter (symbol == input$coins) %>%
                    filter (date >= format(input$date[1]))%>%
                    filter (date <= format(input$date[2]))
                    })
        
                
                #STL Decomposition
                coins_stl <- reactive ({
                  coins() %>%
                    filter (symbol == input$coins)%>%
                    group_by(symbol) %>%
                   #tq_transmute(select= NULL, 
                  #               mutate_fun = to.period, period = "days") %>%
                    plot_stl_diagnostics(date, adjusted, .frequency = "auto", .feature_set=c("observed", "season", "trend", "remainder"),.trend="1 month")
                })
                output$tsstl <- renderPlotly(coins_stl())                
                
                #Seasonal Diagnostics
                coins_plots <- reactive ({
                  coins() %>%
                    group_by(symbol) %>%
                    plot_seasonal_diagnostics(date, adjusted, .feature_set = c("month.lbl", "year"))
                })
                output$tsdiag <- renderPlotly(coins_plots())
                
                #Autocorrelation - timetk
                coins_corr <- reactive ({
                  coins() %>%
                    group_by(symbol) %>%
                    plot_acf_diagnostics(date, adjusted)
                })
                output$tscorr <- renderPlotly(coins_corr())
                
                #Autocorrelation - feasts
                coins_auto <- reactive ({
                  as_tsibble(coins(), key = symbol, index = date) %>%
                    group_by(symbol) %>%
                    ACF(adjusted,na.action = na.pass) %>%
                     autoplot()
                })
                output$tsauto <- renderPlot(coins_auto())
 

                output$coins <- renderDataTable(coins())
                

}
# Run the application 
shinyApp(ui = ui, server = server)
