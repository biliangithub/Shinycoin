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
                mainPanel(plotlyOutput("tsstl"))
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
                coins <- reactive(tq_get(input$coins, get ="stock.prices", from = input$date[1],
                                         to = input$date[2]))
                print(coins)
                
                #STL Decomposition
                coins_stl <- reactive ({
                  coins() %>%
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
 
                #Crossed Correlation - timetk
                coins2 <- reactive(tq_get(input$compare, get ="stock.prices", from = input$date[1],
                                             to = input$date[2]))
                cp1 <- reactive ({pivot_wider(coins(), names_from = symbol)}) 
                cp2 <- reactive ({pivot_wider(coins2(), names_from = symbol)}) 
                combineall <- reactive ({cp1() %>% left_join(cp2(), by = "date")})

                coins_ccf <- reactive ({
                  combineall() %>%
                    #group_by(symbol) %>%
                    plot_acf_diagnostics(date, adjusted.x, .ccf_vars = adjusted.y)
                })
                output$tsccf <- renderPlotly(coins_ccf())
                
                #horizonplot
                coinshori <- reactive(tq_get(top30symbol,get ="stock.prices", from = input$date[1],
                                          to = input$date[2]))
                coins_h <- reactive({
                  
                  coinshori() %>% ggplot() +
                  geom_horizon(aes(date,adjusted), origin = "midpoint") +
                    #origin of horizon plot set as midpoint between the data range (default option)
                    scale_fill_hcl(palette = 'RdBu', reverse = F) +
                    facet_grid(symbol~.) +
                    theme_few() +
                    theme(
                      panel.spacing.y=unit(0, "lines"),
                      strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
                      axis.text.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.border = element_blank(), 
                      legend.position = 'none'
                    )
                })
                output$hori <- renderPlot(coins_h())
                

}
# Run the application 
shinyApp(ui = ui, server = server)
