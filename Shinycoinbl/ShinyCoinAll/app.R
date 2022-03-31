library(shiny)
library(shinythemes)
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
library(xgboost)
library(bslib)
library(rsconnect)

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
  theme = bs_theme(bootswatch = "cerulean"),
  titlePanel("Analysis on Crytocurrencies"),
  navbarPage("ShinyCoin",
             tabPanel("Summary"),
             navbarMenu("Exploratory",
                        tabPanel("Horizon Graph",
                                 sidebarLayout(
                                   sidebarPanel(
                                     dateRangeInput(inputId = "dateh",
                                                    label = "Date range",
                                                    start = "2020-01-01",
                                                    end = Sys.Date(),
                                                    max = Sys.Date()),
                                     actionButton("gomap","View")
                                   ),
                                   mainPanel(plotOutput("hori"))
                                 )
                        ),
                        tabPanel("Anomaly Detection",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                
                                                # Let user pick coins
                                                selectizeInput(
                                                  inputId = "coinsa",
                                                  label = h4("Select a Coin"),
                                                  choices =c(top30symbol),
                                                  selected = "BTC-USD", 
                                                  multiple = T,
                                                  options = list(maxItems = 4)
                                                ),
                                                
                                                # Pick time period
                                                radioButtons("period", label = h4("Period"),
                                                             choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
                                                             selected = 4
                                                ),
                                                dateRangeInput(inputId = "datea",
                                                               label = "Date range",
                                                               start = "2020-01-01",
                                                               end = Sys.Date(),
                                                               max = Sys.Date())
                                                
                                   ),
                                   
                                   # Plot results
                                   mainPanel(
                                     plotlyOutput("anom",height=600)
                                   )
                                 )
                                 
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
                                     selectInput(inputId = "pricetype",
                                                 label = "Choose a value:",
                                                 choices = list("Adjusted"="adjusted", 
                                                                "High" = "high",
                                                                "Low" = "low",
                                                                "Open" = "open",
                                                                "Close" = "close",
                                                                "Volume" = "volume"),
                                                 selected = ""),
                                     dateRangeInput(inputId = "date", 
                                                    label = "Date range",
                                                    start = "2020-01-01",
                                                    end = Sys.Date()-1,
                                                    max = Sys.Date()-1),
                                     actionButton("gobutton", "View")),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("STL Diagnostics", plotlyOutput("tsstl", height = "500")),
                                       tabPanel("Seasonal Diagnostics", plotlyOutput("tsdiag")),
                                       tabPanel("Auto-correlation-timetk", plotlyOutput("tscorr")),
                                       tabPanel("Auto-correlation-feasts", plotOutput("tsauto"))
                                              )
                                            )
                                       )
                              ), 
                      
                        tabPanel("Crossed-Correlation",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "coin1",
                                                 label = "Select coin:",
                                                 choices = c(top30symbol),
                                                 selected = ""),
                                     selectInput(inputId = "coin2",
                                                 label = "Compare with:",
                                                 choices = c(top30symbol),
                                                 selected = ""),
                                     dateRangeInput(inputId = "datec", 
                                                    label = "Date range",
                                                    start = "2020-01-01",
                                                    end = Sys.Date(),
                                                    max = Sys.Date()),
                                     actionButton("comparebutton", "Compare")),
                                   mainPanel(plotlyOutput("tsccf"))
                                 )
                        )
             ),
             
             #Forecasting (XX)
             tabPanel("Prediction",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "coinsf",
                                      label = "Subject:",
                                      choices = c(top30symbol),
                                      selected = ""),
                          sliderInput(inputId = "datef",
                                      label = "Date",
                                      min = as.Date("2020-01-01","%Y-%m-%d"),
                                      max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                      value=as.Date("2020-01-01"),
                                      timeFormat="%Y-%m-%d")
                        ),
                        
                        mainPanel(
                          plotlyOutput("TSLine"), plotlyOutput("TSPredict"), plotlyOutput("TSForecast"), plotlyOutput("TSRefit")
                        )
                      ))
  )
)

# Define server logic

server <- function(input, output, session) {
  
  
  #horizonplot
  coinshori <- reactive(tq_get(top30symbol,get ="stock.prices", from = input$dateh[1],
                               to = input$dateh[2]))
  coins_h <- reactive({
    
    #EXPLORATORY
    input$gomap
    coinshori() %>% ggplot() +
      geom_horizon(aes(date,adjusted), origin = "midpoint") +
      #origin of horizon plot set as midpoint between the data range (default option)
      scale_fill_hcl(palette = 'RdBu', reverse = F) +
      facet_grid(symbol~.) +
      theme_few() +
      theme(
        panel.spacing.y=unit(0, "lines"),
        strip.text.y = element_text(size = 8, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none',
        axis.title=element_text(size=20),
        plot.title = element_text(size=28, face = "bold"),
        plot.subtitle = element_text(size=20, face = "bold")
      )+
      scale_x_date(expand=c(0,0), 
                   date_breaks = "1 month", 
                   date_labels = "%b%Y") +
      xlab("") +
      ggtitle('Adjusted price of top 30 coins by marketcap')+
      guides(fill= guide_legend(title="stock price +(blue) or -(red)",
                                title.position = "top"))
  })
  output$hori <- renderPlot(coins_h(),width = 800, height = 700)
  
  
  #anomaly detection - timetk
  
  # server logic based on user input
  #observeEvent(c(input$period,input$coinsa), {
    
    prices <- reactive(tq_get(input$coinsa,get ="stock.prices", from = input$datea[1],
                                 to = input$datea[2])) 
    
    
    #if (input$period == 1) {
      #prices <- prices() %>%
        #filter(date >= today() - months(3))}
    
    #if (input$period == 2) {
      #prices <- prices() %>%
        #filter(date >= today() - months(6)) }
    
    #if (input$period == 3) {
      #prices <- prices() %>%
        #filter(date >= today() - months(12)) }
    
    #if (input$period == 4) {
      #prices <- prices() %>%
        #filter(date >= today() - months(24)) }
    
    #if (input$period == 5) {
      #prices <- prices() %>%
        #filter(year(date) == year(today())) }
    
    # Create plot
    output$anom <- renderPlotly({
      prices() %>%
        group_by(symbol) %>%
        plot_anomaly_diagnostics(
          date,
          adjusted,
          .facet_vars = NULL,
          .frequency = "auto",
          .trend = "auto",
          .alpha = 0.05,
          .max_anomalies = 0.2,
          .message = TRUE,
          .facet_ncol = 2,
          .facet_scales = "free",
          .facet_dir = "h",
          .line_color = "#2c3e50",
          .line_size = 0.5,
          .line_type = 1,
          .line_alpha = 1,
          .anom_color = "#e31a1c",
          .anom_alpha = 1,
          .anom_size = 1.5,
          .ribbon_fill = "grey20",
          .ribbon_alpha = 0.2,
          .legend_show = TRUE,
          .title = "Anomaly Diagnostics",
          .x_lab = "",
          .y_lab = "price",
          .color_lab = "Anomaly",
          .interactive = TRUE
        )
      
    })
  #})
  
  #STATISTICAL

  coins <- reactive(tq_get(input$coins, get ="stock.prices", 
                           from = input$date[1], to = input$date[2], 
                           complete_cases = T))
  
  #STL Decomposition
  output$tsstl <- renderPlotly({
    input$gobutton
    coins() %>%
    group_by(symbol) %>%
      plot_stl_diagnostics(date, get(input$pricetype), 
                           .frequency = "auto", 
                           .feature_set=c("observed", "season", "trend", "remainder"),
                           .trend="1 month")
  })

  #Seasonal Diagnostics
  output$tsdiag <- renderPlotly ({
    input$gobutton
    coins() %>%
      group_by(symbol) %>%
      plot_seasonal_diagnostics(date, get(input$pricetype), .feature_set = c("month.lbl", "year"))
  })
  
  #Autocorrelation - timetk
  output$tscorr <- renderPlotly ({
    input$gobutton
    coins() %>%
      group_by(symbol) %>%
      plot_acf_diagnostics(date, get(input$pricetype))
  })
  
  #Autocorrelation - feasts
  output$tsauto <- renderPlot ({
    input$gobutton
    as_tsibble(coins(), key = symbol, index = date) %>%
      group_by(symbol) %>%
      ACF(get(input$pricetype),na.action = na.pass) %>%
      autoplot()
  })
  
  #Crossed Correlation - timetk
  cp1 <- reactive(pivot_wider(tq_get(input$coin1, get ="stock.prices", from = input$datec[1],
                            to = input$datec[2]),names_from = symbol, values_from = volume ))
  cp2 <- reactive(pivot_wider(tq_get(input$coin2, get ="stock.prices", from = input$datec[1],
                               to = input$datec[2]), names_from = symbol, values_from = volume))
  combineall <- reactive ({cp1() %>% left_join(cp2(), by = "date")})
  
  coins_ccf <- reactive ({
    input$comparebutton
    combineall() %>%
      #group_by(symbol) %>%
      plot_acf_diagnostics(date, adjusted.x, .ccf_vars = adjusted.y)
  })
  output$tsccf <- renderPlotly(coins_ccf())
  
  ##########################
  # Time Series
  
  
  coins_plots <- reactive ({
    coins() %>%
      group_by(symbol) %>%
      plot_time_series(date, adjusted)
  })
  output$TSLine <- renderPlotly(coins_plots())
  
  ##########################
  # Time Series Cross Validation Plan
  splits <- reactive ({
    coins() %>%
      time_series_split(assess = "3 months", cumulative = TRUE)
  })
  
  pred_plot <- reactive({ 
    splits() %>%
      tk_time_series_cv_plan() %>%
      plot_time_series_cv_plan(date, adjusted)
  })
  
  output$TSPredict <- renderPlotly(pred_plot())
  
 
}
# Run the application 
shinyApp(ui = ui, server = server)
