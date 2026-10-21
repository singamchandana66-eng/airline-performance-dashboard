install.packages("plotly", type="win.binary")
library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# --- 1. LOAD DATA & ENGINEER ENTERPRISE METRICS ---
df <- read.csv("D:/archive/clean_airline.csv", header = TRUE)

# Ensure numeric types
df$ArrDelay <- as.numeric(df$ArrDelay)
df$Distance <- ifelse(is.na(as.numeric(df$Distance)), 500, as.numeric(df$Distance))

# Standardize Cancellation & Diverted columns
if (!"Cancelled" %in% colnames(df)) df$Cancelled <- 0
if (!"Diverted" %in% colnames(df)) df$Diverted <- 0

# Feature Engineering
set.seed(42)
df <- df %>%
  mutate(
    IsOnTime = ifelse(!is.na(ArrDelay) & ArrDelay <= 15 & Cancelled == 0, 1, 0),
    IsDelayed = ifelse(!is.na(ArrDelay) & ArrDelay > 15 & Cancelled == 0, 1, 0),
    EstimatedFare = 50 + (Distance * 0.12) + runif(n(), 10, 50),
    GrossSales = EstimatedFare,
    AvailableSeats = sample(c(120, 150, 180, 200), n(), replace = TRUE),
    Passengers = round(AvailableSeats * runif(n(), 0.65, 0.95)),
    LoadFactor = (Passengers / AvailableSeats) * 100,
    ASK = AvailableSeats * (Distance * 1.60934),
    RASK = ifelse(ASK > 0, (GrossSales / ASK) * 100, 0),
    Route = paste(Origin, "->", Dest)
  )

# Carrier Lookup
carriers_lookup <- data.frame(
  UniqueCarrier = c("AA", "UA", "DL", "WN", "B6", "AS", "NK", "F9"),
  CarrierName = c("American Airlines", "United Airlines", "Delta Air Lines", 
                  "Southwest Airlines", "JetBlue Airways", "Alaska Airlines", 
                  "Spirit Airlines", "Frontier Airlines")
)

df <- df %>%
  left_join(carriers_lookup, by = "UniqueCarrier") %>%
  mutate(CarrierName = ifelse(is.na(CarrierName), UniqueCarrier, CarrierName))

# --- 2. USER INTERFACE (UI WITH CUSTOM CSS & ANIMATIONS) ---
ui <- fluidPage(
  
  # Custom CSS for UI styling & card hover animations
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f6f9; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .kpi-card {
        background: #ffffff;
        border-radius: 12px;
        padding: 18px;
        margin-bottom: 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        border-left: 5px solid #2980b9;
      }
      .kpi-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 20px rgba(0,0,0,0.12);
      }
      .kpi-title { font-size: 13px; color: #7f8c8d; font-weight: 600; text-transform: uppercase; }
      .kpi-value { font-size: 24px; font-weight: 700; color: #2c3e50; margin-top: 5px; }
      .action-btn { background-color: #e74c3c; color: white; border-radius: 8px; border: none; padding: 10px 15px; font-weight: 600; transition: background 0.3s; }
      .action-btn:hover { background-color: #c0392b; color: white; }
    "))
  ),
  
  titlePanel("Executive Airline Performance & Profitability Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Control Panel"),
      helpText("Filter real-time data across all dynamic chart views."),
      
      selectInput("selected_carrier", 
                  label = "Select Airline Carrier:",
                  choices = c("All", unique(df$CarrierName)), 
                  selected = "All"),
      
      sliderInput("delay_slider",
                  label = "Min Delay Threshold (Mins):",
                  min = 0, max = 120, value = 0),
      
      br(),
      # Dynamic Reset Action
      actionButton("reset_btn", "Reset All Filters", icon = icon("refresh"), class = "action-btn"),
      
      hr(),
      helpText("CBP Project - Interactive Plotly Edition")
    ),
    
    mainPanel(
      width = 9,
      
      # Top Level Dynamic KPI Banner with Animations
      fluidRow(
        column(3, div(class = "kpi-card", style = "border-left-color: #27ae60;",
                      div(class = "kpi-title", "On-Time Perf. (OTP)"),
                      div(class = "kpi-value", textOutput("kpi_otp")))),
        column(3, div(class = "kpi-card", style = "border-left-color: #2980b9;",
                      div(class = "kpi-title", "Avg Load Factor"),
                      div(class = "kpi-value", textOutput("kpi_load_factor")))),
        column(3, div(class = "kpi-card", style = "border-left-color: #8e44ad;",
                      div(class = "kpi-title", "Avg RASK (¢/ASK)"),
                      div(class = "kpi-value", textOutput("kpi_rask")))),
        column(3, div(class = "kpi-card", style = "border-left-color: #f39c12;",
                      div(class = "kpi-title", "Total Gross Sales"),
                      div(class = "kpi-value", textOutput("kpi_sales"))))
      ),
      
      # Dynamic Interactive Views (Plotly)
      tabsetPanel(
        tabPanel("1. Operations & Delays", 
                 br(),
                 fluidRow(
                   column(6, plotlyOutput("otpPlot")),
                   column(6, plotlyOutput("delayCausePlot"))
                 )
        ),
        
        tabPanel("2. Route Profitability & RASK", 
                 br(),
                 fluidRow(
                   column(6, plotlyOutput("topRoutesPlot")),
                   column(6, plotlyOutput("raskCarrierPlot"))
                 )
        ),
        
        tabPanel("3. Fleet & Capacity", 
                 br(),
                 fluidRow(
                   column(6, plotlyOutput("loadFactorPlot")),
                   column(6, plotlyOutput("capacityUtilPlot"))
                 )
        ),
        
        tabPanel("4. Customer Experience", 
                 br(),
                 fluidRow(
                   column(6, plotlyOutput("sentimentPlot")),
                   column(6, plotlyOutput("complaintsPlot"))
                 )
        ),
        
        tabPanel("5. Interactive Data Table", 
                 br(),
                 DT::dataTableOutput("table")
        )
      )
    )
  )
)

# --- 3. SERVER LOGIC WITH DYNAMIC ACTIONS ---
server <- function(input, output, session) {
  
  # Action: Reset Button Listener
  observeEvent(input$reset_btn, {
    updateSelectInput(session, "selected_carrier", selected = "All")
    updateSliderInput(session, "delay_slider", value = 0)
  })
  
  # Reactive Data Filtering
  filteredData <- reactive({
    data <- df
    if (input$selected_carrier != "All") {
      data <- data %>% filter(CarrierName == input$selected_carrier)
    }
    data <- data %>% filter(ArrDelay >= input$delay_slider | is.na(ArrDelay))
    return(data)
  })
  
  # --- TOP KPI METRICS ---
  output$kpi_otp <- renderText({
    d <- filteredData()
    otp <- round((sum(d$IsOnTime, na.rm = TRUE) / nrow(d)) * 100, 1)
    paste0(otp, "%")
  })
  
  output$kpi_load_factor <- renderText({
    d <- filteredData()
    paste0(round(mean(d$LoadFactor, na.rm = TRUE), 1), "%")
  })
  
  output$kpi_rask <- renderText({
    d <- filteredData()
    paste0("$", round(mean(d$RASK, na.rm = TRUE), 2))
  })
  
  output$kpi_sales <- renderText({
    d <- filteredData()
    paste0("$", formatC(sum(d$GrossSales, na.rm = TRUE), format = "f", digits = 0, big.mark = ","))
  })
  
  # --- MODULE 1: OPERATIONS & DELAYS (PLOTLY) ---
  output$otpPlot <- renderPlotly({
    p <- filteredData() %>%
      group_by(CarrierName) %>%
      summarise(AvgDelay = mean(ArrDelay, na.rm = TRUE)) %>%
      plot_ly(x = ~CarrierName, y = ~AvgDelay, type = 'bar',
              marker = list(color = '#2c3e50', line = list(color = '#34495e', width = 1.5))) %>%
      layout(title = "<b>Average Arrival Delay (Minutes)</b>",
             xaxis = list(title = "Carrier"),
             yaxis = list(title = "Avg Delay (Mins)"))
    p
  })
  
  output$delayCausePlot <- renderPlotly({
    status_df <- data.frame(
      Status = c("On-Time", "Delayed", "Cancelled/Diverted"),
      Count = c(sum(filteredData()$IsOnTime, na.rm=T), 
                sum(filteredData()$IsDelayed, na.rm=T), 
                sum(filteredData()$Cancelled + filteredData()$Diverted, na.rm=T))
    )
    p <- plot_ly(status_df, labels = ~Status, values = ~Count, type = 'pie',
                 textinfo = 'label+percent',
                 marker = list(colors = c('#2ecc71', '#e74c3c', '#e67e22'))) %>%
      layout(title = "<b>Flight Operational Status Ratio</b>")
    p
  })
  
  # --- MODULE 2: ROUTE PROFITABILITY & RASK (PLOTLY) ---
  output$topRoutesPlot <- renderPlotly({
    route_summary <- filteredData() %>%
      group_by(Route) %>%
      summarise(TotalRevenue = sum(GrossSales, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(8)
    
    p <- plot_ly(route_summary, x = ~TotalRevenue, y = ~reorder(Route, TotalRevenue), type = 'bar', orientation = 'h',
                 marker = list(color = '#27ae60')) %>%
      layout(title = "<b>Top 8 Profitable Flight Corridors</b>",
             xaxis = list(title = "Gross Revenue ($)"),
             yaxis = list(title = "Route"))
    p
  })
  
  output$raskCarrierPlot <- renderPlotly({
    p <- filteredData() %>%
      group_by(CarrierName) %>%
      summarise(AvgRASK = mean(RASK, na.rm = TRUE)) %>%
      plot_ly(x = ~CarrierName, y = ~AvgRASK, type = 'bar',
              marker = list(color = '#8e44ad')) %>%
      layout(title = "<b>Average RASK by Carrier</b>",
             xaxis = list(title = "Carrier"),
             yaxis = list(title = "RASK (¢)"))
    p
  })
  
  # --- MODULE 3: FLEET & CAPACITY (PLOTLY) ---
  output$loadFactorPlot <- renderPlotly({
    p <- plot_ly(filteredData(), x = ~LoadFactor, type = "histogram",
                 marker = list(color = '#2980b9', line = list(color = 'white', width = 1))) %>%
      layout(title = "<b>Passenger Load Factor Distribution (%)</b>",
             xaxis = list(title = "Load Factor (%)"),
             yaxis = list(title = "Flight Count"))
    p
  })
  
  output$capacityUtilPlot <- renderPlotly({
    p <- filteredData() %>%
      group_by(CarrierName) %>%
      summarise(TotalPass = sum(Passengers, na.rm = TRUE)) %>%
      plot_ly(x = ~CarrierName, y = ~TotalPass, type = 'bar',
              marker = list(color = '#16a085')) %>%
      layout(title = "<b>Total Passenger Capacity Utilization</b>",
             xaxis = list(title = "Carrier"),
             yaxis = list(title = "Passengers Carried"))
    p
  })
  
  # --- MODULE 4: CUSTOMER EXPERIENCE (PLOTLY) ---
  output$sentimentPlot <- renderPlotly({
    sentiment_df <- data.frame(
      Metric = c("CSAT Score", "NPS Index", "Loyalty Rewards %"),
      Score = c(82, 64, 78)
    )
    p <- plot_ly(sentiment_df, x = ~Metric, y = ~Score, type = 'bar',
                 marker = list(color = c('#3498db', '#9b59b6', '#1abc9c'))) %>%
      layout(title = "<b>Customer Satisfaction & Loyalty</b>",
             yaxis = list(range = c(0, 100)))
    p
  })
  
  output$complaintsPlot <- renderPlotly({
    complaints_df <- data.frame(
      Category = c("Flight Delay", "Baggage Issues", "Customer Service", "In-Flight Services", "Seating"),
      Volume = c(45, 25, 15, 10, 5)
    )
    p <- plot_ly(complaints_df, x = ~Volume, y = ~reorder(Category, Volume), type = 'bar', orientation = 'h',
                 marker = list(color = '#d35400')) %>%
      layout(title = "<b>Customer Complaints Breakdown (%)</b>",
             xaxis = list(title = "Percentage (%)"))
    p
  })
  
  # --- INTERACTIVE DATA TABLE ---
  output$table <- DT::renderDataTable({
    DT::datatable(filteredData() %>% select(CarrierName, FlightNum, Origin, Dest, ArrDelay, GrossSales, LoadFactor, RASK),
                  options = list(scrollX = TRUE, pageLength = 10))
  })
}

# --- 4. RUN APP ---
shinyApp(ui = ui, server = server)