install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(ggplot2)

#Black-Scholes function years to days
days_to_years <- function(days) {
  return(days/365)
}

#Black-Scholes
bs_call <- function(S, K, days, r, sigma) {
  T <- days_to_years(days)
  d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  call <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(call)
}

bs_put <- function(S, K, days, r, sigma) {
  T <- days_to_years(days)
  d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  put <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  return(put)
}

# UI Definition
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Black-Scholes Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Sensitivity Analysis", tabName = "sensitivity", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
                .box.box-solid.box-primary>.box-header {
                    background:white;
                    color:red;
                }
                .box.box-solid.box-primary {
                    border:1px solid black;
                }
                 .content-wrapper {
                    background-color: white;
                }
            "))
    ),
    tabItems(
      tabItem(
        tabName = "calculator",
        col="black",
        fluidRow(
          box(
            title = "Input Parameters",
            col="black",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            numericInput("stock_price", "Stock Price (S)", 100, min = 0),
            numericInput("strike_price", "Strike Price (K)", 100, min = 0),
            sliderInput("days", "Days to Expiry", 
                        min = 1, max = 365, value = 30),
            numericInput("rate", "Risk-free Rate (r)", 0.05, min = 0, max = 1, step = 0.01),
            sliderInput("volatility", "Volatility (σ)", 
                        min = 0.05, max = 1, value = 0.2, step = 0.05),
            helpText("Note: Time is measured in calendar days")
          ),
          box(
            title = "Option Prices",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            fluidRow(
              valueBoxOutput("call_box", width = 6),
              valueBoxOutput("put_box", width = 6)
            ),
            fluidRow(
              column(12,
                     tags$div(
                       style = "margin-top: 20px;",
                       textOutput("time_display")
                     )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "sensitivity",
        fluidRow(
          box(
            title = "Sensitivity Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(3,
                     selectInput("sensitivity_param", "Parameter to Analyze:",
                                 choices = c("Stock Price" = "price",
                                             "Days to Expiry" = "days",
                                             "Volatility" = "volatility"))
              )
            ),
            plotOutput("sensitivity_plot", height = "400px")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  # Reactive calculations for option prices
  prices <- reactive({
    list(
      call = bs_call(input$stock_price, input$strike_price, 
                     input$days, input$rate, input$volatility),
      put = bs_put(input$stock_price, input$strike_price, 
                   input$days, input$rate, input$volatility)
    )
  })
  
  # Display time in years (for reference)
  output$time_display <- renderText({
    sprintf("Time to expiry: %d days (%.3f years)", 
            input$days, days_to_years(input$days))
  })
  
  # Value boxes for option prices
  output$call_box <- renderValueBox({
    valueBox(
      sprintf("$%.2f", prices()$call),
      "Call Option Price",
      color = "green",
      icon = icon("arrow-up")
    )
  })
  
  output$put_box <- renderValueBox({
    valueBox(
      sprintf("$%.2f", prices()$put),
      "Put Option Price",
      color = "red",
      icon = icon("arrow-down")
    )
  })
  
  # Sensitivity Analysis Plot
  output$sensitivity_plot <- renderPlot({
    # Generate range based on selected parameter
    if (input$sensitivity_param == "price") {
      param_range <- seq(input$stock_price * 0.5, input$stock_price * 1.5, length.out = 100)
      calls <- sapply(param_range, function(x) {
        bs_call(x, input$strike_price, input$days, input$rate, input$volatility)
      })
      puts <- sapply(param_range, function(x) {
        bs_put(x, input$strike_price, input$days, input$rate, input$volatility)
      })
      xlabel <- "Stock Price"
    } else if (input$sensitivity_param == "days") {
      param_range <- seq(1, 365, length.out = 100)
      calls <- sapply(param_range, function(x) {
        bs_call(input$stock_price, input$strike_price, x, input$rate, input$volatility)
      })
      puts <- sapply(param_range, function(x) {
        bs_put(input$stock_price, input$strike_price, x, input$rate, input$volatility)
      })
      xlabel <- "Days to Expiry"
    } else {  # volatility
      param_range <- seq(0.05, 0.5, length.out = 100)
      calls <- sapply(param_range, function(x) {
        bs_call(input$stock_price, input$strike_price, input$days, input$rate, x)
      })
      puts <- sapply(param_range, function(x) {
        bs_put(input$stock_price, input$strike_price, input$days, input$rate, x)
      })
      xlabel <- "Volatility"
    }
    
    # Create data frame for plotting
    df <- data.frame(
      x = rep(param_range, 2),
      price = c(calls, puts),
      type = rep(c("Call", "Put"), each = length(param_range))
    )
    
    # Create plot
    ggplot(df, aes(x = x, y = price, color = type)) +
      geom_line(size = 1) +
      labs(x = xlabel,
           y = "Option Price",
           title = paste("Option Price Sensitivity to", xlabel)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            legend.position = "bottom",
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)) +
      scale_color_manual(values = c("Call" = "forestgreen", "Put" = "firebrick"))
  })
}


# Run the application
shinyApp(ui = ui, server = server)

                