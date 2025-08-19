#' Run the RetailScope Shiny Application
#' @description Launches the interactive Shiny dashboard.
#' @export
#' @import shiny
#' @import shinydashboard
run_app <- function() {
  # --- UI ---
  ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "RetailScope"),
    dashboardSidebar(sidebarMenu(menuItem("Overview", tabName = "overview", icon = icon("chart-line")))),
    dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
                fluidRow(
                  box(title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
                      sliderInput("date_range", "Select Date Range:",
                                  min = as.Date("2016-09-01"), max = as.Date("2018-10-31"),
                                  value = c(as.Date("2017-01-01"), as.Date("2018-01-01")), timeFormat = "%b %Y"),
                      selectInput("state_select", "Select State:",
                                  choices = c("All", "SP", "RJ", "MG", "RS", "PR", "SC", "BA", "DF", "ES", "GO", "PE", "CE", "PA", "MT", "MA", "PB", "MS", "PI", "RN", "AL", "SE", "TO", "RO", "AM", "AC", "AP", "RR"),
                                  selected = "All", multiple = TRUE))
                ),
                fluidRow(
                  valueBoxOutput("total_revenue_box", width = 4),
                  valueBoxOutput("total_orders_box", width = 4),
                  valueBoxOutput("unique_customers_box", width = 4)
                ),
                fluidRow(
                  box(title = "Trends", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      radioButtons("metric_select", "Choose Metric:",
                                   choices = c("Revenue" = "payment_value", "Orders" = "order_id"),
                                   selected = "payment_value", inline = TRUE),
                      plotly::plotlyOutput("revenue_plot", height = "400px"))
                ))
      )
    )
  )

  # --- Server ---
  server <- function(input, output) {
    sales_data <- reactive({ load_core_data() %>% clean_sales_data() })

    filtered_data <- reactive({
      df <- sales_data() %>%
        filter(order_purchase_date >= input$date_range[1], order_purchase_date <= input$date_range[2])
      if (!("All" %in% input$state_select)) { df <- df %>% filter(customer_state %in% input$state_select) }
      return(df)
    })

    output$total_revenue_box <- renderValueBox({
      total_rev <- sum(filtered_data()$payment_value, na.rm = TRUE)
      valueBox(value = paste0("₹", round(total_rev / 100000, 2), "L"), subtitle = "Total Revenue",
               icon = icon("indian-rupee-sign"), color = "purple")
    })

    output$total_orders_box <- renderValueBox({
      valueBox(value = format(n_distinct(filtered_data()$order_id), big.mark = ","), subtitle = "Total Orders",
               icon = icon("shopping-cart"), color = "yellow")
    })

    output$unique_customers_box <- renderValueBox({
      valueBox(value = format(n_distinct(filtered_data()$customer_unique_id), big.mark = ","), subtitle = "Unique Customers",
               icon = icon("users"), color = "green")
    })

    output$revenue_plot <- plotly::renderPlotly({
      if (input$metric_select == "payment_value") {
        monthly_data <- filtered_data() %>%
          group_by(month = lubridate::floor_date(order_purchase_date, "month")) %>%
          summarise(metric = sum(payment_value, na.rm = TRUE))
        y_title <- "Total Revenue (₹)"
      } else {
        monthly_data <- filtered_data() %>%
          group_by(month = lubridate::floor_date(order_purchase_date, "month")) %>%
          summarise(metric = n_distinct(order_id))
        y_title <- "Number of Orders"
      }
      plotly::plot_ly(data = monthly_data, x = ~month, y = ~metric, type = 'scatter', mode = 'lines+markers',
                      line = list(color = '#5e4888'), marker = list(color = '#5e4888')) %>%
        plotly::layout(xaxis = list(title = "Month"), yaxis = list(title = y_title), hovermode = "x unified")
    })
  }

  # --- Run App ---
  shinyApp(ui = ui, server = server)
}
