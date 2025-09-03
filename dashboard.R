# Interactive Birth Country Imputation Dashboard
# Run this with: shiny::runApp()

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(stringr)

# Load your data and functions here
# source('your_preprocessing_file.R')
# data_filled_df <- your_final_data

# For demo - replace with your actual data loading
set.seed(123)
demo_data <- data.frame(
  pid = 1:48031,
  birth_city = sample(c("BERLIN", "HAMBURG", "ISTANBUL", "WARSZAWA", "ROMA", "PARIS", "LONDON", NA), 48031, replace = TRUE),
  birth_country = c(rep(NA, 18814), sample(c("000", "152", "163"), 29217, replace = TRUE)),
  imp_birth_country = sample(c("000", "152", "163", "380", "826"), 48031, replace = TRUE),
  imp_name = sample(c("Germany", "Poland", "Turkey", "Italy", "United Kingdom"), 48031, replace = TRUE),
  imp_type = sample(c("given", "german_city", "world_list", "ostgebiete", "country_literal", 
                      "unique_city", "city_o50", "german_base", "citizen_o70", "top_citizenship"), 
                    48031, replace = TRUE, 
                    prob = c(29217, 3200, 1100, 450, 890, 750, 2100, 1800, 950, 8574)),
  citizenship_1 = sample(c("000", "152", "163", "380", "826"), 48031, replace = TRUE),
  citizenship_2 = sample(c("000", "152", "163", "380", "826", NA), 48031, replace = TRUE)
)

# Define the imputation methods in order
imputation_methods <- list(
  "given" = list(name = "Originally Provided", enabled = TRUE, order = 1),
  "german_city" = list(name = "German City List", enabled = TRUE, order = 2),
  "world_list" = list(name = "World Cities Database", enabled = TRUE, order = 3),
  "ostgebiete" = list(name = "Historical Eastern Territories", enabled = TRUE, order = 4),
  "country_literal" = list(name = "Literal Country Names", enabled = TRUE, order = 5),
  "unique_city" = list(name = "Unique City-Country Mapping", enabled = TRUE, order = 6),
  "city_o50" = list(name = "Majority City-Country (>50%)", enabled = TRUE, order = 7),
  "german_base" = list(name = "Base German City Names", enabled = TRUE, order = 8),
  "citizen_o70" = list(name = "Majority Citizenship (>70%)", enabled = TRUE, order = 9),
  "top_citizenship" = list(name = "Fallback to Citizenship", enabled = TRUE, order = 10)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Birth Country Imputation Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Method Toggle", tabName = "methods", icon = icon("toggles")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Quality Assessment", tabName = "quality", icon = icon("check-circle"))
    ),
    
    br(),
    h4("Quick Stats", style = "margin-left: 15px;"),
    div(style = "margin-left: 15px; margin-right: 15px;",
        verbatimTextOutput("quick_stats")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Imputation Performance", status = "primary", solidHeader = TRUE,
                  width = 8, height = 500,
                  plotlyOutput("performance_plot", height = "420px")
                ),
                box(
                  title = "Success Metrics", status = "info", solidHeader = TRUE,
                  width = 4, height = 500,
                  tableOutput("success_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Country Distribution", status = "success", solidHeader = TRUE,
                  width = 12, height = 500,
                  plotlyOutput("country_plot", height = "420px")
                )
              )
      ),
      
      # Method Toggle Tab  
      tabItem(tabName = "methods",
              fluidRow(
                box(
                  title = "Toggle Imputation Methods", status = "warning", solidHeader = TRUE,
                  width = 4, height = 600,
                  p("Select which imputation methods to include in the analysis:"),
                  br(),
                  div(id = "method_toggles",
                      lapply(names(imputation_methods), function(method) {
                        checkboxInput(
                          inputId = paste0("toggle_", method),
                          label = imputation_methods[[method]]$name,
                          value = imputation_methods[[method]]$enabled
                        )
                      })
                  ),
                  br(),
                  actionButton("reset_all", "Reset All", class = "btn-info"),
                  actionButton("select_all", "Select All", class = "btn-success")
                ),
                
                box(
                  title = "Live Results", status = "primary", solidHeader = TRUE,
                  width = 8, height = 600,
                  tabsetPanel(
                    tabPanel("Summary", 
                             br(),
                             tableOutput("method_summary")),
                    tabPanel("Success Rate", 
                             br(),
                             plotlyOutput("live_success_plot", height = "400px")),
                    tabPanel("Countries", 
                             br(),
                             plotlyOutput("live_country_plot", height = "400px"))
                  )
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "Data Explorer", status = "info", solidHeader = TRUE,
                  width = 12,
                  p("Explore the imputation results. Use the filters above each column to investigate patterns."),
                  DT::dataTableOutput("data_table")
                )
              )
      ),
      
      # Quality Assessment Tab
      tabItem(tabName = "quality",
              fluidRow(
                box(
                  title = "Exact Matches Analysis", status = "success", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("exact_matches_plot", height = "300px")
                ),
                box(
                  title = "Citizenship Validation", status = "warning", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("citizenship_plot", height = "300px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Method Accuracy Details", status = "primary", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("accuracy_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data based on selected methods
  filtered_data <- reactive({
    # Get selected methods
    selected_methods <- c()
    for (method in names(imputation_methods)) {
      if (input[[paste0("toggle_", method)]]) {
        selected_methods <- c(selected_methods, method)
      }
    }
    
    # Apply hierarchical imputation logic
    result <- demo_data
    
    # Reset imputation for unselected methods
    result$final_country <- result$birth_country  # Start with original
    result$final_method <- ifelse(is.na(result$birth_country), NA, "given")
    
    # Apply selected methods in order
    for (method in selected_methods) {
      if (method != "given") {
        # Simulate applying this method to remaining NA values
        still_missing <- is.na(result$final_country)
        if (any(still_missing)) {
          # For demo: randomly assign some of the missing values to this method
          method_candidates <- which(still_missing & result$imp_type == method)
          if (length(method_candidates) > 0) {
            result$final_country[method_candidates] <- result$imp_birth_country[method_candidates]
            result$final_method[method_candidates] <- method
          }
        }
      }
    }
    
    result
  })
  
  # Quick stats in sidebar
  output$quick_stats <- renderText({
    data <- filtered_data()
    total <- nrow(data)
    original_missing <- sum(is.na(data$birth_country))
    still_missing <- sum(is.na(data$final_country))
    imputed <- original_missing - still_missing
    success_rate <- round((imputed / original_missing) * 100, 1)
    
    paste0(
      "Total Records: ", formatC(total, format = "d", big.mark = ","), "\n",
      "Originally Missing: ", formatC(original_missing, format = "d", big.mark = ","), "\n",
      "Successfully Imputed: ", formatC(imputed, format = "d", big.mark = ","), "\n",
      "Success Rate: ", success_rate, "%"
    )
  })
  
  # Overview tab plots
  output$performance_plot <- renderPlotly({
    data <- filtered_data()
    
    method_counts <- data %>%
      filter(!is.na(final_method)) %>%
      count(final_method) %>%
      left_join(
        data.frame(
          final_method = names(imputation_methods),
          method_name = sapply(imputation_methods, function(x) x$name)
        ),
        by = "final_method"
      ) %>%
      mutate(method_name = ifelse(is.na(method_name), final_method, method_name))
    
    p <- ggplot(method_counts, aes(x = reorder(method_name, n), y = n, 
                                   text = paste0("Method: ", method_name, 
                                                 "<br>Records: ", formatC(n, format="d", big.mark=",")))) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(title = "Records by Imputation Method",
           x = "Method", y = "Number of Records") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$success_table <- renderTable({
    data <- filtered_data()
    total <- nrow(data)
    original_missing <- sum(is.na(data$birth_country))
    still_missing <- sum(is.na(data$final_country))
    imputed <- original_missing - still_missing
    
    data.frame(
      Metric = c("Total Records", "Originally Missing", "Successfully Imputed", "Still Missing", "Success Rate"),
      Value = c(
        formatC(total, format = "d", big.mark = ","),
        formatC(original_missing, format = "d", big.mark = ","),
        formatC(imputed, format = "d", big.mark = ","),
        formatC(still_missing, format = "d", big.mark = ","),
        paste0(round((imputed / original_missing) * 100, 1), "%")
      )
    )
  }, striped = TRUE)
  
  output$country_plot <- renderPlotly({
    data <- filtered_data()
    
    country_dist <- data %>%
      filter(!is.na(final_country)) %>%
      count(imp_name) %>%
      arrange(desc(n)) %>%
      head(10)
    
    plot_ly(country_dist, x = ~reorder(imp_name, n), y = ~n, type = 'bar',
            text = ~paste0("Country: ", imp_name, "<br>Records: ", formatC(n, format="d", big.mark=",")),
            hovertemplate = "%{text}<extra></extra>") %>%
      layout(title = "Top 10 Birth Countries After Imputation",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Number of Records"))
  })
  
  # Method toggle functionality
  observe({
    if (input$reset_all > 0) {
      for (method in names(imputation_methods)) {
        updateCheckboxInput(session, paste0("toggle_", method), value = FALSE)
      }
    }
  })
  
  observe({
    if (input$select_all > 0) {
      for (method in names(imputation_methods)) {
        updateCheckboxInput(session, paste0("toggle_", method), value = TRUE)
      }
    }
  })
  
  # Live results in method tab
  output$method_summary <- renderTable({
    data <- filtered_data()
    
    selected_count <- sum(sapply(names(imputation_methods), function(m) input[[paste0("toggle_", m)]]))
    
    method_performance <- data %>%
      filter(!is.na(final_method)) %>%
      count(final_method) %>%
      arrange(desc(n))
    
    if (nrow(method_performance) == 0) {
      return(data.frame(Message = "No methods selected"))
    }
    
    method_performance %>%
      mutate(
        Method = final_method,
        Records = formatC(n, format = "d", big.mark = ","),
        Percentage = paste0(round(n/sum(n)*100, 1), "%")
      ) %>%
      select(Method, Records, Percentage)
  })
  
  output$live_success_plot <- renderPlotly({
    data <- filtered_data()
    original_missing <- sum(is.na(data$birth_country))
    imputed_by_method <- data %>%
      filter(!is.na(final_method), is.na(birth_country)) %>%
      count(final_method) %>%
      mutate(success_rate = round(n/original_missing*100, 2))
    
    if (nrow(imputed_by_method) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(imputed_by_method, aes(x = reorder(final_method, success_rate), y = success_rate)) +
      geom_col(fill = "darkgreen", alpha = 0.7) +
      coord_flip() +
      labs(title = "Success Rate by Method", x = "Method", y = "Success Rate (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$live_country_plot <- renderPlotly({
    data <- filtered_data()
    
    if (sum(!is.na(data$final_country)) == 0) {
      return(plotly_empty())
    }
    
    country_pie <- data %>%
      filter(!is.na(final_country)) %>%
      count(imp_name) %>%
      arrange(desc(n)) %>%
      head(8)
    
    plot_ly(country_pie, labels = ~imp_name, values = ~n, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent') %>%
      layout(title = "Country Distribution")
  })
  
  # Data Explorer
  output$data_table <- DT::renderDataTable({
    data <- filtered_data() %>%
      select(ID = pid, `Birth City` = birth_city, `Original Country` = birth_country,
             `Final Country` = final_country, `Country Name` = imp_name, 
             `Method Used` = final_method) %>%
      head(5000)  # Limit for performance
    
    datatable(data, 
              filter = 'top',
              options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # Quality Assessment
  output$exact_matches_plot <- renderPlotly({
    # Simulate exact match analysis
    exact_matches <- data.frame(
      Method = c("German City", "World Cities", "Literal Country", "Unique City", "Other"),
      ExactMatches = c(85, 72, 95, 88, 65),
      TotalAttempts = c(100, 100, 100, 100, 100)
    ) %>%
      mutate(SuccessRate = ExactMatches/TotalAttempts*100)
    
    p <- ggplot(exact_matches, aes(x = reorder(Method, SuccessRate), y = SuccessRate)) +
      geom_col(fill = "orange", alpha = 0.7) +
      coord_flip() +
      labs(title = "Exact Match Success Rate by Method", 
           x = "Method", y = "Success Rate (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$citizenship_plot <- renderPlotly({
    # Simulate citizenship validation
    citizenship_validation <- data.frame(
      Category = c("Matches Primary", "Matches Secondary", "Germany/German", "No Match", "Missing Data"),
      Count = c(2500, 800, 15000, 1200, 300),
      Percentage = c(26.0, 8.3, 46.2, 12.5, 3.1)
    )
    
    plot_ly(citizenship_validation, x = ~Category, y = ~Count, type = 'bar',
            text = ~paste0("Count: ", formatC(Count, big.mark=","), "<br>Percentage: ", Percentage, "%"),
            hovertemplate = "%{text}<extra></extra>") %>%
      layout(title = "Citizenship Validation Results",
             xaxis = list(title = "Validation Category"),
             yaxis = list(title = "Number of Records"))
  })
  
  output$accuracy_table <- DT::renderDataTable({
    # Simulate detailed accuracy metrics
    accuracy_data <- data.frame(
      Method = names(imputation_methods)[-1],  # Exclude 'given'
      `Records Processed` = c(3200, 1100, 450, 890, 750, 2100, 1800, 950, 8574),
      `Exact Matches` = c(2720, 792, 427, 845, 660, 1890, 1440, 665, 6002),
      `Success Rate` = c(85.0, 72.0, 94.9, 94.9, 88.0, 90.0, 80.0, 70.0, 70.0),
      `Avg Confidence` = c(0.95, 0.78, 0.99, 0.88, 0.92, 0.85, 0.72, 0.65, 0.60)
    )
    
    datatable(accuracy_data,
              options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatPercentage(c('Success Rate'), 1) %>%
      formatRound(c('Avg Confidence'), 2)
  })
}

# Run the app
shinyApp(ui = ui, server = server)