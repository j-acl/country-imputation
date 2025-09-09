# Interactive Birth Country Imputation Dashboard

library(shiny)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(memoise)
library(RColorBrewer)

# Load data
data_filled_df <- read.csv("data_filled_df.csv", stringsAsFactors = FALSE)

# Filter missing data
missing_data <- data_filled_df %>% filter(imp_type != "given")
missing_data$imp_type <- as.character(missing_data$imp_type)

# Imputation methods
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
  "top_citizenship" = list(name = "Fallback to Citizenship", enabled = TRUE, order = 10),
  "history" = list(name = "German exonym", enabled = TRUE, order = 11)
)

# Allowed countries
allowed_countries <- unique(missing_data$imp_name)

# Memoised city matrix
getCityMatrix <- memoise(function(country) {
  df <- missing_data %>%
    filter(imp_name == country, !is.na(birth_city), birth_city != "")
  
  if (nrow(df) == 0) return(NULL)
  
  myCorpus <- Corpus(VectorSource(df$birth_city))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("en"))
  
  m <- as.matrix(TermDocumentMatrix(myCorpus))
  freqs <- sort(rowSums(m), decreasing = TRUE)
  head(freqs, 50)  # top 50 cities only
})

# UI
ui <- fluidPage(
  # Main title
  

  
  titlePanel("Birth Country Imputation Dashboard"),
  # Author
  h4("Jamie Christy & Danaia Burtseva"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country_bar", "Select Country for Missing Data:",
                  choices = allowed_countries),
      hr(),
      selectInput("country_word", "Select Country for City Wordcloud:",
                  choices = allowed_countries)
    ),
    
    mainPanel(
      plotOutput("missingPlot", height = "400px"),
      hr(),
      plotOutput("cityWordcloud", height = "600px")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Missing data bar chart
  output$missingPlot <- renderPlot({
    allowed_types <- names(imputation_methods)
    
    df <- missing_data %>%
      filter(imp_name == input$country_bar, imp_type %in% allowed_types) %>%
      count(imp_type)
    
    if (nrow(df) == 0) df <- data.frame(imp_type = "None", n = 0)
    
    df$imp_type <- factor(df$imp_type, levels = allowed_types)
    
    ggplot(df, aes(x = imp_type, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Missing Data for", input$country_bar),
           x = "Imputation Type", y = "Number of observations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # City wordcloud
  output$cityWordcloud <- renderPlot({
    freqs <- getCityMatrix(input$country_word)
    
    if (is.null(freqs) || length(freqs) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for this country")
    } else {
      wordcloud(names(freqs), freqs, scale = c(3, 0.3),
                max.words = 50,
                colors = brewer.pal(8, "Dark2"))
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)
