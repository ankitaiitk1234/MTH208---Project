library(shiny)

library(DT)
library(tidyverse)
library(ggplot2)

# Load the data
data <- read.csv("C:/Users/USER/OneDrive/Desktop/DSProject/data.csv")
data$Tomatometer <- as.numeric(data$Tomatometer)
data$Popcornmeter <- as.numeric(data$Popcornmeter)
data$Seasons <- as.numeric(data$Seasons)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap');
      body {
        font-family: 'Poppins', sans-serif;
        background-color: #e8f1f2;
        color: #333333;
      }
      h2, h3 {
        color: #3498db;
        font-weight: 600;
      }
      .shiny-input-container {
        margin-bottom: 20px;
      }
      .dt-button {
        background-color: #3498db;
        color: white;
        font-weight: bold;
        border: none;
        padding: 10px 20px;
        border-radius: 8px;
        transition: background-color 0.3s ease;
      }
      .dt-button:hover {
        background-color: #2980b9;
      }
      .form-control, .selectize-input {
        border-radius: 10px;
        border-color: #c1c1c1;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      .well {
        background-color: #ffffff;
        border: 1px solid #e0e0e0;
        border-radius: 12px;
        padding: 30px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .sidebar-panel h4 {
        color: #3498db;
        font-weight: 600;
      }
      .sidebarPanel {
        background-color: #ffffff;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      }
      .mainPanel {
        padding: 30px;
        background-color: #ffffff;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      }
      .tab-content {
        padding-top: 20px;
      }
    "))
  ),
  
  titlePanel(
    h2("Rotten Tomatoes Netflix Web Series Analysis")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "well",
      
      conditionalPanel(
        condition = "input.tabs != 'Genre Comparison' && input.tabs != 'Score Density'",
        h4("Filter Options"),
        selectInput("genre", "Select Genre:", choices = c("All", unique(data$Genre)), selected = "All"),
        sliderInput("tomatometer", "Minimum Tomatometer Score (%):", min = 0, max = 100, value = 50),
        sliderInput("popcornmeter", "Minimum Audience Score (%):", min = 0, max = 100, value = 50),
        sliderInput("seasons", "Minimum Seasons:", min = 1, max = max(data$Seasons, na.rm = TRUE), value = 1),
        
        h4("Detailed Genre Distribution"),
        plotOutput("detailedGenrePlot", height = "300px") 
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Genre Comparison'",
        h4("Genre Comparison Filters"),
        selectInput("genre1", "Select First Genre:", choices = c("All", unique(data$Genre)), selected = "All"),
        selectInput("genre2", "Select Second Genre:", choices = c("All", unique(data$Genre)), selected = "All")
      )
    ),
    
    mainPanel(
      class = "mainPanel",
      
      tabsetPanel(id = "tabs",
                  tabPanel("Data Table", DTOutput("webseriesTable")),
                  tabPanel("Score Comparison", plotOutput("scorePlot")),
                  
                  # New tab for genre comparison
                  tabPanel("Genre Comparison",
                           h3("Comparison Between Two Genres"),
                           DTOutput("comparisonTable"),
                           plotOutput("comparisonGenrePlot"),
                           plotOutput("comparisonScorePlot")
                  ),
                  
                  tabPanel("Score Density",
                           h3("Density of Tomatometer and Audience Scores by Genre"),
                           plotOutput("scoreDensityPlot")
                  )
      )
    )
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data %>%
      filter((input$genre == "All" | Genre == input$genre) & 
               Tomatometer >= input$tomatometer & 
               Popcornmeter >= input$popcornmeter & 
               Seasons >= input$seasons)
  })
  
  
  filtered_comparison_data <- reactive({
    req(input$genre1, input$genre2)  # Ensure genres are selected
    data %>%
      filter((input$genre1 == "All" | Genre == input$genre1) | 
               (input$genre2 == "All" | Genre == input$genre2))
  })
  
  
  output$webseriesTable <- renderDT({
    datatable(filtered_data(), 
              options = list(pageLength = 10, autoWidth = TRUE), 
              rownames = FALSE,
              class = "stripe cell-border hover") 
  })
  
  
  output$detailedGenrePlot <- renderPlot({
    ggplot(data, aes(x = Genre, fill = Genre)) +
      geom_bar(color = "black") +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Detailed Genre Distribution", x = "Genre", y = "Count") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(color = "#3498db", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
        legend.position = "none",
        panel.background = element_rect(fill = "#e8f1f2")
      )
  })
  
  
  output$scorePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Tomatometer, y = Popcornmeter, color = Genre)) +
      geom_point(size = 4, alpha = 0.8) +
      scale_color_brewer(palette = "Dark2") +
      labs(title = "Tomatometer vs Audience Score", x = "Tomatometer Score (%)", y = "Audience Score (%)") +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(color = "#3498db", face = "bold"),
        panel.background = element_rect(fill = "#e8f1f2")
      )
  })
  
  output$comparisonTable <- renderDT({
    datatable(filtered_comparison_data(), 
              options = list(pageLength = 10, autoWidth = TRUE), 
              rownames = FALSE,
              class = "stripe cell-border hover")
  })
  
  
  output$comparisonGenrePlot <- renderPlot({
    ggplot(filtered_comparison_data(), aes(x = Genre, fill = Genre)) +
      geom_bar(color = "black", position = "dodge") +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Distribution of Selected Genres", x = "Genre", y = "Count") +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(color = "#3498db", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
        panel.background = element_rect(fill = "#e8f1f2")
      )
  })
  
  
  output$comparisonScorePlot <- renderPlot({
    ggplot(filtered_comparison_data(), aes(x = Tomatometer, y = Popcornmeter, color = Genre)) +
      geom_point(size = 4, alpha = 0.8) +
      scale_color_brewer(palette = "Dark2") +
      labs(title = "Tomatometer vs Audience Score by Genre", x = "Tomatometer Score (%)", y = "Audience Score (%)") +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(color = "#3498db", face = "bold"),
        panel.background = element_rect(fill = "#e8f1f2")
      )
  })
  
  output$scoreDensityPlot <- renderPlot({
    ggplot(data, aes(x = Tomatometer, fill = Genre)) +
      geom_density(alpha = 0.6) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Density of Tomatometer Scores by Genre", x = "Tomatometer Score (%)", y = "Density") +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(color = "#3498db", face = "bold"),
        panel.background = element_rect(fill = "#e8f1f2")
      )
  })
}


shinyApp(ui = ui, server = server)
