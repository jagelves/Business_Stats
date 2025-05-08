library(shiny)
library(ggplot2)
library(ggthemes)

# Generate fictional population data for male weights (in kg)
set.seed(123)
population_weights <- rnorm(10000, mean = 80, sd = 10)

# UI
ui <- fluidPage(
  titlePanel("Sampling Male Weights From a Population of 10,000"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sample_size", "Select Sample Size:", 
                  choices = c(10, 100, 200, 500, 1000, 5000,10000), 
                  selected = 10),
      actionButton("sample_btn", "Take Sample")
    ),
    mainPanel(
      tableOutput("stats_table"),
      h4(""),
      plotOutput("sample_hist")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive value to store sample
  sample_data <- reactiveVal()
  
  # Update sample when button is clicked
  observeEvent(input$sample_btn, {
    sample_data(sample(population_weights, size = as.numeric(input$sample_size)))
  })
  
  # Calculate and display statistics
  output$stats_table <- renderTable({
    req(sample_data())
    sample_mean <- mean(sample_data())
    sample_sd <- sd(sample_data())
    pop_mean <- mean(population_weights)
    pop_sd <- sd(population_weights)
    
    data.frame(
      Statistic = c("Mean", "Standard Deviation"),
      Sample = c(round(sample_mean, 2), round(sample_sd, 2)),
      Population = c(round(pop_mean, 2), round(pop_sd, 2)),
      Error = c(round(sample_mean - pop_mean, 2), round(sample_sd - pop_sd, 2))
    )
  })
  
  # Plot sample histogram
  output$sample_hist <- renderPlot({
    req(sample_data())
    ggplot(data.frame(weights = sample_data()), aes(x = weights)) +
      geom_histogram(binwidth =  (max(sample_data())-min(sample_data()))/(1 + 3.322 * log10(length(sample_data()))),
                     bins= 1 + 3.322 * log10(length(sample_data())),
                     fill = "#088F8F", color = "black", alpha=0.3) +
      geom_vline(aes(xintercept = mean(sample_data())), color = "red", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = mean(population_weights)), color = "blue", linetype = "solid", size = 1) +
      labs(title = "Histogram of Sample Weights",
           x = "Weight (kg)", y = "Frequency") +
      theme_minimal() +
      annotate("text", x = mean(sample_data()) + 5, y = Inf, 
               label = "Sample Mean", color = "red", vjust = 1) +
      annotate("text", x = mean(population_weights) + 5, y = Inf, 
               label = "Population Mean", color = "blue", vjust = 2) +
      theme_clean()
  })
}

# Run the app
shinyApp(ui = ui, server = server)