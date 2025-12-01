library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Histogram with Density"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mu", 
                   "Mean (μ):", 
                   value = 10, 
                   min = -50, 
                   max = 50, 
                   step = 0.5),
      
      numericInput("sd", 
                   "Standard Deviation (σ):", 
                   value = 5, 
                   min = 0.1, 
                   max = 20, 
                   step = 0.1),
      
      numericInput("n_samples",
                   "Number of samples:",
                   value = 1000,
                   min = 100,
                   max = 10000,
                   step = 100),
      
      numericInput("bins",
                   "Number of bins:",
                   value = 30,
                   min = 10,
                   max = 100,
                   step = 5)
    ),
    
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$histogram <- renderPlot({
    # Generate random data with user-specified parameters
    y <- rnorm(n = input$n_samples, mean = input$mu, sd = input$sd)
    
    # Create histogram with density overlay
    hist(y, freq = FALSE, breaks = input$bins,
         main = paste("Interactive Histogram with Density\nNormal Distribution (μ =", input$mu, ", σ =", input$sd, ")"),
         xlab = "Values", ylab = "Density",
         col = "lightblue", border = "white")
    
    # Add density curve for the actual data
    lines(density(y), col = "red", lwd = 2)
    
    # Add theoretical normal density curve
    x_range <- seq(min(y), max(y), length.out = 100)
    theoretical_density <- dnorm(x_range, mean = input$mu, sd = input$sd)
    lines(x_range, theoretical_density, col = "blue", lwd = 2, lty = 2)
    
    # Add legend
    legend("topright", 
           legend = c("Histogram", "Sample Density", "Theoretical Density"), 
           fill = c("lightblue", NA, NA), 
           border = c("white", NA, NA),
           lty = c(NA, 1, 2), 
           col = c(NA, "red", "blue"), 
           lwd = c(NA, 2, 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)