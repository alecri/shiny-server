library(tidyverse)
library(shiny)

ui <- pageWithSidebar(
  headerPanel('Comparison between Binomial and Poisson distribution'),
  sidebarPanel(
    sliderInput("n", label = "Number of trials (n)", min = 100, 
                max = 1000, value = 500),
    sliderInput("p", label = "Probability of success (n)", min = 0, 
                max = 1, value = .1),
    sliderInput("lambda", label = "Expected number of success (lambda)", 
                min = 0, 
                max = 100, value = 50)
  ),
  mainPanel(
    plotOutput('dist_plot')
  )
)
server <- function(input, output, session) {
  
  observeEvent(input$n,
               updateSliderInput(session, "lambda",
                                 max = floor(input$n*input$p + 3*input$n*input$p),
                                 value = input$n*input$p)
  )
  observeEvent(input$p,
               updateSliderInput(session, "lambda",
                                 max = floor(input$n*input$p + 3*input$n*input$p),
                                 value = input$n*input$p)
  )
  observeEvent(input$lambda,
               updateSliderInput(session, "p",
                                 value = input$lambda/input$n)
  )
  
  
  dat <- reactive({
    lambda <- input$n*input$p
    data_dist <- data_frame(
      x = seq(floor(lambda - 3*sqrt(lambda)), floor(lambda + 3*sqrt(lambda))),
      d_bin = dbinom(x, size = input$n, prob = input$p),
      d_poi = dpois(x, lambda = lambda)
    )
    data_dist
  })
  
  output$dist_plot <- renderPlot({
    ggplot(dat(), aes(x, d_bin)) +
      geom_line(aes(col = "Binomial")) +
      geom_line(aes(y = d_poi, col = "Poisson")) +
      ylab("density") +
      theme_classic()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

