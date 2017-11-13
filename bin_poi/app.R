library(tidyverse)
library(shiny)
library(DT)
library(htmlTable)
library(htmlwidgets)

ui <- pageWithSidebar(
  headerPanel('Comparison between Binomial and Poisson distribution'),
  sidebarPanel(
    sliderInput("n", label = "Number of trials (n)", min = 100, 
                max = 1000, value = 500),
    sliderInput("p", label = "Probability of success (n)", min = 0, 
                max = 1, value = .1),
    sliderInput("lambda", label = "Expected number of success (lambda)", 
                min = 0, max = 100, value = 50),
    sliderInput("x", label = "Choose value", 
                min = 0, max = 100, value = 50)
  ),
  mainPanel(
    plotOutput('dist_plot'),
    br(),
    checkboxInput("show", "Show probability on graph", value = FALSE),
    tableOutput('prob')
  )
)
server <- function(input, output, session){
  observeEvent(input$n, {
    updateSliderInput(session, "lambda",
                      min = max(0,floor(input$n*input$p - 3*sqrt(input$n*input$p))),
                      max = floor(input$n*input$p + 3*sqrt(input$n*input$p)),
                      value = input$n*input$p)
    updateSliderInput(session, "x",
                      min = max(0,floor(input$n*input$p - 2*sqrt(input$n*input$p))),
                      max = floor(input$n*input$p +  2*sqrt(input$n*input$p)),
                      value = input$n*input$p)
  })
  observeEvent(input$p, {
    updateSliderInput(session, "lambda",
                      min = max(0,floor(input$n*input$p - 3*sqrt(input$n*input$p))),
                      max = floor(input$n*input$p + 3*sqrt(input$n*input$p)),
                      value = input$n*input$p)
    updateSliderInput(session, "x",
                      min = max(0,floor(input$n*input$p - 2*sqrt(input$n*input$p))),
                      max = floor(input$n*input$p + 2*sqrt(input$n*input$p)),
                      value = input$n*input$p)
  })
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
    ) %>% gather(dist, p, -x)
    data_dist
  })
  
  output$dist_plot <- renderPlot({
    dist_plot <- ggplot(dat(), aes(x, p, col = dist)) +
      geom_line() +
      labs(y = "density", col = "distribution") + 
      theme_classic()
    if (input$show)
      dist_plot <- dist_plot + geom_area(data = subset(dat(), x <= input$x), aes(fill = dist), 
                                         position = "dodge", alpha = .5) +
        guides(col = FALSE)
    dist_plot
  })
  
  output$prob <- renderTable({
    prob <- matrix(c(dbinom(input$x, input$n, input$p), dpois(input$x, input$lambda),
                    pbinom(input$x, input$n, input$p), ppois(input$x, input$lambda),
                    pbinom(input$x, input$n, input$p, lower.tail = F), 
                    ppois(input$x, input$lambda, lower.tail = F)),
                  ncol = 2, nrow = 3, byrow = T)
    dimnames(prob) <- list(c("P(X = x)", "P(X < x)", "P(X > x)"), c("Binomial", "Poisson"))
    prob
  }, rownames = T)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

