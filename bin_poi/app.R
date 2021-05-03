library(tidyverse)
library(shiny)

ui <- pageWithSidebar(
  headerPanel('Comparison between Binomial and Poisson distribution'),
  sidebarPanel(
    sliderInput("n", label = "Number of trials (n)", min = 10, 
                max = 100, value = 50, step = 5),
    sliderInput("p", label = "Probability of success (n)", min = 0, 
                max = 1, value = .1),
    sliderInput("lambda", label = "Expected number of success (lambda)", 
                min = 0, max = 11, value = 5),
    p("Given a Binomial distribution with some n and p, if you let n goes to infinite and 
      p tends to 0 in such a way that np tends λ, then the Binomial distribution 
      approaches a Poisson distribution with parameter λ.")
  ),
  mainPanel(
    plotOutput('dist_plot'),
    br(),
    sliderInput("x", label = "Choose value (x)", 
                min = 0, max = 9, value = 5),
    checkboxInput("show", "Show probability on graph", value = FALSE),
    tableOutput('prob')
  )
)
server <- function(input, output, session){
  
  v <- reactiveValues(x = NULL)
  
  observeEvent(input$p, {
    v$x <- input$p
  })
  observeEvent(input$lambda, {
    v$x <- input$lambda
  }, priority = 10)
  
  observeEvent(v$x, {
    if (v$x != input$p) {
      updateSliderInput(session, "p", value = min(1, v$x/input$n))
    }
    if (v$x != input$lambda){
      updateSliderInput(session, "lambda",
                        min = max(0, floor(input$n*v$x - 3*sqrt(input$n*v$x))),
                        max = floor(input$n*v$x + 3*sqrt(input$n*v$x)),
                        value = input$n*v$x)
    }
    updateSliderInput(session, "x",
                      min = max(0, floor(input$n*input$p - 2*sqrt(input$n*input$p))),
                      max = floor(input$n*input$p +  2*sqrt(input$n*input$p)),
                      value = input$n*input$p)
  })
  
  observeEvent(input$n, {
    updateSliderInput(session, "lambda",
                      min = max(0, floor(input$n*input$p - 3*sqrt(input$n*input$p))),
                      max = floor(input$n*input$p+ 3*sqrt(input$n*input$p)),
                      value = input$n*input$p)
  })
  

  dat <- reactive({
    lambda <- input$n*input$p
    data_dist <- data_frame(
      x = seq(max(0, floor(lambda - 3*sqrt(lambda))), floor(lambda + 3*sqrt(lambda))),
      binomial = dbinom(x, size = input$n, prob = input$p),
      poisson = dpois(x, lambda = lambda)
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

