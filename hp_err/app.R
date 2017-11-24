library(shiny)
library(tidyverse)
library(DT)

ui <- fluidPage(
   
   titlePanel("Visualize Type I/II errors: One-sample Test of Means (Z test)"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("mu_0", "Null hypothesis mean :",
                     min = 150, max = 250, value = 200),
         sliderInput("mu_1", "Alternative hypothesis mean:",
                     min = 150, max = 250, value = 220),
         sliderInput("n", "Sample size:",
                     min = 10, max = 100, value = 25),
         sliderInput("sd", "Standard deviation of X:",
                     min = 25, max = 75, value = 50),
         sliderInput("alpha", "Choose alpha level :",
                     min = 0.01, max = 0.15, value = .05, step = .01),
         radioButtons("side", "Choose tail of the test",
                      c("One Tail, Upper Tail" = "greater",
                        "One Tail, Lower Tail" = "less",
                        "Two Tail" = "two.sided"), "greater")
      ),
      
      mainPanel(
         plotOutput("plot_d"),
         dataTableOutput("error")
      )
   )
)

server <- function(input, output) {
  
  dt <- reactive({
    
    xmin = min(input$mu_0 - 3*input$sd/sqrt(input$n), input$mu_1 - 3*input$sd/sqrt(input$n))
    xmax = max(input$mu_0 + 3*input$sd/sqrt(input$n), input$mu_1 + 3*input$sd/sqrt(input$n))
    
    data_frame(
      x = seq(xmin, xmax, length.out = 1000),
      null_d = dnorm(x, input$mu_0, input$sd/sqrt(input$n)),
      alter_d = dnorm(x, input$mu_1, input$sd/sqrt(input$n))
    )
  })
  
  output$error <- renderDataTable({
    
    xbar <- if (input$side == "two.sided"){
      qnorm(c(input$alpha/2, 1-input$alpha/2), input$mu_0, input$sd/sqrt(input$n))
    } else if (input$side == "greater") {
      qnorm(1-input$alpha, input$mu_0, input$sd/sqrt(input$n))
    } else {
      qnorm(input$alpha, input$mu_0, input$sd/sqrt(input$n))
    }
    beta <- if (input$side == "two.sided"){
      pnorm(xbar[2], input$mu_1, input$sd/sqrt(input$n)) - 
        pnorm(xbar[1], input$mu_1, input$sd/sqrt(input$n))
    } else if (input$side == "greater") {
      pnorm(xbar, input$mu_1, input$sd/sqrt(input$n))
    } else {
      pnorm(xbar, input$mu_1, input$sd/sqrt(input$n), lower.tail = FALSE)
    }
    
    err <- data_frame(
      `sample size` = input$n,
      `standard deviation` = input$sd,
      alpha = input$alpha,
      beta = beta,
      power = 1 - beta)
    if (input$side == "two.sided"){
      err <- mutate(err, xbar1 = xbar[1], xbar2 = xbar[2])
    } else {
      err <- mutate(err, xbar1 = xbar)
    }

    round(err, 3)    
  })
  
  #labl <- list(expression(paste(bar(x) %~% N, bgroup("(", paste(mu[1], ", ", frac(sigma, sqrt(n))) ,")"))), 
  #             expression(paste(bar(x) %~% N, bgroup("(", paste(mu[0], ", ", frac(sigma, sqrt(n))) ,")")))) 
  labl <- list(expression(bar(X) %~% N (mu[1], frac(sigma, sqrt(n)))), 
               expression(bar(X) %~% N (mu[0], frac(sigma, sqrt(n))))) 
  
  output$plot_d <- renderPlot({
    p <- ggplot(dt(), aes(x, null_d)) +
      geom_line(aes(col = "Null hypothesis (H0)")) +
      geom_line(aes(y = alter_d, col = "Alternative hypothesis (H1)")) +
      labs(x = "X", y = "density", col = "Distribution:") +
      scale_color_manual(values = c("blue", "red"), labels = labl) +
      theme_classic()
    
    if (input$side == "two.sided"){
      p <- p + 
        geom_vline(xintercept = qnorm(c(input$alpha/2, 1-input$alpha/2), input$mu_0, input$sd/sqrt(input$n)), 
                   linetype = "dashed") +
        geom_area(data = subset(dt(), x >= qnorm(1-input$alpha/2, input$mu_0, input$sd/sqrt(input$n))),
                  fill = "red", alpha = .5) +
        geom_area(data = subset(dt(), x <= qnorm(input$alpha/2, input$mu_0, input$sd/sqrt(input$n))),
                  fill = "red", alpha = .5) +
        geom_area(data = subset(dt(), x < qnorm(1-input$alpha/2, input$mu_0, input$sd/sqrt(input$n)) &
                                  x > qnorm(input$alpha/2, input$mu_0, input$sd/sqrt(input$n))),
                  aes(y = alter_d), fill = "lightblue", alpha = .5)
      
    }
    if (input$side == "greater"){
      p <- p + 
        geom_vline(xintercept = qnorm(1-input$alpha, input$mu_0, input$sd/sqrt(input$n)), 
                   linetype = "dashed") +
        geom_area(data = subset(dt(), x >= qnorm(1-input$alpha, input$mu_0, input$sd/sqrt(input$n))),
                  fill = "red", alpha = .5) +
        geom_area(data = subset(dt(), x < qnorm(1-input$alpha, input$mu_0, input$sd/sqrt(input$n))),
                  aes(y = alter_d), fill = "lightblue", alpha = .5)
      
    }
    if (input$side == "less"){
      p <- p + 
        geom_vline(xintercept = qnorm(input$alpha, input$mu_0, input$sd/sqrt(input$n)), 
                   linetype = "dashed") +
        geom_area(data = subset(dt(), x <= qnorm(input$alpha, input$mu_0, input$sd/sqrt(input$n))),
                  fill = "red", alpha = .5) +
        geom_area(data = subset(dt(), x > qnorm(input$alpha, input$mu_0, input$sd/sqrt(input$n))),
                  aes(y = alter_d), fill = "lightblue", alpha = .5)
      
    }
    
    p
    })
   
}

shinyApp(ui = ui, server = server)

