library(shiny)
library(tidyverse)
library(cowplot)
library(knitr)

## load all files
files <- list.files(path = "www", pattern = '*.Rdata')
lapply(files, function(x) load(paste0("www/", x), envir = globalenv()))
load("www/param_4.5.RData")

ui <- fluidPage(
  
  titlePanel("Exemplification of a simulation study for ProBio"),
  hr(),
  selectInput("scen", h3("Select scenario"), 
              choices = list("Scenario 1" = "sim_scen1", "Scenario 2" = "sim_scen2",
                             "Scenario 3" = "sim_scen3", "Scenario 4" = "sim_scen4"), 
              selected = "sim_scen1"),
  tabsetPanel(
    tabPanel("Report",
             htmlOutput("frame")
    ),
    tabPanel("Indices",
             sliderInput("month", "Select follow-up month",
                         min = 1, max = 35, value = 1),
             
             tabsetPanel(
               tabPanel("Summary",
                        fluidRow(
                          #column(4, uiOutput("month_b")),
                          column(5, uiOutput("month")),
                          column(5, uiOutput("month_a"))
                        ),
                        br(),
                        h3("Randomization probabilities", align = "center"),
                        fluidRow(
                          #column(4, uiOutput('r_b'), style = "border-right: 1px solid"),
                          column(5, uiOutput('r'), style = "border-right: 1px solid"),
                          # column(5,  div(id = "r-container",
                          #                tags$img(src = "spinner.gif",
                          #                         id = "loading-spinner"),
                          #                uiOutput('r'))),
                          column(5, uiOutput('r_a'), style = "border-right: 1px solid")
                        ),
                        br(),
                        h3("Randomized participants (subtypes)", align = "center"),
                        fluidRow(
                          #column(4, uiOutput('n_tb'), style = "border-right: 1px solid"),
                          column(5, uiOutput('n_t'), style = "border-right: 1px solid"),
                          column(5, uiOutput('n_ta'), style = "border-right: 1px solid")
                        ),
                        br(),
                        h3("Randomized participants (signatures)", align = "center"),
                        fluidRow(
                          #column(4, uiOutput('n_sb'), style = "border-right: 1px solid"),
                          column(5, uiOutput('n_s'), style = "border-right: 1px solid"),
                          column(5, uiOutput('n_sa'), style = "border-right: 1px solid")
                        ),
                        br(),
                        h3("Number of progressions (subtype)", align = "center"),
                        fluidRow(
                          #column(4, uiOutput('delta_tb'), style = "border-right: 1px solid"),
                          column(5, uiOutput('delta_t'), style = "border-right: 1px solid"),
                          column(5, uiOutput('delta_ta'), style = "border-right: 1px solid")
                        ),
                        br(),
                        h3("Accumulated person-month (subtype)", align = "center"),
                        fluidRow(
                          #column(4, uiOutput('time_tb'), style = "border-right: 1px solid"),
                          column(5, uiOutput('time_t'), style = "border-right: 1px solid"),
                          column(5, uiOutput('time_ta'), style = "border-right: 1px solid")
                        ),
                        br(),
                        h3("Probability of superiority (subtype)", align = "center"),
                        fluidRow(
                          #column(4, uiOutput('p_tb'), style = "border-right: 1px solid"),
                          column(5, uiOutput('p_t'), style = "border-right: 1px solid"),
                          column(5, uiOutput('p_ta'), style = "border-right: 1px solid")
                        ),
                        br(),
                        h3("Probability of superiority (signature)", align = "center"),
                        fluidRow(
                          #column(4, uiOutput('p_sb'), style = "border-right: 1px solid"),
                          column(5, 
                                 uiOutput('p_s'),
                                 plotOutput("density_s", height = "600px"),
                                 style = "border-right: 1px solid"),
                          column(5, uiOutput('p_sa'),
                                 plotOutput("density_sa", height = "600px"),
                                 style = "border-right: 1px solid")
                        )
               ),
               tabPanel("Data",
                        uiOutput("dat_title"),
                        dataTableOutput("dat_m")
               )
             )
    ),
    tabPanel("Stat",
             tags$iframe(src="model_exemplification.pdf", width="900", height="1000")
    )
  )
)

server <- function(input, output, session) {
  
  output$frame <- renderUI({
    tags$iframe(src = paste0("./", input$scen, ".html"), # put myMarkdown.html to /www
                width = '100%', height = '800px', 
                frameborder = 0, scrolling = 'auto'
    )
  })
  dat <- reactive({
    # if (input$scen == "sim_scen1"){
    #   sim_scen1
    # }
    eval(as.name(input$scen))
  })
  
  output$month <- renderUI(
    h3(paste("Selected Month:", input$month), align = "center")
  )
  output$month_a <- renderUI(
    h3(paste("Month after:", input$month + 1), align = "center")
  )
  
  output$r <- renderTable(
    dat()$results_inter[[input$month]]$post$r, spacing = "xs", digits = 2, rownames = TRUE
  )
  output$r_a <- renderTable(
    dat()$results_inter[[input$month + 1]]$post$r, spacing = "xs", digits = 2, rownames = TRUE
  )
  output$n_t <- renderTable(
    dat()$results_inter[[input$month]]$dta$n$n_type, spacing = "xs", digits = 0, rownames = TRUE
  )
  output$n_ta <- renderTable(
    dat()$results_inter[[input$month + 1]]$dta$n$n_type, spacing = "xs", digits = 0, rownames = TRUE
  )
  output$n_s <- renderTable(
    dat()$results_inter[[input$month]]$dta$n$n_sign, spacing = "xs", digits = 0, rownames = TRUE
  )
  output$n_sa <- renderTable(
    dat()$results_inter[[input$month + 1]]$dta$n$n_sign, spacing = "xs", digits = 0, rownames = TRUE
  )
  delta_t <- reactive({
    delta <- dat()$results_inter[[input$month]]$dta$delta
    dimnames(delta) <- dimnames(scheme_type)
    delta
  })
  delta_ta <- reactive({
    delta <- dat()$results_inter[[input$month + 1]]$dta$delta
    dimnames(delta) <- dimnames(scheme_type)
    delta
  })
  output$delta_t <- renderTable(
    delta_t(), spacing = "xs", digits = 0, rownames = TRUE
  )
  output$delta_ta <- renderTable(
    delta_ta(), spacing = "xs", digits = 0, rownames = TRUE
  )
  time_t <- reactive({
    time_t <- dat()$results_inter[[input$month]]$dta$PT
    dimnames(time_t) <- dimnames(scheme_type)
    time_t
  })
  time_ta <- reactive({
    time_t <- dat()$results_inter[[input$month + 1]]$dta$PT
    dimnames(time_t) <- dimnames(scheme_type)
    time_t
  })
  output$time_t <- renderTable(
    time_t(), spacing = "xs", digits = 1, rownames = TRUE
  )
  output$time_ta <- renderTable(
    time_ta(), spacing = "xs", digits = 1, rownames = TRUE
  )
  output$p_t <- renderTable(
    dat()$results_inter[[input$month]]$post$p_type, spacing = "xs", digits = 2, rownames = TRUE
  )
  output$p_ta <- renderTable(
    dat()$results_inter[[input$month + 1]]$post$p_type, spacing = "xs", digits = 2, rownames = TRUE
  )
  output$p_s <- renderTable(
    dat()$results_inter[[input$month]]$post$p_sign, spacing = "xs", digits = 2, rownames = TRUE
  )
  output$p_sa <- renderTable(
    dat()$results_inter[[input$month + 1]]$post$p_sign, spacing = "xs", digits = 2, rownames = TRUE
  )


  output$density_s <- renderPlot({
    set.seed(190211)
    mu_dat <- lapply(1:1000, function(i){
      unlist(Map(function(a, b){
        l <- rgamma(1, shape = a, rate = b)
        1/l^(1/shape)*gamma(1 + 1/shape)
      }, dat()$results_inter[[input$month]]$post$prior$a, 
      dat()$results_inter[[input$month]]$post$prior$b
      )) %>% matrix(., ncol = X, nrow = G, dimnames = dimnames(scheme_type))
    })
    mu_sign <- lapply(mu_dat, function(m)
      t(apply(subtypes[, 6:(5 + nrow(signatures))], 2, function(x) colSums(m*x)/sum(x)))
    ) %>% do.call("rbind", .)

    gglist <- data.frame(mu_sign) %>%
      cbind(signature = rownames(mu_sign)) %>%
      gather(trt, pfs, -signature) %>%
      mutate(sig = signature) %>%
      split(., .$signature) %>%
      lapply(., function(d)
        ggplot(d, aes(x = pfs, color = trt)) +
          geom_line(stat = "density") +
          labs(title = d$sig[1]) +
          theme(legend.position = "none")
      )
    legend <- get_legend(gglist[[1]] + theme(legend.position = "top"))
    plot_grid(plotlist = c(gglist, list(legend)), nrow = 3, ncol = 2)
  })
  output$density_sa <- renderPlot({
    set.seed(190111)
    mu_dat <- lapply(1:1000, function(i){
      unlist(Map(function(a, b){
        l <- rgamma(1, shape = a, rate = b)
        1/l^(1/shape)*gamma(1 + 1/shape)
      }, dat()$results_inter[[input$month + 1]]$post$prior$a,
      dat()$results_inter[[input$month + 1]]$post$prior$b
      )) %>% matrix(., ncol = X, nrow = G, dimnames = dimnames(scheme_type))
    })
    mu_sign <- lapply(mu_dat, function(m)
      t(apply(subtypes[, 6:(5 + nrow(signatures))], 2, function(x) colSums(m*x)/sum(x)))
    ) %>%
      do.call("rbind", .)

    gglist <- data.frame(mu_sign) %>%
      cbind(signature = rownames(mu_sign)) %>%
      gather(trt, pfs, -signature) %>%
      mutate(sig = signature) %>%
      split(., .$signature) %>%
      lapply(., function(d)
        ggplot(d, aes(x = pfs, color = trt)) +
          geom_line(stat = "density") +
          labs(title = d$sig[1]) +
          theme(legend.position = "none")
      )
    legend <- get_legend(gglist[[1]] + theme(legend.position = "top"))
    plot_grid(plotlist = c(gglist, list(legend)), nrow = 3, ncol = 2)
  })
  
  output$dat_title <- renderUI(
    h3(paste("Data up to the end of month", input$month ), align = "center")
  )
  output$dat_m <- renderDataTable({
    dat()$dta$sim_dta
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

