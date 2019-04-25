# shinyapp_probio.Rdata

library(shiny)
library(shinycssloaders)
library(survminer)
library(survival)
library(tidyverse)
library(cowplot)
library(knitr)
library(plotly)
library(markdown)
library(Epi)


## load all files
load("www/dat_probio_dsmb.Rdata")
sigs <- list("all", "TP53- & AR-", "TP53+", "DRD+", "TEfus+")

ui <- navbarPage(
  title = div(img(src = "favicon-32x32.png"), "ProBio DSMB"),
  #tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png")),
  
  tabPanel("Home",
           titlePanel("Examples from simulation studies for ProBio"),
           #includeMarkdown(rmarkdown::render("www/intro.md")),
           includeMarkdown("www/intro.md"),
           hr(),
           selectInput("scen", h4("Select scenario"), 
                       choices = list("Scenario 1" = 1, "Scenario 2" = 2,
                                      "Scenario 3" = 3, "Scenario 4" = 4), 
                       selected = 1),
           br(),
           h5("Treatment effects accross disease subtypes in terms of progression free survival (PFS)"),
           tableOutput("ri_gx")
           ),
  navbarMenu("Example",
             tabPanel("Summary results",
                      h4("Graduation output"),
                      fluidRow(
                        column(5, h5("Superiority"), tableOutput("sup") %>% withSpinner(color="#0dc5c1")),
                        column(5, h5("Inferiority"), tableOutput("inf") %>% withSpinner(color="#0dc5c1"))
                      ),
                      br(),
                      h4("Final probabilities of superiority"),
                      fluidRow(
                        column(5, h5("By signature"), tableOutput("prob_sign_tab") %>% withSpinner(color="#0dc5c1")),
                        column(5, h5("By subtype"), tableOutput("prob_type_tab") %>% withSpinner(color="#0dc5c1"))
                      ),
                      br(),
                      h4("Enrolled participants"),
                      fluidRow(
                        column(5, h5("By signature"), tableOutput("n_sign_tab") %>% withSpinner(color="#0dc5c1")),
                        column(5, h5("By subtype"), tableOutput("n_type_tab") %>% withSpinner(color="#0dc5c1"))
                      ),
                      hr(),
                      br(),
                      fluidRow(
                        column(3, selectInput("signature", label = h4("Select signature"), 
                                              choices = levels(signatures$signatures), selected = "all")),
                        column(3, uiOutput("subtypes_in")),
                        column(3, selectInput("treatment", label = h4("Select treatment"), 
                                              choices = names(scheme_trt), 
                                              selected = names(scheme_trt), multiple = TRUE)),
                        column(3, actionButton("update", "Update plots"))
                      ),
                      br(),
                      h4(" Probabilities of superiority (signatures)"),
                      plotlyOutput("prob_sign", height = "600px") %>% withSpinner(color="#0dc5c1"),
                      br(),
                      h4(" Probabilities of superiority (subtypes)"),
                      plotlyOutput("prob_type", height = "800px") %>% withSpinner(color="#0dc5c1"),
                      br(),
                      h4("Randomization probabilities (subtype)"),
                      plotlyOutput("r_type", height = "800px") %>% withSpinner(color="#0dc5c1"),
                      br(),
                      h4("Enrolled participants over time"),
                      fluidRow(
                        column(5, h5("By signature"), plotlyOutput("n_sign", height = "600px") %>% withSpinner(color="#0dc5c1")),
                        column(5, h5("By subtype"), plotlyOutput("n_type", height = "600px") %>% withSpinner(color="#0dc5c1"))
                      ),
                      br(),
                      h4("Rates over time"),
                      fluidRow(
                        column(5, h5("By signature"), plotlyOutput("rate_sign", height = "600px") %>% withSpinner(color="#0dc5c1")),
                        column(5, h5("By subtype"), plotlyOutput("rate_type", height = "600px") %>% withSpinner(color="#0dc5c1"))
                      ),
                      hr(),
                      br(),
                      h4("Analysis of final simulated results"),
                      plotOutput("density_sign", height = "600px") %>% withSpinner(color="#0dc5c1"),
                      br(),
                      h4("Kaplan-Meier results:"),
                      verbatimTextOutput("km") %>% withSpinner(color="#0dc5c1"),
                      br(),
                      plotOutput("km_plot", height = "800px") %>% withSpinner(color="#0dc5c1")
                      ),
             tabPanel("Over month",
                      sliderInput("month", "Select follow-up month",
                                  min = 1, max = 35, value = 1),
                      fluidRow(
                        column(5, uiOutput("month")),
                        column(5, uiOutput("month_a"))
                      ),
                      br(),
                      h3("Randomization probabilities", align = "center"),
                      fluidRow(
                        column(5, uiOutput('r') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid"),
                        column(5, uiOutput('r_a') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid")
                      ),
                      br(),
                      h3("Enrolled participants (signatures)", align = "center"),
                      fluidRow(
                        column(5, uiOutput('n_s') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid"),
                        column(5, uiOutput('n_sa') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid")
                      ),
                      br(),
                      h3("Enrolled participants (subtypes)", align = "center"),
                      fluidRow(
                        column(5, uiOutput('n_t') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid"),
                        column(5, uiOutput('n_ta') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid")
                      ),
                      br(),
                      h3("Number of progressions (subtype)", align = "center"),
                      fluidRow(
                        column(5, uiOutput('delta_t') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid"),
                        column(5, uiOutput('delta_ta') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid")
                      ),
                      br(),
                      h3("Accumulated person-month (subtype)", align = "center"),
                      fluidRow(
                        column(5, uiOutput('time_t') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid"),
                        column(5, uiOutput('time_ta') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid")
                      ),
                      br(),
                      h3("Probability of superiority (subtype)", align = "center"),
                      fluidRow(
                        column(5, uiOutput('p_t') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid"),
                        column(5, uiOutput('p_ta') %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid")
                      ),
                      br(),
                      h3("Probability of superiority (signature)", align = "center"),
                      fluidRow(
                        column(5, uiOutput('p_s'), style = "border-right: 1px solid"),
                        column(5, uiOutput('p_sa'), style = "border-right: 1px solid")
                      ),
                      br(),
                      h3("Densities", align = "center"),
                      fluidRow(
                        column(5, plotOutput("density_s", height = "600px") %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid"),
                        column(5, plotOutput("density_sa", height = "600px") %>% withSpinner(color="#0dc5c1"), style = "border-right: 1px solid")
                      )
                      ),
             tabPanel("Data view",
                      sliderInput("month_dat", "Select follow-up month",
                                  min = 1, max = 35, value = 1),
                      uiOutput("dat_title"),
                      dataTableOutput("dat_m")
                      )
             ),
  tabPanel("Stat", tags$iframe(src="model_exemplification.pdf", width="900", height="1000")
           )
)
  
                 
                 
        
 
server <- function(input, output, session) {
  
  subtypes_in <- reactive({
    colnames(signatures)[signatures[signatures$signatures == input$signature, ] == "X"]
  })
  output$subtypes_in <- renderUI({
    selectInput("subtypes", label = h4("Select subtypes"),
                choices = subtypes_in(),
                selected = subtypes_in(), multiple = TRUE)
  })
  
  dat <- reactive(data_list[[as.numeric(input$scen)]])

  output$ri_gx <- renderTable(1/dat()$ri_gx^(1/shape)*gamma(1 + 1/shape), spacing = "xs", digits = 2, rownames = TRUE)
  output$sup <- renderTable(dat()$post$superiority, spacing = "xs", digits = 0, rownames = TRUE)
  output$inf <- renderTable(dat()$post$inferiority, spacing = "xs", digits = 0, rownames = TRUE)
  output$prob_sign_tab <- renderTable(dat()$post$p_sign, spacing = "xs", digits = 2, rownames = TRUE)
  output$prob_type_tab <- renderTable(dat()$post$p_type, spacing = "xs", digits = 2, rownames = TRUE)
  output$n_sign_tab <- renderTable(dat()$dta$n$n_sign, spacing = "xs", digits = 0, rownames = TRUE)
  output$n_type_tab <- renderTable(dat()$dta$n$n_type, spacing = "xs", digits = 0, rownames = TRUE)
  
  output$prob_sign <- renderPlotly({
    input$update
    ggplotly(
      dat()$dta$t_trt %>%
        as.data.frame() %>%
        rownames_to_column(var = "signature") %>%
        gather(Treatment, time, Enzalutamide:Docetaxel) %>%
        merge(dat()$dat_sign, ., by = c("signature", "Treatment")) %>%
        mutate(time = replace(time, time != month + 1 | time == mxmonth, NA)) %>%
        filter(isolate(input$signature == "all") | signature == isolate(input$signature),
               Treatment %in% isolate(input$treatment)) %>%
        ggplot(aes(month, prob, col = Treatment, event = event, n = n, PT = PT,
                   rate = rate, prob = prob)) +
        geom_line() + 
        geom_hline(yintercept = pU, linetype = "dashed", col = "grey") +
        geom_hline(yintercept = pL, linetype = "dashed", col = "red", alpha = .25) +
        geom_point(aes(x = time - 1)) +
        labs(y = "", title = "", x = "") +
        facet_wrap(~ signature, ncol = 3, scales = "fixed")
    )
  })
  output$prob_type <- renderPlotly({
    ggplotly(
      ggplot(filter(dat()$dat_type, input$update == 0 | (subtype %in% isolate(input$subtypes) & 
                    Treatment %in% isolate(input$treatment))),  
             aes(month, prob, col = Treatment, event = event, n = n, PT = PT,
                                 rate = rate, prob = prob, r = r, ntot = ntot)) +
        geom_line() +
        geom_hline(yintercept = pU2, linetype = "dashed", col = "grey") +
        geom_hline(yintercept = pU2n, linetype = "dashed", col = "grey") +
        geom_hline(yintercept = pL2, linetype = "dashed", col = "red", alpha = .25) +
        labs(y = "", title = "", x = "") +
        facet_wrap(~ subtype, ncol = 4, scales = "fixed")
    )
  })
  output$r_type <- renderPlotly({
    ggplotly(
      ggplot(filter(dat()$dat_type, input$update == 0 | (subtype %in% isolate(input$subtypes) &
                    Treatment %in% isolate(input$treatment))), 
             aes(month, r, col = Treatment, event = event, n = n, PT = PT,
                                 rate = rate, prob = prob, r = r, ntot = ntot)) +
        geom_line() + 
        facet_wrap(~ subtype, ncol = 4, scales = "fixed") +
        labs(y = "", title = "", x = "")
    )
  })
  output$n_sign <- renderPlotly({
    ggplotly(
      ggplot(filter(dat()$dat_sign, isolate(input$signature == "all") | signature == isolate(input$signature),
                    Treatment %in% isolate(input$treatment)), 
             aes(month, n, col = Treatment, event = event, n = n, PT = PT,
                                 rate = rate, prob = prob)) +
        geom_line() + labs(y = "", title = "", x = "") +
        facet_wrap(~ signature, ncol = 3, scales = "fixed")
    )
  })
  output$n_type <- renderPlotly({
    ggplotly(
      ggplot(filter(dat()$dat_type, input$update == 0 | (subtype %in% isolate(input$subtypes) & 
                    Treatment %in% isolate(input$treatment))), 
             aes(month, n, col = Treatment, event = event, n = n, PT = PT,
                                 rate = rate, prob = prob, r = r, ntot = ntot)) +
        geom_line() + 
        labs(y = "", title = "", x = "") +
        facet_wrap(~ subtype, ncol = 4, scales = "fixed")
    )
  })
  output$rate_type <- renderPlotly({
    ggplotly(
      ggplot(filter(dat()$dat_type, input$update == 0 | (subtype %in% isolate(input$subtypes) &
                    Treatment %in% isolate(input$treatment))), 
             aes(month, rate, col = Treatment, event = event, n = n, PT = PT,
                                 rate = rate, prob = prob, r = r, ntot = ntot)) +
        geom_line() + 
        facet_wrap(~ subtype, ncol = 4, scales = "free_y") +
        labs(y = "", title = "", x = "")
    )
  })
  output$rate_sign <- renderPlotly({
    ggplotly(
      ggplot(filter(dat()$dat_sign, input$update == 0 | (isolate(input$signature == "all") | signature == isolate(input$signature) &
                    Treatment %in% isolate(input$treatment))), 
             aes(month, rate, col = Treatment, event = event, n = n, PT = PT,
                                 rate = rate, prob = prob)) +
        geom_line() + 
        facet_wrap(~ signature, ncol = 3, scales = "free_y") +
        labs(y = "", title = "", x = "")
    )
  })
  output$density_sign <- renderPlot({
    legend <- get_legend(dat()$densities[[1]] + theme(legend.position = "top"))
    plot_grid(plotlist = c(dat()$densities, list(legend)), ncol = 3)
    
  })
  output$km <- renderPrint(dat()$fit_km)
  output$km_plot <- renderPlot(arrange_ggsurvplots(dat()$kmplots, ncol = 3, nrow = 2))
  
  
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
  output$n_s <- renderTable(
    dat()$results_inter[[input$month]]$dta$n$n_sign, spacing = "xs", digits = 0, rownames = TRUE
  )
  output$n_sa <- renderTable(
    dat()$results_inter[[input$month + 1]]$dta$n$n_sign, spacing = "xs", digits = 0, rownames = TRUE
  )
  output$n_t <- renderTable(
    dat()$results_inter[[input$month]]$dta$n$n_type, spacing = "xs", digits = 0, rownames = TRUE
  )
  output$n_ta <- renderTable(
    dat()$results_inter[[input$month + 1]]$dta$n$n_type, spacing = "xs", digits = 0, rownames = TRUE
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
    h3(paste("Data up to the end of month", input$month_dat ), align = "center")
  )
  output$dat_m <- renderDataTable({
    dat()$results_inter[[input$month_dat]]$dta$sim_dta
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

