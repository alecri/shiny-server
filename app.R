## Packages required
library(shiny)
Sys.setenv("plotly_username"="alecri")
Sys.setenv("plotly_api_key"="nr287vef19")
library(readxl)
library(dosresmeta)
library(ggplot2)
library(gridExtra)
library(rms)
library(plotly)

##------------------------------------------------------------------------------

## Additional function
vpc <- function(object){
   v <- object$v
   id <- object$id
   Psi <- object$Psi
   Slist <- object$Slist
   vlist <- lapply(unique(id), function(j) cbind(v[id == j]))
   Z <- model.matrix(object$formula, data = object$model)[, 2:(object$dim$q+1), drop = FALSE]
   Zlist <- lapply(unique(id), function(j)
      Z[id == j, , drop = FALSE])
   if (object$center){
      Zlist <- mapply(function(Z, v){
         scale(Z, Z[v==0, ], scale = FALSE)
      }, Zlist, vlist, SIMPLIFY = FALSE)
      Z <- do.call("rbind", Zlist)
   }
   Zlist <- lapply(Zlist, function(z) z[-1, , drop = FALSE])
   vpclist <- mapply(function(Z, S){
      diag(Z %*% Psi %*% t(Z))/diag(S + Z %*% Psi %*% t(Z))
   }, Zlist, Slist, SIMPLIFY = FALSE)
   vpc <- do.call("c", vpclist)
   vpc
}

## Read fluoa from excel files
fluo <- read_excel("Fluoride (2015)-animal-bioassay.xlsx", sheet = 1)
fluo <- fluo[, -c(3:20, 23:25, 29:32, 34:46, 50)]
fluo$`response units`[fluo$`response units` == "seconds"] <- "sec"
fluo$id_plot <- with(fluo, paste(`study name`, `endpoint id`, `dose index`, sep = ", " ))
fluo_wide <- reshape(fluo, v.names = c("dose", "N", "response", "stdev"),
                     timevar = "dose", idvar = "endpoint id", direction = "wide",
                     drop = c("dose index","lower_ci", "upper_ci", "pairwise significant",
                              "percent control mean", "percent control low", 
                              "percent control high", "id_plot"))   
fluo <- subset(fluo, (!is.na(stdev)) & (tags %in% c("|learning|", "|memory|")))
lin <- dosresmeta(formula = response ~ dose, id = `endpoint id`,
                  sd = stdev, n = N, covariance = "smd", data = fluo)
fluo$smd <- lin$model[, 1]
fluo$smd_v <- lin$v
fluo$smd_low <- fluo$smd - 1.96*sqrt(lin$v)
fluo$smd_upp <- fluo$smd + 1.96*sqrt(lin$v)
newdata_tab <- data.frame(dose = seq(0, 60, 10))


ui <- fluidPage(
   
   titlePanel("Learning and Memory: Morris Water Maze"),
   
   sidebarLayout(
      sidebarPanel(
         
         radioButtons("overall", h3("Select analysis"),
                      list("Overall" = 1, "Tags" = 2, "Effect subtype" = 3,
                           "Response units" = 4), selected = 1),
         conditionalPanel("input.overall == 2",
                          radioButtons("subgrp_tag", h3("Select subgroup"),
                                       list("Learning" = "|learning|", 
                                            "Memory" = "|memory|"),
                                       selected = "|learning|")
         ),
         conditionalPanel("input.overall == 3",
                          radioButtons("subgrp_eff", h3("Select subgroup"),
                                       list("Escape time" = "Escape time", 
                                            "Platform crossing" = "Platform crossing",
                                            "Time in platform area/location" = "Time in platform area/location",
                                            "Time in target quadrant" = "Time in target quadrant"),
                                       selected = "Escape time")
         ),
         conditionalPanel("input.overall == 4",
                          radioButtons("subgrp_unit", h3("Select subgroup"),
                                       list("Count" = "count", "Time (sec)" = "sec"),
                                       selected = "count")
         )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
            tabPanel("Descriptives",
                     fluidRow(
                        uiOutput("forest"),
                        column(6, plotlyOutput("p_pcm", height = 800)),
                        column(6, plotlyOutput("p_smd", height = 800))
                     ),
                     fluidRow(
                        uiOutput("hist"),
                        column(6, plotlyOutput("hist_pcm", height = 250)),
                        column(6, plotlyOutput("hist_smd", height = 250))
                     )
            ),
            tabPanel("Dose-response",
                     fluidRow(
                        uiOutput("spagh"),
                        plotlyOutput("spaghetti"),
                        uiOutput("drcurve"),
                        plotlyOutput("pooled"),
                        h3("Pooled standarized mean differences for selected fluoride doses",
                           align = "center"),
                        dataTableOutput("pred")
                        )
            ),
            tabPanel("Analytical results",
                     fluidRow(
                        h4("R output for the linear model", align = "center"),
                        verbatimTextOutput("linear"),
                        h4("R output for the spline model", align = "center"),
                        verbatimTextOutput("spline"),
                        h4("vcp plot for heterogeneity", align = "center"),
                        plotlyOutput("vpcPlot")
                     )
            )
         )
      )
   )
)


server <- function(input, output){

   # interactive titles
   lab <- reactive({
      lab <- if (input$overall == 1){
         "all the studies"
      } else if (input$overall == 2){
         paste("studies with tag", ifelse(input$subgrp_tag == "|learning|", "learning", "memory"))
      }
      else if (input$overall == 3){
         paste("studies with effect subtype", input$subgrp_eff)
      }
      else if (input$overall == 4){
         paste("studies with response unit", input$subgrp_unit)
      }
      lab
   })
   output$forest <- renderUI({ 
      h3(paste("Forest plot including", lab()), align = "center")
   })
   output$hist <- renderUI({ 
      h3(paste("Histogram of the response including", lab()), align = "center")
   })
   output$spagh <- renderUI({ 
      h3(paste("Spaghetti plot including", lab()), align = "center")
   })
   output$drcurve <- renderUI({ 
      h3(paste("Pooled dose-response curve based on", lab()), align = "center")
   })
   
   
   fluor <- reactive({
      dat <- if (input$overall == 1){
         fluo
      } else if (input$overall == 2){
         subset(fluo, tags == input$subgrp_tag)
      } else if (input$overall == 3){
         subset(fluo, `effect subtype` == input$subgrp_eff)
      } else if (input$overall == 4){
         subset(fluo, `response units` == input$subgrp_unit)
      }
      dat
   })
   
   lin <- reactive({
      dosresmeta(formula = response ~ dose, id = `endpoint id`,
                 sd = stdev, n = N, covariance = "smd", data = fluor(),
                 proc = "1stage")
   })
   output$linear <- renderPrint({
      summary(lin())
   })
   
   spl <- reactive({
      k <- with(fluor(), quantile(dose, c(.25, .5, .75)))
      dosresmeta(formula = response ~ rcs(dose, k), id = `endpoint id`,
                     sd = stdev, n = N, covariance = "smd", data = fluor(),
                     proc = "1stage")
   })
   output$spline <- renderPrint({
      summary(spl())
   })
   output$pred <- renderDataTable({
      round(data.frame(newdata_tab, predict(spl(), newdata_tab)[, -c(1:2)]), 2)
   })

   output$p_pcm <- renderPlotly({
      ggplotly(ggplot(subset(fluor(), `dose index` != 0), 
                      aes(x = id_plot, y= `percent control mean`,
                          ymin=`percent control low`, ymax=`percent control high`)) + 
                  geom_pointrange() + coord_flip() + geom_hline(aes(yintercept=0), lty=2) +
                  xlab("") + ylab("Percent control mean")
      )
   })
   output$hist_pcm <- renderPlotly({
      ggplotly(ggplot(subset(fluor(), `dose index` != 0), aes(x = `percent control mean`)) + geom_histogram())
   })
   output$p_smd <- renderPlotly({
      ggplotly(ggplot(subset(fluor(), `dose index` != 0), 
                      aes(x = id_plot, y= smd, ymin=smd_low, ymax=smd_upp)) + 
                  geom_pointrange() + coord_flip() + geom_hline(aes(yintercept=0), lty=2) +
                  xlab("") + ylab("Standardized mean differences")
      )
   })
   output$hist_smd <- renderPlotly({
      ggplotly(ggplot(subset(fluor(), `dose index` != 0), aes(x = smd)) + geom_histogram() +
                  xlab("Standardized mean differences"))
   })
   output$spaghetti <- renderPlotly({
      ggplotly(
         ggplot(fluor(), aes(dose, smd, group = `endpoint id`, shape = `study name`)) + 
            scale_shape_manual(values = seq_along(unique(fluo$`study name`))) +
            labs(shape = "author") +
            geom_line() + geom_point() + theme_bw() + 
            ylab("Standardized mean differences") + xlab("Fluoride, ppm")
      )
   })
   output$pooled <- renderPlotly({
         newdata <- data.frame(dose = seq(min(fluor()$dose), max(fluor()$dose), length.out = 100))
         pred <- ggplot(predict(spl(), newdata), 
                        aes(x = newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) +
            geom_line() + geom_ribbon(alpha = .1) + theme_bw() +
            xlab("Fluoride, ppm") + ylab("standardized mean difference") +
            geom_hline(aes(yintercept =  0), lty = 3) + 
            geom_line(data = predict(lin(), newdata), aes(y = pred), lty = 2)
         ggplotly(pred)
   })
   
   output$vpcPlot <- renderPlotly({
      ggplotly(
         ggplot(data.frame(x = fluor()$dose[fluor()$`dose index` != 0], y = vpc(spl())),
                aes(x = x, y = y)) + geom_point() + geom_smooth(se = F, method = "loess") + 
            theme_bw() + xlab("Fluoride, ppm") + ylab("vpc")
      )
   })
}