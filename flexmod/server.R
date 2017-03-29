library(shiny)
library(scales)
# read_excel does not accept url yet
library(gdata)
library(DT)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(survival)
library(splines)
library(rms)
library(Epi)

shinyServer(function(input, output, session) {
   
   
   ##---------------------------------------------------------------------------
   ## Reading and fixing data
   
   ## Load initial data
   output$loadData <- renderPrint({
      invisible(dataset())
   })
   
   ## flag variable for avoiding initial errors
   avoidErr <- reactive({
      err <- FALSE
      if (input$type == "" | is.null(input$outcome) | is.null(input$exposure)){
         err <- TRUE
      }
      if (input$type == "surv"){
         if (input$censor == "" | is.null(input$exposure)){
            err <- TRUE
         }
      }
      err
   })
   
   ## Reading data
   dataset <- reactive({
      filename <- if (input$data == "marathon") {
         # can be expanded to other example datasets
         paste0("http://alecri.github.io/downloads/data/",input$data, ".xlsx")
      } else {
         inFile <- input$file
         if (is.null(inFile)) return(NULL)
         inFile$datapath
      }
      data <- read.xls(filename, sheet = 1, header = T,
                       na.strings = c("NA", "#DIV/0!", "#N/A", " ", "."))
      data.frame(data)
   })
   
   ## Definition of variables
   output$outcome <- renderUI({
      selectizeInput(
         'outcome',  strong('Outcome *'),
         choices = names(dataset()), options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
         )
      )
   })
   output$exposure <- renderUI({
      selectizeInput(
         'exposure',  strong('Quantitative predictor *'),
         choices = names(dataset()), options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
         )
      )
   })
   output$confounding <- renderUI({
      checkboxGroupInput(
         'confounding',  strong('Adjusting for'),
         names(dataset()), selected = NULL
         )
   })

   output$PT <- renderUI({
      selectizeInput(
         'pt',  " ",
         choices = names(dataset()), options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
         )
      )
   })
   output$surv <- renderUI({
      selectizeInput(
         'censor',  strong('Censoring *'),
         choices = names(dataset()), options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
         )
      )
   })
   
   ## Formatting data: need to be fixed
   ## can't access multiple elements of input
   datap <- reactive({
      if (input$outcome == "" | input$exposure == ""){
         return(dataset())
      } else {
         var <- c(input$outcome, input$exposure)
         if (!is.null(input$confounding)){
            var <- c(var, input$confounding)
         }
         if (input$PTcond){
            if (input$pt != "")
               var <- c(var, input$pt)
         }
         if (input$type == 'surv'){
            if (input$censor != "")
               var <- c(var, input$censor)
         }
         data <- subset(dataset(), select = var)
         colnames(data)[1:2] <- c("y", "x")
         return(data.frame(data))
      }
   })
   
   ## Output Data Panel
   output$datap <- renderDataTable({
     if (input$type == "") return(data.frame(dataset()))
     data.frame(datap())
   })

   ## Defaults value for knots 
   valuesKnots <- reactive({
      round(quantile(datap()$x, 1:input$k/(input$k + 1), na.rm = T), 2)
   })

   ## knots input
   knots <- reactive({
      if (input$knots_selection == FALSE){
         knots <- valuesKnots()
      } else {
         knots <- c()
         for (i in 1:input$k)
            knots <- c(knots, input[[paste0('k', i)]])
      }
      knots
   })
   
   ## values for tabular predictions
   xvalue_pred <- reactive({
      if (input$valuePred){
         input$updatePred
         xpred <- isolate(as.double(unlist(strsplit(input$valpred, ", ", fixed = T))))
         xvalue_pred <- if (length(xpred) > 1L)
            xpred
         else
            seq(min(datap()$x, na.rm = T), max(datap()$x, na.rm = T), length.out = 10)
      } else {
         xvalue_pred <- seq(min(datap()$x, na.rm = T), max(datap()$x, na.rm = T), length.out = 10)
      }
      xvalue_pred
   })
   
   
   ##---------------------------------------------------------------------------
   ## Rendering reactive ouput
   
   ## Defining knots: probably can be simplified
   output$knots <- renderUI({
      if (avoidErr())  return(NULL)
      kloc <- if (input$kloc != "")
         as.double(unlist(strsplit(input$kloc, ", ", fixed = T)))
      else
         NA
      lapply(1:input$k, function(i){
         vali <- if (!is.na(kloc[i]) & kloc[i] >= round(min(datap()$x, na.rm = T), 2) &
                     kloc[i] <= round(max(datap()$x, na.rm = T), 2)){
            kloc[i]
         } else {
            valuesKnots()[i]
         }
         sliderInput(eval(paste0('k', i)), label = NULL, min = round(min(datap()$x, na.rm = T), 2), 
                     max = round(max(datap()$x, na.rm = T), 2), value = vali, step = .01,
                     sep = "", round = 2)
      })
   })
   
   ## X-range
   output$x_range <- renderUI({
      if (is.null(input$exposure))  return(NULL)
      xmin <- round(min(datap()$x, na.rm = T), 2)
      xmax <- round(max(datap()$x, na.rm = T), 2)
      sliderInput("xrange", "X-Range:", min = xmin, max = xmax,
                  step = (xmax - xmin)/99, value = c(xmin, xmax), round = 2, sep = "")
   })
   
   ## Slider for referent value
   output$ref_slider <- renderUI({
      if (is.null(input$exposure))  return(NULL)
      xmin <- round(min(datap()$x, na.rm = T), 2)
      xmax <- round(max(datap()$x, na.rm = T), 2)
      ref <- if (input$xrefloc != ""){
         if (as.numeric(input$xrefloc) >= xmin & 
             as.numeric(input$xrefloc) <= xmax)
            as.numeric(input$xrefloc)
         else quantile(datap()$x, .5, na.rm = T)
      } else
         quantile(datap()$x, .5, na.rm = T)
      sliderInput("xref", strong("Referent value"), min = xmin, max = xmax, 
                  value = round(ref, 2), step = .01, sep = "", round = 2)
   })
   
   ## Y-range
   output$y_range <- renderUI({
      if (avoidErr())  return(NULL)
      if (ncol(prediction()) == 1L)  return(NULL)
      #ymin <- ggplot_build(p)$panel$ranges[[1]]$y.range[1]
      #ymax <- ggplot_build(p)$panel$ranges[[1]]$y.range[2]
      ymin <- max(round(min(prediction()[, -1]), 2), .01)
      ymax <- round(max(prediction()[, -1]), 2)
      sliderInput("yrange", "Y-range", min = ymin, max = ymax,
                  step = (ymax - ymin)/99, value = c(ymin, ymax), sep = "", round = 2)
   })
   output$y_range_abs <- renderUI({
      if (avoidErr())  return(NULL)
      if (ncol(prediction_abs()) == 1L)  return(NULL)
      ymin <- max(round(min(prediction_abs()[, -1]), 2), .01)
      ymax <- round(max(prediction_abs()[, -1]), 2)
      sliderInput("yrange_abs", "Y-range", min = ymin, max = ymax,
                  step = (ymax - ymin)/99, value = c(ymin, ymax), sep = "", round = 2)
   })
   
   ## Y-lab
   output$ylab <- renderUI({
      if (input$type == "")  lab = ""
      if (input$type == "gaussian")
         lab <- "Mean Difference"
      else if (input$type == "surv") 
         lab <- "Hazard Risk"
      else
         lab <- "Relative Risk"
      textInput("ylab", "ylabel", lab)
   })
   output$ylab_abs <- renderUI({
      lab <- ""
      if (input$type == "gaussian"){
         lab <- "Mean"
      }
      if (input$type == "binomial"){
         lab <- "Odds"
      }
      if (input$type == "poisson"){
         lab <- "Counts"
         if (input$PTcond && input$pt != "")
            lab <- "Rate"
      }
      textInput("ylab_abs", "ylabel", lab)
   })
   
   ## binwidth
   output$binwidth <- renderUI({
      if (is.null(input$exposure))  return(NULL)
      xrange <- max(datap()$x, na.rm = T) - min(datap()$x, na.rm = T)
      sliderInput("binwidth", "Width of bins:", min = round(xrange/100, 2), 
                  max = round(xrange/10, 2), value = round(xrange/30, 2), round = 2)      
   })
   
   
   ##---------------------------------------------------------------------------
   ##Analyses
   
   ## Fitting the chosen model: issue confounding + cox/poisson
   cat <- reactive({
      if (!input$mod_categorical) return(NULL)
      knots <- c(min(datap()$x, na.rm = T), knots(), max(datap()$x, na.rm = T))
      fit_model(formula = ~ I(cut(x, knots, include.lowest = T)), 
                       data = datap(), type = input$type, 
                       PTcond = input$PTcond, confounding = input$confounding)
   })
   lin <- reactive({
      if (!input$mod_linear) return(NULL)
     fit_model(formula = ~ x, data = datap(), type = input$type,
                       PTcond = input$PTcond, confounding = input$confounding)
   })
   linspl <- reactive({
      if (!input$linspl) return(NULL)
      fit_model(formula = ~ bs(x, knots = knots(), degree = 1), data = datap(), 
                       type = input$type, PTcond = input$PTcond, confounding = input$confounding)
   })
   quadrspl <- reactive({
      if (!input$quadrspl) return(NULL)
      fit_model(formula = ~ bs(x, knots = knots(), degree = 2), data = datap(), 
                       type = input$type, PTcond = input$PTcond, confounding = input$confounding)
   })
   cubspl <- reactive({
      if (!input$cubspl) return(NULL)
      fit_model(formula = ~ bs(x, knots = knots(), degree = 3), data = datap(), 
                type = input$type, PTcond = input$PTcond, confounding = input$confounding)
   })
   rcubspl <- reactive({
      if (!input$rcubspl) return(NULL)
      fit_model(formula = ~ rcs(x, parms = knots()), data = datap(), 
                type = input$type, PTcond = input$PTcond, confounding = input$confounding)
   })
   poly <- reactive({
      if (!input$mod_polynomial) return(NULL)
      expr <- "~"
      for (i in 1:input$n){
         expr <- paste0(expr, "+ I(x^", i, ")")
      }
      fit_model(formula = eval(parse(text = expr)), data = datap(), 
                type = input$type, PTcond = input$PTcond, confounding = input$confounding)
   })
   
   ## Overall tests
   test <- reactive({
      table <- data.frame()
      table <- rbind(table, 
                     if (input$mod_categorical){
                        test_parm(mod = cat(), label = "categorical", type = input$type, k = input$k)
                     },
                     if (input$mod_linear){
                        test_parm(mod = lin(), label = "linear", type = input$type, k = input$k)
                     }, 
                     if (input$linspl){
                        test_parm(mod = linspl(), label = "linspl", type = input$type, k = input$k)
                     }, 
                     if (input$quadrspl){
                        test_parm(mod = quadrspl(), label = "quadrspl", type = input$type, k = input$k)
                     }, 
                     if (input$cubspl){
                        test_parm(mod = cubspl(), label = "cubspl", type = input$type, k = input$k)
                     },
                     if (input$rcubspl){
                        test_parm(mod = rcubspl(), label = "rcubspl", type = input$type, k = input$k)
                     },
                     if (input$mod_polynomial){
                        test_parm(mod = poly(), label = "poly", type = input$type, k = input$n)
                     }
      )
      table
   })
   
   data_pred <- reactive({
      if (avoidErr())  return(NULL)
      x <- c(input$xref, xvalue_pred(), 
              seq(min(datap()$x, na.rm = T), max(datap()$x, na.rm = T), length.out = 500))
      if (input$valuePred){
         input$updatePred
         xpred <- isolate(as.double(unlist(strsplit(input$valpred, ", ", fixed = T))))
         x <- c(x, xpred)
      }
      data.frame(x = x)
   })
   
   ## Reactive (partial) predictions
   prediction <- reactive({
      if (avoidErr())  return(NULL)
      pred_value <<- data_pred()
      if (input$mod_categorical){
         knots <- c(min(datap()$x, na.rm = T), knots(), max(datap()$x, na.rm = T))
         pred_value <- cbind(pred_value, categorical =
                             model_pred(mod = cat(), formula = ~ I(cut(x, knots, include.lowest = T)),
                                        label = "categorical", type = input$type, 
                                        k = input$k, xref = input$xref))
      }
      if (input$mod_linear){
         pred_value <- cbind(pred_value, linear =
                             model_pred(mod = lin(), formula = ~ x,
                                        label = "linear", type = input$type, 
                                        k = input$k, xref = input$xref))
      }
      if (input$linspl){
         pred_value <- cbind(pred_value, linSpline = 
                             model_pred(mod = linspl(), formula = ~ bs(x, knots = knots(), degree = 1),
                                        label = "linspl", type = input$type, 
                                        k = input$k, xref = input$xref))
      }
      if (input$quadrspl){
         pred_value <- cbind(pred_value, quadrSpline = 
                             model_pred(mod = quadrspl(), formula = ~ bs(x, knots = knots(), degree = 2),
                                        label = "quadrspl", type = input$type, 
                                        k = input$k, xref = input$xref))
      }
      if (input$cubspl){
         pred_value <- cbind(pred_value, cubSpline = 
                             model_pred(mod = cubspl(), formula = ~ bs(x, knots = knots(), degree = 3),
                                        label = "cubspl", type = input$type, 
                                        k = input$k, xref = input$xref))
      }
      if (input$rcubspl){
         pred_value <- cbind(pred_value, RestrCubSpline =
                             model_pred(mod = rcubspl(), formula = ~ rcs(x, parms = knots()),
                                        label = "rcubspl", type = input$type, 
                                        k = (input$n+1), xref = input$xref))
      }
      if (input$mod_polynomial){
         expr <- "~ 1"
         for (i in 1:input$n){
            expr <- paste0(expr, "+ I(x^", i, ")")
         }
         pred_value <- cbind(pred_value, polynomial =
                             model_pred(mod = poly(), formula = as.formula(expr),
                                        label = "poly", type = input$type, 
                                        k = input$n, xref = input$xref))
      }
      pred_value
   })
   
   ## Reactive (absolute) predictions
   prediction_abs <- reactive({
      if (avoidErr())  return(NULL)
      if (input$type == "surv")  return(NULL)
      pred_value_abs <<- data_pred()
      if (input$mod_categorical){
         knots <- c(min(datap()$x, na.rm = T), knots(), max(datap()$x, na.rm = T))
         pred_value_abs <- cbind(pred_value_abs, categorical =
                                model_pred(mod = cat(), formula = ~ I(cut(x, knots, include.lowest = T)),
                                           label = "categorical", type = input$type, 
                                           k = input$k, xref = input$xref, int = T))
      }
      if (input$mod_linear){
         pred_value_abs <- cbind(pred_value_abs, linear =
                                model_pred(mod = lin(), formula = ~ x,
                                           label = "linear", type = input$type, 
                                           k = input$k, xref = input$xref, int = T))
      }
      if (input$linspl){
         pred_value_abs <- cbind(pred_value_abs, linSpline = 
                                model_pred(mod = linspl(), formula = ~ bs(x, knots = knots(), degree = 1),
                                           label = "linspl", type = input$type, 
                                           k = input$k, xref = input$xref, int = T))
      }
      if (input$quadrspl){
         pred_value_abs <- cbind(pred_value_abs, quadrSpline = 
                                model_pred(mod = quadrspl(), formula = ~ bs(x, knots = knots(), degree = 2),
                                           label = "quadrspl", type = input$type, 
                                           k = input$k, xref = input$xref, int = T))
      }
      if (input$cubspl){
         pred_value_abs <- cbind(pred_value_abs, cubSpline = 
                                model_pred(mod = cubspl(), formula = ~ bs(x, knots = knots(), degree = 3),
                                           label = "cubspl", type = input$type, 
                                           k = input$k, xref = input$xref, int = T))
      }
      if (input$rcubspl){
         pred_value_abs <- cbind(pred_value_abs, RestrCubSpline =
                                model_pred(mod = rcubspl(), formula = ~ rcs(x, parms = knots()),
                                           label = "rcubspl", type = input$type, 
                                           k = (input$n+1), xref = input$xref, int = T))
      }
      if (input$mod_polynomial){
         expr <- "~ 1"
         for (i in 1:input$n){
            expr <- paste0(expr, "+ I(x^", i, ")")
         }
         pred_value_abs <- cbind(pred_value_abs, polynomial =
                                model_pred(mod = poly(), formula = as.formula(expr),
                                           label = "poly", type = input$type, 
                                           k = input$n, xref = input$xref, int = T))
      }
      pred_value_abs
   })
   
   ## --------------------------------------------------------------------------
   ## Rendering output
   
   
   ## Output summary models
   output$mod_categorical_text <- renderPrint({
      summary(cat())
   })
   output$mod_linear_text <- renderPrint({
      summary(lin())
   })
   output$mod_linspl_text <- renderPrint({
      summary(linspl())
   })
   output$mod_quadrspl_text <- renderPrint({
      summary(quadrspl())
   })
   output$mod_cubspl_text <- renderPrint({
      summary(cubspl())
   })
   output$mod_rcubspl_text <- renderPrint({
      summary(rcubspl())
   })
   output$mod_poly_text <- renderPrint({
      summary(poly())
   })
   
   ## Output overall and non-linearity test
   output$test <- renderTable({
      if (avoidErr())  return(NULL)
      if (length(test()) == 0) return(NULL)
      test()
   })
   
   ## Histogram of observed exposure
   hist <- reactive({
      if (avoidErr())  return(NULL)
      p <- ggplot(datap(), aes(x = x)) + theme_bw() +
         geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = input$binwidth,
                        colour="black", fill="white") + 
         scale_y_continuous('percent') + scale_x_continuous(input$xlab)
      if (input$custplot){
         expr_xaxis <- "scale_x_continuous(input$xlab, limits = input$xrange"
         if (input$breaks){
            input$update
            xbreaks <- isolate(as.double(unlist(strsplit(input$xbreaks, ", ", fixed = T))))
            if (length(xbreaks) != 0)
               expr_xaxis <- paste(expr_xaxis, "breaks = xbreaks", sep = ", ")
         } 
         expr_xaxis <- paste0(expr_xaxis, ")")
         p <- p + eval(parse(text = expr_xaxis)) + eval(parse(text = input$theme)) 
      }
      p
   })

   pred <- reactive({
      if (avoidErr())  return(NULL)
      
      # Basic plot
      p <- ggplot(prediction(), aes(x, y = 1)) + geom_blank() + theme_bw() +
         scale_x_continuous("exposure") + scale_y_continuous("Mean Difference", labels = function(y)  format(round(y, 2), nsmall = 2))
      if (input$type != "gaussian") p <- p + scale_y_continuous(ifelse(input$type != "surv", "Relative Risk", "Hazard Rario"), 
                                                                trans = "log", breaks = trans_breaks('log', function(x) exp(x)),
                                                                labels = function(y) format(round(y, 2), nsmall = 2))
      # Display knots' location
      if (input$showKnots){
         p <- p + geom_vline(xintercept = knots(), linetype = input$knotlty)
      }
      # Adding models prediction
      if (input$mod_categorical){
         knots <- c(min(datap()$x, na.rm = T), knots(), max(datap()$x, na.rm = T))
         p <- p + geom_step(data = prediction(), aes(x = x, y = categorical.pred), linetype = input$catlty)
         if (any(input$CI == "categorical")){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, ymin = categorical.lowpred, 
                                     ymax = categorical.upppred), colour = "black", alpha = input$alpha)
         }
      }
      p <- if (input$mod_linear){
         addPred(name = "linear", p = p, data = prediction(), ltype = input$linlty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$linspl){
         addPred(name = "linSpline", p = p, data = prediction(), ltype = input$linspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$quadrspl){
         addPred(name = "quadrSpline", p = p, data = prediction(), ltype = input$quadrspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$cubspl){
         addPred(name = "cubSpline", p = p, data = prediction(), ltype = input$cubspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$rcubspl){
         addPred(name = "RestrCubSpline", p = p, data = prediction(), ltype = input$rcubspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$mod_polynomial){
         addPred(name = "polynomial", p = p, data = prediction(), ltype = input$linlty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      
      
      ## Customize the plot
      if (input$custplot){
         p <- p + labs(title = input$title)
         expr_xaxis <- "scale_x_continuous(input$xlab, limits = input$xrange"
         if (input$breaks){
            input$update
            xbreaks <- isolate(as.double(unlist(strsplit(input$xbreaks, ", ", fixed = T))))
            if (length(xbreaks) != 0)
               expr_xaxis <- paste(expr_xaxis, "breaks = xbreaks", sep = ", ")
         } 
         expr_xaxis <- paste0(expr_xaxis, ")")
         
         expr_yaxis <- "scale_y_continuous(input$ylab, limits = input$yrange, labels = function(y) format(round(y, 2), nsmall = 2)"
         if (input$type != "gaussian")
            expr_yaxis <- paste(expr_yaxis, "trans = 'log'", sep = ", ")
         if (input$breaks){
            input$update
            ybreaks <- isolate(as.double(unlist(strsplit(input$ybreaks, ", ", fixed = T))))
            if (length(ybreaks) != 0)
               expr_yaxis <- paste(expr_yaxis, "breaks = ybreaks", sep = ", ")  
         }
         expr_yaxis <- paste0(expr_yaxis, ")")
         
         p <- p + eval(parse(text = expr_xaxis)) +
            eval(parse(text = expr_yaxis)) + eval(parse(text = input$theme)) 
      }
      p
   })
   
   pred_abs <- reactive({
      if (avoidErr())  return(NULL)
      if (input$type == "surv")  return(NULL)

      lab <- ""
      if (input$type == "gaussian"){
         lab <- "Mean"
      }
      if (input$type == "binomial"){
         lab <- "Odds"
      }
      if (input$type == "poisson"){
         lab <- "Counts"
         if (input$PTcond && input$pt != "")
            lab <- "Rate"
      }
      
      # Basic plot
      p <- ggplot(prediction_abs(), aes(x = x, y = mean(datap()$y, na.rm = T))) + 
         geom_blank() + theme_bw() + scale_x_continuous("exposure") + 
         scale_y_continuous(lab, labels = function(y)  format(round(y, 2), nsmall = 2))
      if (input$type != "gaussian") p <- p + scale_y_continuous(lab, trans = "log", breaks = trans_breaks('log', function(x) exp(x)),
                                                                labels = function(y) format(round(y, 2), nsmall = 2))
      
      # Display knots' location
      if (input$showKnots){
         p <- p + geom_vline(xintercept = knots(), linetype = input$knotlty)
      }
      # Adding models prediction
      if (input$mod_categorical){
         knots <- c(min(datap()$x, na.rm = T), knots(), max(datap()$x, na.rm = T))
         p <- p + geom_step(data = prediction_abs(), aes(x = x, y = categorical.pred), linetype = input$catlty)
         if (any(input$CI == "categorical")){
            p <- p + geom_ribbon(data = prediction_abs(), 
                                 aes(x = x, ymin = categorical.lowpred, 
                                     ymax = categorical.upppred), colour = "black", alpha = input$alpha)
         }
      }
      p <- if (input$mod_linear){
         addPred(name = "linear", p = p, data = prediction_abs(), ltype = input$linlty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$linspl){
         addPred(name = "linSpline", p = p, data = prediction_abs(), ltype = input$linspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$quadrspl){
         addPred(name = "quadrSpline", p = p, data = prediction_abs(), ltype = input$quadrspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$cubspl){
         addPred(name = "cubSpline", p = p, data = prediprediction_absction(), ltype = input$cubspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$rcubspl){
         addPred(name = "RestrCubSpline", p = p, data = prediction_abs(), ltype = input$rcubspllty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      p <- if (input$mod_polynomial){
         addPred(name = "polynomial", p = p, data = prediction_abs(), ltype = input$linlty,
                 ci = input$CI, ltypeci = input$cilty, alpha = input$alpha, ciline = input$lines)
      } else p
      
      
      ## Customize the plot
      if (input$custplot){
         p <- p + labs(title = input$title_abs)
         expr_xaxis <- "scale_x_continuous(input$xlab, limits = input$xrange"
         if (input$breaks){
            input$update
            xbreaks <- isolate(as.double(unlist(strsplit(input$xbreaks, ", ", fixed = T))))
            if (length(xbreaks) != 0)
               expr_xaxis <- paste(expr_xaxis, "breaks = xbreaks", sep = ", ")
         } 
         expr_xaxis <- paste0(expr_xaxis, ")")
         p <- p + eval(parse(text = expr_xaxis)) + eval(parse(text = input$theme))
         
         if (input$custplot_abs){
            expr_yaxis <- "scale_y_continuous(input$ylab_abs, limits = input$yrange_abs, labels = function(y) format(round(y, 2), nsmall = 2)"
            if (input$type != "gaussian")
               expr_yaxis <- paste(expr_yaxis, "trans = 'log'", sep = ", ")
            if (input$breaks_abs){
               input$update_abs
               ybreaks <- isolate(as.double(unlist(strsplit(input$ybreaks_abs, ", ", fixed = T))))
               if (length(ybreaks) != 0)
                  expr_yaxis <- paste(expr_yaxis, "breaks = ybreaks", sep = ", ")  
            }
            expr_yaxis <- paste0(expr_yaxis, ")")
            p <- p + eval(parse(text = expr_yaxis))
         }
      }
      p
   })
   
   
   # Plot of 'partial' prediction
   output$predplot <- renderPlot({
      
      if (avoidErr())  return(NULL)
      if(input$hist == TRUE){
         plot(grid.arrange(pred(), hist(), heights = c(750, 250)))
      } else {
         plot(pred())
      }
      
   }, height = 400, width = 600)
   
   
   # Plot of 'absolute' prediction
   output$predplot_abs <- renderPlot({
      if (avoidErr())  return(NULL)
      if (input$type == "surv")  return(NULL)

      if(input$hist == TRUE){
         plot(grid.arrange(pred_abs(), hist(), heights = c(750, 250)))
      } else {
         plot(pred_abs())
      }
   }, height = 400, width = 600)
   
   
   ## Analytical predictions
   output$prediction <- renderTable({
      if (avoidErr())  return(NULL)
      round(unique(prediction()[1:(length(xvalue_pred())+1), , drop = F]), 2)
   })
   
   output$prediction_abs <- renderTable({
      if (avoidErr())  return(NULL)
      if (input$type == "surv")  return(NULL)
      round(unique(prediction_abs()[1:(length(xvalue_pred())+1), , drop = F]), 2)
   })
   

   
   ## Download:
   
   
   ## Figure
   ## Download plot with customized parameters
   wh <- reactive({
      as.double(unlist(strsplit(input$wh, ", ", fixed = T)))
      })
   output$downloadPlot <- downloadHandler(
      filename = function() { paste0("Figure", '.', input$format) },
      content = function(file) {
         if (input$format == 'png') png(file, width = wh()[1], height = wh()[2], px = "in")
         if (input$format == 'pdf') pdf(file, width = wh()[1], height = wh()[2]) 
         if (input$format == 'eps') postscript(file, width = wh()[1], height = wh()[2])
         if(input$fig == 'relative'){
            if(input$hist == TRUE){
               plot(grid.arrange(pred(), hist(), heights = c(750, 250)))
            } else {
               plot(pred())
            }
         }
         if(input$fig == 'absolute'){
            if(input$hist == TRUE){
               plot(grid.arrange(pred_abs(), hist(), heights = c(750, 250)))
            } else {
               plot(pred_abs())
            }
         }
         dev.off()
      })
   
   ## Download predictions
   output$downloadPred <- downloadHandler(
      filename = function() {"prediction.csv"},
      content = function(file) {
         if (input$tab == "relative"){
            write.csv(unique(prediction()[1:(length(xvalue_pred())+1), , drop = F]), file)
         }
         if (input$tab == "absolute"){
            write.csv(unique(prediction_abs()[1:(length(xvalue_pred())+1), , drop = F]), file)
         }
      }
      )
   
  
})
