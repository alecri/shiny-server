require("shiny")
require("scales")
require("gdata")
require("ggplot2")
require("splines")
require("rms")
require("aod")
require("survival")

shinyServer(function(input, output, session) {
   
   
   ##---------------------------------------------------------------------------
   ## Reading and fixing data
   
   ## Reading data
   dataset <- reactive({
      filename <- if (!input$data %in% c('yourdata', 'edit')) {
         paste0("http://alessiocrippa.altervista.org/data/",input$data, ".xlsx")
      } else {
         inFile <- input$file1
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
         'outcome',  strong('Outcome'),
         choices = names(dataset()), options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
         )
      )
   })
   output$exposure <- renderUI({
      selectizeInput(
         'exposure',  strong('Quantitative predictor'),
         choices = names(dataset()), options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
         )
      )
   })
   output$confounding <- renderUI({
      if (is.null(dataset())) return(NULL)
      checkboxGroupInput(
         'Confounding',  strong('Adjusting for'),
         names(dataset()), selected = ""
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
         'censor',  strong('Censoring'),
         choices = names(dataset()), options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
         )
      )
   })
   
   ## 'Fixed' dataset
   data <- reactive({
      data <- subset(dataset(), select = c(input$outcome, input$exposure, 
                                           input$confounding))
      if (input$PTcond){
         data <- cbind(data, subset(dataset(), select = input$pt))
         colnames(data)[ncol(data)] <- "pt"
      }
      if (input$type == 'surv'){
         data <- cbind(data, subset(dataset(), select = input$censor))
         colnames(data)[ncol(data)] <- "status"
      }
      colnames(data)[1:2] <- c("y", "x")
      data
   })
   
   ## Defaults for knots value
   valuesKnots <- reactive({
      xq <- c()
      for (i in 1:input$k){
         xq <- c(xq, quantile(data()$x, i/(input$k + 1), na.rm = T))
      }
      round(xq, 2)
   })
   ## knots input
   knots <- reactive({
      knots <- c()
      for (i in 1:input$k)
         knots <- c(knots, input[[paste0('k', i)]])
      knots
   })
   
   
   ##---------------------------------------------------------------------------
   ## Rendering reactive ouput
   
   ## X-range
   output$x_range <- renderUI({
      if (input$type == "")  return(NULL)
      xmin <- min(data()$x, na.rm = T)
      xmax <- max(data()$x, na.rm = T)
      sliderInput("xrange", "X-Range:", min = xmin, max = xmax, format = "##0.##",
                  step = (xmax - xmin)/99, value = c(xmin, xmax) )
   })
   
   # Y-range
   output$y_range <- renderUI({
      #ymin <- ggplot_build(p)$panel$ranges[[1]]$y.range[1]
      #ymax <- ggplot_build(p)$panel$ranges[[1]]$y.range[2]
      ymin <- min(prediction()[, -1])
      ymax <- max(prediction()[, -1])
      sliderInput("yrange", "Y-range", min = ymin, max = ymax, format = "##0.##",
                  step = (ymax - ymin)/99, value = c(ymin, ymax) )
   })
   
   ## Slider for referent value
   output$ref_slider <- renderUI({
      if (input$type == "")  return(NULL)
      xmin <- min(data()$x, na.rm = T)
      xmax <- max(data()$x, na.rm = T)
      ref <- if (input$xrefloc != " "){
         if (as.numeric(input$xrefloc) >= xmin & 
                as.numeric(input$xrefloc) <= xmax)
            as.numeric(input$xrefloc)
         else quantile(data()$x, .5, na.rm = T)
      } else
         quantile(data()$x, .5, na.rm = T)
      sliderInput("xref", strong("Referent value"), min = xmin, max = xmax, 
                  format = "##0.##", value = ref, step = .01)
   })
   
   ## Defining knots
   output$knots <- renderUI({
      if (input$type == "")  return(NULL)
      kloc <- if (input$kloc != "")
         as.double(unlist(strsplit(input$kloc, ", ", fixed = T)))
      else
         NA
      lapply(1:input$k, function(i){
         vali <- if (!is.na(kloc[i]) & kloc[i] >= round(min(data()$x, na.rm = T), 2) &
                        kloc[i] <= round(max(data()$x, na.rm = T), 2)){
            kloc[i]
         } else {
            valuesKnots()[i]
         }
         sliderInput(eval(paste0('k', i)), "", min = round(min(data()$x, na.rm = T), 2), 
                     max = round(max(data()$x, na.rm = T), 2), value = vali, step = .01)
      })
   })
   
   ## Y-lab
   output$ylab <- renderUI({
      if(input$type == "gaussian")
         lab <- "Mean Difference"
      else if (input$type == "surv") 
         lab <- "Hazard Risk"
      else
         lab <- "Relative Risk"
      textInput("ylab", "ylabel", lab)
   })
   
   ## binwidth
   output$binwidth <- renderUI({
      xrange <- max(data()$x, na.rm = T) - min(data()$x, na.rm = T)
      sliderInput("binwidth", "Width of bins:", min = round(xrange/100, 2), 
                  max = round(xrange/10, 2), value = round(xrange/30, 2))      
   })
   
   
   ##---------------------------------------------------------------------------
   ##Analyses
   
   ## Fitting the chosen model
   cat <- reactive({
      if (!input$mod_categorical) return(NULL)
      knots <- c(min(data()$x, na.rm = T), knots(), max(data()$x, na.rm = T))
      if (input$type != "surv"){
         mod <- glm(y ~ I(cut(x, knots, include.lowest = T)), data = data(), 
                    family = input$type)
         if (input$PTcond)
            mod <- update(mod, . ~ . + offset(log(pt)))
      } else {
         mod <- coxph(Surv(y, status) ~ I(cut(x, knots, include.lowest = T)), data = data())
      }
      if (length(input$confounding) > 0L)
         mod <- update(mod, as.formula(paste(". ~ . +", paste(input$confounding, collapse = " + "))))
      mod
   })
   lin <- reactive({
      if (!input$mod_linear) return(NULL)
      if (input$type != "surv"){
         mod <- glm(y ~ x, data = data(), family = input$type)
         if (input$PTcond)
            mod <- update(mod, . ~ . + offset(log(pt)))
      } else {
         mod <- coxph(Surv(y, status) ~ x, data = data())
      }
      if (length(input$confounding) > 0L)
         mod <- update(mod, as.formula(paste(". ~ . +", paste(input$confounding, collapse = " + "))))
      mod
   })
   linspl <- reactive({
      if (!input$linspl) return(NULL)
      if (input$type != "surv"){
         mod <- glm(y ~ bs(x, knots = knots(), degree = 1), data = data(), family = input$type)
         if (input$PTcond)
            mod <- update(mod, . ~ . + offset(log(pt)))
      } else {
         mod <- coxph(Surv(y, status) ~ bs(x, knots = knots(), degree = 1), data = data())
      }
      if (length(input$confounding) > 0L)
         mod <- update(mod, as.formula(paste(". ~ . +", paste(input$confounding, collapse = " + "))))
      mod
   })
   quadrspl <- reactive({
      if (!input$quadrspl) return(NULL)
      if (input$type != "surv"){
         mod <- glm(y ~ bs(x, knots = knots(), degree = 2), data = data(), family = input$type)
         if (input$PTcond)
            mod <- update(mod, . ~ . + offset(log(pt)))
      } else {
         mod <- coxph(Surv(y, status) ~ bs(x, knots = knots(), degree = 2), data = data())
      }
      if (length(input$confounding) > 0L)
         mod <- update(mod, as.formula(paste(". ~ . +", paste(input$confounding, collapse = " + "))))
      mod
   })
   cubspl <- reactive({
      if (!input$cubspl) return(NULL)
      if (input$type != "surv"){
         mod <- glm(y ~ bs(x, knots = knots(), degree = 3), data = data(), family = input$type)
         if (input$PTcond)
            mod <- update(mod, . ~ . + offset(log(pt)))
      } else {
         mod <- coxph(Surv(y, status) ~ bs(x, knots = knots(), degree = 3), data = data())
      }
      if (length(input$confounding) > 0L)
         mod <- update(mod, as.formula(paste(". ~ . +", paste(input$confounding, collapse = " + "))))
      mod
   })
   rcubspl <- reactive({
      if (!input$rcubspl) return(NULL)
      if (input$type != "surv"){
         mod <- glm(y ~ rcs(x, parms = knots()), data = data(), family = input$type)
         if (input$PTcond)
            mod <- update(mod, . ~ . + offset(log(pt)))
      } else {
         mod <- coxph(Surv(y, status) ~ rcs(x, parms = knots()), data = data())
      }
      if (length(input$confounding) > 0L)
         mod <- update(mod, as.formula(paste(". ~ . +", paste(input$confounding, collapse = " + "))))
      mod
   })
   poly <- reactive({
      if (!input$mod_polynomial) return(NULL)
      expr <- ifelse(input$type != "surv", "y ~ 1", "Surv(y, status) ~ 1")
      for (i in 1:input$n){
         expr <- paste0(expr, "+ I(x^", i, ")")
      }
      if (input$type != "surv"){
         mod <- glm(eval(parse(text = expr)), data = data(), family = input$type)
         if (input$PTcond)
            mod <- update(mod, . ~ . + offset(log(pt)))
      } else {
         mod <- coxph(eval(parse(text = expr)), data = data())
      }
      if (length(input$confounding) > 0L)
         mod <- update(mod, as.formula(paste(". ~ . +", paste(input$confounding, collapse = " + "))))
      mod
   })
   
   ## number of coeff for prediction and testing
   numbCoef <- function(model, type, k, n){
      if (model == "categorical"){
         index <- if (type != "surv"){
            2:(k+1)
         } else {
            1:k
         }
      }
      if (model == "linear"){
         index <- if (type != "surv"){
            2
         } else {
            1
         }
      }
      if (model == "linspl"){
         index <- if (type != "surv"){
            2:(k+2)
         } else {
            1:(k+1)
         }
      }
      if (model == "linspl"){
         index <- if (type != "surv"){
            2:(k+2)
         } else {
            1:(k+1)
         }
      }
      if (model == "quadrspl"){
         index <- if (type != "surv"){
            2:(k+3)
         } else {
            1:(k+2)
         }
      }
      if (model == "cubspl"){
         index <- if (type != "surv"){
            2:(k+4)
         } else {
            1:(k+3)
         }
      }
      if (model == "rcubspl"){
         index <- if (type != "surv"){
            2:(k)
         } else {
            1:(k-1)
         }
      }
      if (model == "poly"){
         index <- if (type != "surv"){
            2:(n+1)
         } else {
            1:(n)
         }
      }

      index
   }
   
   ## Overall tests
   test <- reactive({
      table <- data.frame()
      if (input$mod_categorical){
         index <- reactive({
            numbCoef("categorical", input$type, input$k)
         })
         overall <- wald.test(b = coef(cat())[index()], Sigma = vcov(cat())[index(), index()],
                              Terms = 1:length(coef(cat())[index()]))
         table <- rbind(table, data.frame(model = "categorical", overall = overall$result$chi2[3], 
                                          nonlinear = NA))
      }
      if (input$mod_linear){
         index <- reactive({
            numbCoef("linear", input$type, input$k)
         })
         overall <- wald.test(b = coef(lin())[index()], Sigma = vcov(lin())[index(), index()],
                              Terms = 1)
         table <- rbind(table, data.frame(model = "linear", overall = overall$result$chi2[3], 
                                          nonlinear = NA))
      }
      if (input$linspl){
         index <- reactive({
            numbCoef("linspl", input$type, input$k)
         })
         overall <- wald.test(b = coef(linspl())[index()], Sigma = vcov(linspl())[index(), index()],
                              Terms = 1:(length(coef(linspl())[index()])))
         nonlinear <- wald.test(b = coef(linspl())[index()], Sigma = vcov(linspl())[index(), index()],
                                Terms = 2:(length(coef(linspl())[index()])))
         table <- rbind(table, data.frame(model = "linspline", overall = overall$result$chi2[3], 
                                          nonlinear = nonlinear$result$chi2[3]))
      }
      if (input$quadrspl){
         index <- reactive({
            numbCoef("quadrspl", input$type, input$k)
         })
         overall <- wald.test(b = coef(quadrspl())[index()], Sigma = vcov(quadrspl())[index(), index()],
                              Terms = 1:(length(coef(quadrspl())[index()])))
         nonlinear <- wald.test(b = coef(quadrspl())[index()], Sigma = vcov(quadrspl())[index(), index()],
                                Terms = 2:(length(coef(quadrspl())[index()])))
         table <- rbind(table, data.frame(model = "quadrspline", overall = overall$result$chi2[3], 
                                          nonlinear = nonlinear$result$chi2[3]))
      }
      if (input$cubspl){
         index <- reactive({
            numbCoef("cubspl", input$type, input$k)
         })
         overall <- wald.test(b = coef(cubspl())[index()], Sigma = vcov(cubspl())[index(), index()],
                              Terms = 1:(length(coef(cubspl())[index()])))
         nonlinear <- wald.test(b = coef(cubspl())[index()], Sigma = vcov(cubspl())[index(), index()],
                                Terms = 2:(length(coef(cubspl())[index()])))
         table <- rbind(table, data.frame(model = "cubspline", overall = overall$result$chi2[3], 
                                          nonlinear = nonlinear$result$chi2[3]))
      }
      if (input$rcubspl){
         index <- reactive({
            numbCoef("rcubspl", input$type, input$k)
         })
         overall <- wald.test(b = coef(rcubspl())[index()], Sigma = vcov(rcubspl())[index(), index()],
                              Terms = 1:(length(coef(rcubspl())[index()])))
         nonlinear <- wald.test(b = coef(rcubspl())[index()], Sigma = vcov(rcubspl())[index(), index()],
                                Terms = 2:(length(coef(rcubspl())[index()])))
         table <- rbind(table, data.frame(model = "rcubspline", overall = overall$result$chi2[3], 
                                          nonlinear = nonlinear$result$chi2[3]))
      }
      if (input$mod_polynomial){
         index <- reactive({
            numbCoef("poly", input$type, n = input$n)
         })
         overall <- wald.test(b = coef(poly())[index()], Sigma = vcov(poly())[index(), index()],
                              Terms = 1:(length(coef(poly())[index()])))
         nonlinear <- wald.test(b = coef(poly())[index()], Sigma = vcov(poly())[index(), index()],
                                Terms = 2:(length(coef(poly())[index()])))
         table <- rbind(table, data.frame(model = "polynomial", overall = overall$result$chi2[3], 
                                          nonlinear = nonlinear$result$chi2[3]))
      }
      table
   })
   
   
   ## --------------------------------------------------------------------------
   ## Rendering output
   
   ## Output Data Panel
   output$data <- renderDataTable(dataset())
   
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
      test()
   })
   
   ## Histogram of observed exposure
   output$hist <- renderPlot({
      if (input$type == "")  return(NULL)
      p <- ggplot(data(), aes(x = x)) + theme_classic() +
         geom_bar(aes(y = (..count..)/sum(..count..)), binwidth= input$binwidth, 
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
         p <- p + eval(parse(text = expr_xaxis))
      }
      print(p)
   }, height = 150, width = 600)
   
   ## Reactive predictions
   prediction <- reactive({
      if (input$type == "")  return(NULL)
      x <- c(input$xref, seq(min(data()$x, na.rm = T), max(data()$x, na.rm = T), length.out = 1000))
      if (input$valuePred){
         input$updatePred
         xpred <- isolate(as.double(unlist(strsplit(input$valpred, ", ", fixed = T))))
         x <- c(x, xpred)
      }
      prediction <- data.frame(x = x)
      if (input$mod_categorical){
         knots <- c(min(data()$x, na.rm = T), knots(), max(data()$x, na.rm = T))
         X <- model.matrix(~ I(cut(x, knots, include.lowest = T)), data = prediction)
         X <-  t(apply(X, 1, "-", X[which(prediction$x == input$xref)[1], ]))[, -1]
         index <- reactive({
            numbCoef("categorical", input$type, input$k)
         })
         pred <- X %*% coef(cat())[index()]
         sepred <- sqrt(diag((X %*% vcov(cat())[index(), index()] %*% t(X))))
         if (input$type != "gaussian") pred <- exp(pred)
         ci <- if (input$type == "gaussian")
            data.frame(lowpred = pred - qnorm(.975)*sepred,
                       upppred = pred + qnorm(.975)*sepred)
         else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                         upppred = exp(log(pred) + qnorm(.975)*sepred))
         prediction <- cbind(prediction, categorical = cbind(pred, ci))
      }
      if (input$mod_linear){
         X <- model.matrix(~ x, data = prediction)
         X <-  t(apply(X, 1, "-", X[which(prediction$x == input$xref)[1], ]))[, -1, drop = F]
         index <- reactive({
            numbCoef("linear", input$type, input$k)
         })
         pred <- X %*% coef(lin())[index()]
         sepred <- sqrt(diag((X %*% vcov(lin())[index(), index()] %*% t(X))))
         if (input$type != "gaussian") pred <- exp(pred)
         ci <- if (input$type == "gaussian")
            data.frame(lowpred = pred - qnorm(.975)*sepred,
                       upppred = pred + qnorm(.975)*sepred)
         else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                         upppred = exp(log(pred) + qnorm(.975)*sepred))
         prediction <- cbind(prediction, linear = cbind(pred, ci))
      }
      if (input$linspl){
         X <- model.matrix(~ bs(x, knots = knots(), degree = 1), data = prediction)
         X <-  t(apply(X, 1, "-", X[which(prediction$x == input$xref)[1], ]))[, -1]
         index <- reactive({
            numbCoef("linspl", input$type, input$k)
         })
         pred <- X %*% coef(linspl())[index()]
         sepred <- sqrt(diag((X %*% vcov(linspl())[index(), index()] %*% t(X))))
         if (input$type != "gaussian") pred <- exp(pred)
         ci <- if (input$type == "gaussian")
            data.frame(lowpred = pred - qnorm(.975)*sepred,
                       upppred = pred + qnorm(.975)*sepred)
         else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                         upppred = exp(log(pred) + qnorm(.975)*sepred))
         prediction <- cbind(prediction, linSpline = cbind(pred, ci))
      }
      if (input$quadrspl){
         X <- model.matrix(~ bs(x, knots = knots(), degree = 2), data = prediction)
         X <-  t(apply(X, 1, "-", X[which(prediction$x == input$xref)[1], ]))[, -1]
         index <- reactive({
            numbCoef("quadrspl", input$type, input$k)
         })
         pred <- X %*% coef(quadrspl())[index()]
         sepred <- sqrt(diag((X %*% vcov(quadrspl())[index(), index()] %*% t(X))))
         if (input$type != "gaussian") pred <- exp(pred)
         ci <- if (input$type == "gaussian")
            data.frame(lowpred = pred - qnorm(.975)*sepred,
                       upppred = pred + qnorm(.975)*sepred)
         else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                         upppred = exp(log(pred) + qnorm(.975)*sepred))
         prediction <- cbind(prediction, quadrSpline = cbind(pred, ci))
      }
      if (input$cubspl){
         X <- model.matrix(~ bs(x, knots = knots(), degree = 3), data = prediction)
         X <-  t(apply(X, 1, "-", X[which(prediction$x == input$xref)[1], ]))[, -1]
         index <- reactive({
            numbCoef("cubspl", input$type, input$k)
         })
         pred <- X %*% coef(cubspl())[index()]
         sepred <- sqrt(diag((X %*% vcov(cubspl())[index(), index()] %*% t(X))))
         if (input$type != "gaussian") pred <- exp(pred)
         ci <- if (input$type == "gaussian")
            data.frame(lowpred = pred - qnorm(.975)*sepred,
                       upppred = pred + qnorm(.975)*sepred)
         else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                         upppred = exp(log(pred) + qnorm(.975)*sepred))
         prediction <- cbind(prediction, cubSpline = cbind(pred, ci))
      }
      if (input$rcubspl){
         X <- model.matrix(~ rcs(x, parms = knots()), data = prediction)
         X <-  t(apply(X, 1, "-", X[which(prediction$x == input$xref)[1], ]))[, -1]
         index <- reactive({
            numbCoef("rcubspl", input$type, input$k)
         })
         pred <- X %*% coef(rcubspl())[index()]
         sepred <- sqrt(diag((X %*% vcov(rcubspl())[index(), index()] %*% t(X))))
         if (input$type != "gaussian") pred <- exp(pred)
         ci <- if (input$type == "gaussian")
            data.frame(lowpred = pred - qnorm(.975)*sepred,
                       upppred = pred + qnorm(.975)*sepred)
         else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                         upppred = exp(log(pred) + qnorm(.975)*sepred))
         prediction <- cbind(prediction, RestrCubSpline = cbind(pred, ci))
      }
      if (input$mod_polynomial){
         expr <- ifelse(input$type != "surv", "~ 1", " ~ 1")
         for (i in 1:input$n){
            expr <- paste0(expr, "+ I(x^", i, ")")
         }
         X <- model.matrix(as.formula(expr), data = prediction)
         X <-  t(apply(X, 1, "-", X[which(prediction$x == input$xref)[1], ]))[, -1, drop = F]
         index <- reactive({
            numbCoef("poly", input$type, n = input$n)
         })
         pred <- X %*% coef(poly())[index()]
         sepred <- sqrt(diag((X %*% vcov(poly())[index(), index()] %*% t(X))))
         if (input$type != "gaussian") pred <- exp(pred)
         ci <- if (input$type == "gaussian")
            data.frame(lowpred = pred - qnorm(.975)*sepred,
                       upppred = pred + qnorm(.975)*sepred)
         else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                         upppred = exp(log(pred) + qnorm(.975)*sepred))
         prediction <- cbind(prediction, polynomial = cbind(pred, ci))
      }
      
      prediction
   })
   
   ## Plot of 'partial' prediction
   output$predplot <- renderPlot({
      if (input$type == "")  return(NULL)
      
      ## Basic plot
      p <- ggplot(data(), aes(x, y = 1)) + geom_blank() + theme_classic() +
         scale_x_continuous("exposure") + scale_y_continuous("Mean Difference", labels = function(y)  format(round(y, 2), nsmall = 2))
      if (input$type != "gaussian") p <- p + scale_y_continuous(ifelse(input$type != "surv", "Relative Risk", "Hazard Rario"), 
                                                                trans = "log", breaks = trans_breaks('log', function(x) exp(x)),
                                                                labels = function(y) format(round(y, 2), nsmall = 2))
      coord_trans(y="log")
      ## Display knots' location
      if (input$showKnots){
         for (i in 1:input$k){
            p <- p + geom_vline(xintercept = input[[paste0('k', i)]], linetype = input$knotlty)
         }
      }
      ## Adding models prediction
      if (input$mod_categorical){
         knots <- c(min(data()$x, na.rm = T), knots(), max(data()$x, na.rm = T))
         for (i in 2:length(knots)){
            p <- p + geom_step(data = prediction()[(prediction()$x <= knots[i] & prediction()$x > knots[i-1]), ], 
                               aes(x = x, y = categorical.pred), linetype = input$catlty)  
         }
         if (input$confCategorical){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, ymin = categorical.lowpred, 
                                     ymax = categorical.upppred),
                                 alpha = input$alpha)
            if (input$lines)
               for (i in 2:length(knots)){
                  p <- p + geom_line(data = prediction()[(prediction()$x <= knots[i] & prediction()()$x > knots[i-1]), ], 
                                     aes(x = x, y = categorica.lowpred), linetype = input$cilty) +
                     geom_line(data = prediction()[(prediction()$x <= knots[i] & prediction()$x > knots[i-1]), ], 
                               aes(x = x, y = categorical.upppred), linetype = input$cilty)
               }
         }
      }
      if (input$mod_linear){
         p <- p + geom_line(data = prediction(), 
                            aes(x = x, y = linear.pred), linetype = input$linlty)
         if (input$confLinear){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, y = linear.pred, ymin = linear.lowpred, 
                                     ymax = linear.upppred),
                                 alpha = input$alpha)
            if (input$lines)
               p <- p + geom_line(data = prediction(), 
                                  aes(x = x, y = linear.lowpred), linetype = input$cilty) +
               geom_line(data = prediction(), 
                         aes(x = x, y = linear.upppred), linetype = input$cilty)
         }
      }
      if (input$linspl){
         p <- p + geom_line(data = prediction(), 
                            aes(x = x, y = linSpline.pred), linetype = input$linspllty)
         if (input$confLinspl){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, y = linSpline.pred, ymin = linSpline.lowpred, 
                                     ymax = linSpline.upppred),
                                 alpha = input$alpha)
            if (input$lines)
               p <- p + geom_line(data = prediction(), 
                                  aes(x = x, y = linSpline.lowpred), linetype = input$cilty) +
               geom_line(data = prediction(), 
                         aes(x = x, y = linSpline.upppred), linetype = input$cilty)
         }
      }
      if (input$quadrspl){
         p <- p + geom_line(data = prediction(), 
                            aes(x = x, y = quadrSpline.pred), linetype = input$quadrspllty)
         if (input$confQuadrspl){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, y = quadrSpline.pred, ymin = quadrSpline.lowpred, 
                                     ymax = quadrSpline.upppred),
                                 alpha = input$alpha)
            if (input$lines)
               p <- p + geom_line(data = prediction(), 
                                  aes(x = x, y = quadrSpline.lowpred), linetype = input$cilty) +
               geom_line(data = prediction(), 
                         aes(x = x, y = quadrSpline.upppred), linetype = input$cilty)
         }
      }
      if (input$cubspl){
         p <- p + geom_line(data = prediction(), 
                            aes(x = x, y = cubSpline.pred), linetype = input$cubspllty)
         if (input$confCubspl){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, y = cubSpline.pred, ymin = cubSpline.lowpred, 
                                     ymax = cubSpline.upppred),
                                 alpha = input$alpha)
            if (input$lines)
               p <- p + geom_line(data = prediction(), 
                                  aes(x = x, y = cubSpline.lowpred), linetype = input$cilty) +
               geom_line(data = prediction(), 
                         aes(x = x, y = cubSpline.upppred), linetype = input$cilty)
         }
      }
      if (input$rcubspl){
         p <- p + geom_line(data = prediction(), 
                            aes(x = x, y = RestrCubSpline.pred), linetype = input$rcubspllty)
         if (input$confRcubspl){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, y = RestrCubSpline.pred, ymin = RestrCubSpline.lowpred, 
                                     ymax = RestrCubSpline.upppred),
                                 alpha = input$alpha)
            if (input$lines)
               p <- p + geom_line(data = prediction(), 
                                  aes(x = x, y = RestrCubSpline.lowpred), linetype = input$cilty) +
               geom_line(data = prediction(), 
                         aes(x = x, y = RestrCubSpline.upppred), linetype = input$cilty)
         }
      }
      if (input$mod_polynomial){
         p <- p + geom_line(data = prediction(), 
                            aes(x = x, y = polynomial.pred), linetype = input$linlty)
         if (input$confPolynomial){
            p <- p + geom_ribbon(data = prediction(), 
                                 aes(x = x, y = polynomial.pred, ymin = polynomial.lowpred, 
                                     ymax = polynomial.upppred),
                                 alpha = input$alpha)
            if (input$lines)
               p <- p + geom_line(data = prediction(), 
                                  aes(x = x, y = polynomial.lowpred), linetype = input$cilty) +
               geom_line(data = prediction(), 
                         aes(x = x, y = polynomial.upppred), linetype = input$cilty)
         }
      }
      
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
         
         expr_yaxis <- "scale_y_continuous(input$ylab, limits = input$xrange, labels = function(y) format(round(y, 2), nsmall = 2)"
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
            eval(parse(text = expr_yaxis))
      }
      
      print(p)
      
      ## Analytical predictions
      j <- reactive({
         if (input$valuePred){
            input$updatePred
            xpred <- isolate(as.double(unlist(strsplit(input$valpred, ", ", fixed = T))))
            j <- if (length(xpred) > 1L)
               which(prediction()$x %in% xpred)
            else
               seq(1, nrow(prediction()), length.out = 50)
         } else {
            j <- seq(1, nrow(prediction()), length.out = 50)
         }
         j
      })
      
      output$prediction <- renderTable({
         round(unique(prediction()[j(), ]), 2)
      })
      
      ## Download plot with customized parameters
      wh <- as.double(unlist(strsplit(input$wh, ", ", fixed = T)))
      output$downloadPlot <- downloadHandler(
         filename = function() { paste0("Figure", '.', input$format) },
         content = function(file) {
            if (input$format == 'png') png(file, width = wh[1], height = wh[2], px = "in")
            if (input$format == 'pdf') pdf(file, width = wh[1], height = wh[2]) 
            if (input$format == 'eps') postscript(file, width = wh[1], height = wh[1]) 
            print(p)
            dev.off()
         })
      
      ## Download:
      ## Download predictions
      output$downloadPred <- downloadHandler(
         filename = function() {"prediction.csv"},
         content = function(file) {
            write.csv(prediction[j() , ], file)
         })
      
   }, height = 400, width = 600)
  
})