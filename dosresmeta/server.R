library("gdata")
library("rms")
library("dosresmeta")
library("ggplot2")
library("scales")


shinyServer(function(input, output) {
  
  ## Reading and fixing dataset
  dataset <- reactive({
    if(input$data != 'yourdata'){
      urldata <- paste0("http://alecri.github.io/downloads/data/", input$data, ".xlsx")
      data <- read.xls(urldata, sheet = 1, header = T, na.strings = c("NA", "#DIV/0!", "#N/A", " ", "."))
    }	
    if(input$data == 'yourdata'){
      inFile <- input$file1
      ## Otherwise error
      if (is.null(inFile))	return(NULL)
      data <- read.xls(inFile$datapath, sheet = 1, header = T, na.strings = c("NA", "#DIV/0!", "#N/A", " ", "."))
    }
    data$logrr <- log(data$rr)
    data$se <- (log(data$urr) - log(data$lrr)) / (2 * qnorm(.975))
    ## Center exposure variable for descriptive plot
    data$se[is.na(data$se)] <- 0
    index <- with(data, order(id, se))
    data$dose <- data$exposure[index]
    data.frame(data)
  })

  ## Slider for referent value
  output$ref_slider <- renderUI({
    inFile <- input$file1
    if (is.null(dataset())) {
      xmin <- 0
      xmax <- 1
      ref <- 0
    } else {
      xmin <- min(dataset()$exposure)
      xmax <- max(dataset()$exposure)
      ref <- min(dataset()$exposure)
    } 
    sliderInput("xref", "Referent value:", min = xmin, max = xmax, value = ref, step = (xmax - xmin)/99)
  }) 

  ## Y-range
  output$y_range <- renderUI({
    sliderInput("yrange", "Y-Range:", min = .01, max = 10, step = .1, 
                value = c(min(dataset()$rr), max(dataset()$rr)), round = 2)
  })
  ## X-range
  output$x_range <- renderUI({
    xmin <- min(dataset()$exposure)
    xmax <- max(dataset()$exposure)
    sliderInput("xrange", "X-Range:", min = xmin, max = xmax, step = (xmax - xmin)/99, value = c(xmin, xmax) )
  })
  
  ## Tab "Table" (data)
  output$table <- renderTable({
    dataset()
  })
  
  
  ## Tab "Summary"
  ## Defining shape of plotted points
  points <- reactive({
    points <- geom_point(aes(size = 1/se))
    if (input$study) {
      points <- geom_point(aes(size = 1/se, colour = factor(id)))
    }
    if (input$type) {
      points <- geom_point(aes(size = 1/se, shape = factor(type)))
    }
    if (input$study & input$type) {
      points <- geom_point(aes(size = 1/se, colour = factor(id), shape = factor(type)))
    }
    points
  })
  
  ## 1st summary plot
  output$sum_plot1 <- renderPlot({
    if (is.null(dataset()) )	return(NULL)
    p <- ggplot(dataset(), mapping = aes(exposure, rr)) + points() + theme_bw() + ylab("Relative Risks")
    print(p)
  })
  ## 2nd summary plot
  output$sum_plot2 <- renderPlot({
    if (is.null(dataset()) )	return(NULL)
    p <- ggplot(dataset(), mapping = aes(dose, logrr)) + points() + theme_bw() + ylab("Log Relative Risks")
    print(p)
  })
  ## Summary statistics:
  output$summarystudy <- renderPrint({
    if (is.null(dataset()) )	return(NULL)
    idtab <- as.data.frame(table(dataset()$id))
    names(idtab) <- list("ID Study", "# Observations")
    print(idtab, row.names = F)
  })
  output$summarystat <- renderPrint({
    if (is.null(dataset()) )	return(NULL)
    stat <- rbind(RRs = summary(dataset()$rr), LogRRs = summary(dataset()$logrr), Exposure = summary(dataset()$exposure))
    ## if exposure has non zero referent category
    if (sum(dataset()$exposure - dataset()$dose) != 0) stat <- rbind(stat, Dose = summary(dataset()$dose)) 
    stat
  })
  
  
  ## Tab "Dose-response"
  ## Give a message if no-model is selected
  output$selectmod <- renderText({
    sm = "Select a model"
    if (input$mod_linear | input$mod_quadratic | input$mod_spline) sm = ""
    sm
  })
  
  ## Performing chosen analyses
  lin <- reactive({
    dosresmeta(formula = logrr ~ exposure, id = id, type = type, se = se, cases = cases, n = n,
               data = dataset(), covariance = input$pscorr)
  })
  spl <- reactive({
      knots <- quantile(dataset()$exposure, prob = c(input$knots[1], 50, input$knots[2])/100)
      dosresmeta(formula = logrr ~ rcs(exposure, knots), id = id, type = type, se = se, cases = cases, n = n,
                 data = dataset(), covariance = input$pscorr)
  })
  quadr <- reactive({
    dosresmeta(formula = logrr ~ exposure + I(exposure^2), id = id, type = type, se = se, cases = cases, n = n,
               data = dataset(), covariance = input$pscorr)
  })
  
  ## Results from the dosresmeta analysis
  output$mod_linear_text <- renderPrint({
    summary(lin())
  })
  output$mod_spline_text <- renderPrint({
    summary(spl())
  })
  output$mod_quadratic_text <- renderPrint({
    summary(quadr())
  })
  
  
  ## Predictions from the dosresmeta analysis (Graph & Table)
  output$predplot <- renderPlot({
    if (is.null(dataset()) )	return(NULL)
    newdata <- data.frame(exposure = c(input$xref,
      seq(min(dataset()$exposure), max(dataset()$exposure), length = 100)))
    pred <- newdata 

    ## Setting of the plot  
    p <- ggplot(dataset(), aes(exposure, rr)) + geom_blank() + 
       scale_y_continuous(trans = "log", labels = function(y) round(y, 2)) + 
       theme_bw() + theme(axis.line = element_line(colour = "black"),
                          panel.border = element_blank(), panel.background = element_blank())
    
    if (input$mod_linear) {
      predlin <- predict(lin(), newdata = newdata, xref = input$xref, expo = T)[, c("pred", "ci.lb", "ci.ub")]
      names(predlin) <- paste0(names(predlin), ".lin")
      pred <- cbind(pred, predlin)
      p <- p + geom_line(data = pred, aes(x = exposure, y = pred.lin), linetype = "dotdash")
      if ("Linear" %in% input$conflim) {
        p <- p + geom_ribbon(data = pred, aes(x = exposure, y = pred.lin, ymin = ci.lb.lin, 
                                              ymax = ci.ub.lin), alpha = 0.1)
      }
    }
    if (input$mod_spline) {
      predspl <- predict(spl(), newdata = newdata, xref = input$xref, expo = T)[, c("pred", "ci.lb", "ci.ub")]
      names(predspl) <- paste0(names(predspl), ".spl")
      pred <- cbind(pred, predspl)
      p <- p + geom_line(data = pred, aes(x = exposure, y = pred.spl), linetype = "solid")
      if ("Spline" %in% input$conflim) {
        p <- p + geom_ribbon(data = pred, aes(x = exposure, y = pred.spl, ymin = ci.lb.spl, 
                                              ymax = ci.ub.spl), alpha = 0.1)
      }
    }
    if (input$mod_quadratic) {
      predquadr <- predict(quadr(), newdata = newdata, xref = input$xref, expo = T)[, c("pred", "ci.lb", "ci.ub")]
      names(predquadr) <- paste0(names(predquadr), ".quadr")
      pred <- cbind(pred, predquadr)
      p <- p + geom_line(data = pred, aes(x = exposure, y = pred.quadr), linetype = "dashed")
      if ("Quadratic" %in% input$conflim) {
        p <- p + geom_ribbon(data = pred, aes(x = exposure, y = pred.quadr, ymin = ci.lb.quadr, 
                                              ymax = ci.ub.quadr), alpha = 0.1)
      }
    }
 
    
    ## Adding chosen curves (with customized parameters)
    if (input$custplot){
    p <- p + scale_y_continuous(input$ylab, trans = "log", labels = function(y) round(y, 2),
            breaks = round(seq(input$yrange[1], input$yrange[2], length.out = 5), 2) ) + 
      scale_x_continuous(input$xlab) + labs(title = input$title)
    if (input$limaxes){
      p <- p + scale_y_continuous(input$ylab, trans = "log", limits = input$yrange) +
        scale_x_continuous(input$xlab, limits = input$xrange)
    }
    if (input$ybreaks != ""){
      breaks <- as.double(unlist(strsplit(input$ybreaks, ", ", fixed = T)))
      p <- p + scale_y_continuous(input$ylab, trans = "log", breaks = breaks)
      if (input$limaxes){
        p <- p + scale_y_continuous(input$ylab, trans = "log", limits = input$yrange, breaks = breaks)
      }
    }
    if (input$xbreaks != ""){
      breaks <- as.double(unlist(strsplit(input$xbreaks, ", ", fixed = T)))
      p <- p + scale_x_continuous(input$xlab, breaks = breaks)
      if (input$limaxes){
        p <- p + scale_x_continuous(input$xlab, limits = input$xrange, breaks = breaks)
      }
    }
    }
    print(p)
    
    ## Analytical predictions
    i <- seq(1, 100, length = input$npred)
    outpred <- as.data.frame(pred[i, ])
    colnames(outpred)[1] <- "exposure"
    output$pred <- renderTable(outpred)
    
    
    ## Download:
    ## Download predictions
    output$downloadPred <- downloadHandler(
      filename = function() { paste(input$data, '.Pred.csv', sep='') },
      content = function(file) {
        write.csv(outpred, file)
      })
    
    
    ## Download plot with customized parameters
    wh <- as.double(unlist(strsplit(input$wh, ", ", fixed = T)))
    output$downloadPlot <- downloadHandler(
      filename = function() { paste0(input$data, '.', input$format) },
      content = function(file) {
        if (input$format == 'png') png(file, width = wh[1], height = wh[2], px = "in")
        if (input$format == 'pdf') pdf(file, width = wh[1], height = wh[2]) 
        if (input$format == 'eps') postscript(file, width = wh[1], height = wh[1]) 
        print(p)
        dev.off()
      })
  })
  
  ## Download data (with modeled variables)	
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$data, '.csv', sep='') },
    content = function(file) {
      write.csv(dataset(), file)
    })
  
})
