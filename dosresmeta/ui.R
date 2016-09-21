

library(shiny)

shinyUI(pageWithSidebar(
    
  ## Header
  tagList(tags$h1("Multivariate Dose-Response Meta-Analysis"),
          tags$h4("Designed by ", a("A. Crippa", target="_blank", href = "http://alessiocrippa.altervista.org"), 
                  "&", a("N. Orsini", target = "_blank", href = "http://nicolaorsini.altervista.org") ),
          tags$p("More info at", a("www.imm.ki.se/biostatistics/glst/", target = "_blank", 
                                   href = "http://www.imm.ki.se/biostatistics/glst/"))),
  
  ## Siderbar
  sidebarPanel(
    ## Panel "data"
    wellPanel(
      radioButtons('data', p(strong('Select dataset:')),
                   c(   #'Alcohol intake and colorectal cancer' = 'ex_alcohol_crc',
                     'Alcohol intake and lung cancer' = 'ex_alcohol_lc',
                     'Body mass index and renal cancer' = 'bmi_rc2',
                     'Alcohol intake and cardiovascular risk' = 'alcohol_cvd',
                     'Select your own data' = 'yourdata'), 'Body mass index and renal cancer'),  
      conditionalPanel(condition = "input.data == 'yourdata'",
                       helpText(a("Click Here to Download yourdata.xlsx Template", target = "_blank",     
                                  href = "http://alessiocrippa.altervista.org/data/yourdata.xlsx")), 
                       fileInput('file1', 'Choose xlsx File')
      )
    ),
    
    ## Panel "Graphical parameters"
    wellPanel(
      p(strong("Graphical parameters (Summary)")),
      checkboxInput('study', 'Mark studies', FALSE),
      checkboxInput('type', 'Differentiate study design', FALSE),
      helpText("Note: only the graphical results in the summary are affected by the selection;
               the dose-response analysis will not consider that.")
      ),
    wellPanel(
      radioButtons('pscorr', p(strong('Approximating covariance:')),
                   c('Greenland & Longnecker method' = 'gl','Hamling method' = 'h'), 
                   'Greenland & Longnecker method'),
      p(strong("Dose-response analyses")),
      checkboxInput(inputId = "mod_linear", label = "Linear Trend (dot-dash)"),
      checkboxInput(inputId = "mod_spline", label = "Spline Model (solid)"),
      conditionalPanel(condition = "input.mod_spline == true",
                       sliderInput("knots", "Select knots as the quantile of the distribution", 
                                   min = 0, max = 100, value = c(10, 90))
      ),
      checkboxInput(inputId = "mod_quadratic", label = "Quadratic Trend (dashed)"),
      tags$hr(),
      p(strong("Predictions")),
      checkboxGroupInput('conflim', 'Add confidence band:', list("Linear", "Spline", "Quadratic")),
      uiOutput("ref_slider"),
      numericInput("npred", "Number of predictions:", 15)
    ),
    
    ## Panel "Download"
    wellPanel(
      downloadButton('downloadPlot', 'Download Plot'),
      downloadButton('downloadPred', 'Download Prediction'),
      downloadButton('downloadData', 'Download Data'),
      tags$h4(),
      textInput("wh", "Width, Height (in)", "7, 7"),
      radioButtons('format', p(strong('Select graphical format:')),
                   c('png' = 'png', 'pdf' = 'pdf', 'eps' = 'eps'), 'pdf')
    )
  ),

  ## Main Panel (results)
  mainPanel(
    tabsetPanel(    
      tabPanel("Table",
               tableOutput("table")
      ),
      tabPanel("Summary", 
               p(strong("Scatter plot of RRs vs original exposure variable")),
               plotOutput(outputId = "sum_plot1"),
               p(strong("Scatter plot of logRRs vs dose (centered exposure)")),
               plotOutput(outputId = "sum_plot2"),
               h4("Main summary statistics"),
               verbatimTextOutput("summarystudy"),
               verbatimTextOutput("summarystat")
      ),    
      tabPanel("Dose-Response",
               h2(textOutput("selectmod")), 
               conditionalPanel("input.mod_linear == true",
                                p(strong("Linear Trend")),
                                verbatimTextOutput(outputId = "mod_linear_text")
               ),
               conditionalPanel("input.mod_spline == true",
                                p(strong("Spline model")),
                                verbatimTextOutput(outputId = "mod_spline_text")
               ),
               conditionalPanel("input.mod_quadratic== true",
                                p(strong("Quadratic trend")),
                                verbatimTextOutput(outputId = "mod_quadratic_text")
               )
      ),  
      tabPanel("Prediction",
               h4("Graphical prediction"),
               plotOutput(outputId = "predplot"),
               wellPanel(
                 checkboxInput('custplot', 'Show and modify default graphical options', FALSE),
                 conditionalPanel(condition = "input.custplot == true",
                                  textInput("xlab", "xlabel", "exposure"),
                                  textInput("ylab", "ylabel", "Relative Risk"),
                                  textInput("title", "title", " "),
                                  checkboxInput('limaxes', 'Limit axes', FALSE),
                                  conditionalPanel(condition = "input.limaxes == true",
                                                   uiOutput("y_range"),
                                                   uiOutput("x_range")
                                  ),
                                  textInput("xbreaks", "X-breaks:", ""),
                                  textInput("ybreaks", "Y-breaks:", ""),
                                  helpText("Note: breaks should be passed as numbers separated by , ex: .5, 1, 1.5, 2")
                 )
               ),
               tags$hr(),
               h4("Analytical predictions"),
               tableOutput(outputId = "pred")
      )
    )
  )

))
