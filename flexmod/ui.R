lty <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F")

shinyUI(navbarPage(
   "Flexible modeling",

   ## Homepage Panl
   tabPanel(
      "Homepage",
      fluidPage(  
         h1("Flexible Modeling of Quantitative Predictors"),
         h4("Designed by ",
            a("A. Crippa", target="_blank", href = "http://alessiocrippa.altervista.org"),
            "&", 
            a("N. Orsini", target = "_blank", href = "http://nicolaorsini.altervista.org")), 
         p("More info at",
           a("www.imm.ki.se/biostatistics", target = "_blank", href = "http://www.imm.ki.se/biostatistics/")),
         br()
      ),
      img(src = "pic.jpg", align = "middle", height = 600, width = 600)
   ),
  
   
   ## Data Panel
   tabPanel(
      "Data",
      sidebarLayout(
         sidebarPanel(
            radioButtons(
               "data", strong("Select dataset:"), 
               list("hyponatremia" = "hyponatremianum",
                    "Select your own data" = "yourdata"), "hyponatremianum"
            ),
            conditionalPanel(
               condition = "input.data == 'yourdata'",
               fileInput('file1', 'Choose xlsx File')
            ),
            hr(),
            fluidRow(
               column(5,
                      selectizeInput(
                         'type',  strong('Outcome Type'),
                         choices = c(' ' = ' ',
                                     Continuous = 'gaussian',
                                     Binary = 'binomial',
                                     Counts = 'poisson',
                                     Survival = 'surv'), ' ', options = list(
                                        placeholder = 'Please select an option below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                     )
                      )
               ),
               column(5,
                      uiOutput("outcome")
               )
            ),
            conditionalPanel(condition = "input.type == 'poisson'",
                             fluidRow(
                                column(5, NULL),
                                column(5,
                                       checkboxInput(inputId = "PTcond", label = strong("Person-Time"), FALSE)))
            ),
            conditionalPanel(condition = "input.PTcond == true",
                             fluidRow(
                                column(5, NULL),
                                column(5, uiOutput("PT")))
            ),
            conditionalPanel(condition = "input.type == 'surv'",
                             fluidRow(
                                column(5, NULL),
                                column(5, uiOutput("surv")))
            ),
            uiOutput("exposure"),
            uiOutput("confounding")
         ),
         
         mainPanel(
            dataTableOutput('data')
         )
      )
   ),
   
   
   ## Modeling
   tabPanel(
      "Modeling Strategies",
      sidebarLayout(
         sidebarPanel(
            checkboxInput(inputId = "showKnots", label = "Display knots position", FALSE),
            numericInput("k", "Number of knots:", 4),
            uiOutput("knots"),
            textInput("kloc", "Knots location", ' '),
            
            br(),
            strong("Choose functional relationship"),
            checkboxInput(inputId = "mod_categorical", label = "Categories"),
            checkboxInput(inputId = "mod_linear", label = "Linear Trend"),
            checkboxInput(inputId = "linspl", label = "Linear Splines"),
            checkboxInput(inputId = "quadrspl", label = "Quadratic Splines"),
            checkboxInput(inputId = "cubspl", label = "Cubic Splines"),
            checkboxInput(inputId = "rcubspl", label = "Restricted Cubic Splines"),
            checkboxInput(inputId = "mod_polynomial", label = "Polynomial Trend"),
            conditionalPanel(condition = "input.mod_polynomial == true",
                             numericInput("n", "Polynomial degree:", 3)
                             ),
            br(),
            uiOutput("ref_slider"),
            textInput("xrefloc", "", ' '),
            br(),
            strong("Add confidence band"),
            checkboxInput(inputId = "confCategorical", label = "Categories"),
            checkboxInput(inputId = "confLinear", label = "Linear Trend"),
            checkboxInput(inputId = "confLinspl", label = "Linear Splines"),
            checkboxInput(inputId = "confQuadrspl", label = "Quadratic Splines"),
            checkboxInput(inputId = "confCubspl", label = "Cubic Splines"),
            checkboxInput(inputId = "confRcubspl", label = "Restricted Cubic Splines"),
            checkboxInput(inputId = "confPolynomial", label = "Polynomial Trend"),
            hr(),
            wellPanel(
               downloadButton('downloadPlot', 'Download Plot'),
               tags$h4(),
               textInput("wh", "Width, Height (in)", "7, 7"),
               radioButtons('format', p(strong('Select graphical format:')),
                            c('png' = 'png', 'pdf' = 'pdf', 'eps' = 'eps'), 'pdf')
            )
         ),
         
         
         mainPanel(
            div(align = "center", 
            plotOutput(outputId = "predplot", width = "100%")),
            conditionalPanel(condition = "input.hist == true",
                             div(align = "center", 
                                 plotOutput(outputId = "hist", width = "100%", height = "150px"))),
            checkboxInput('hist', 'Exposure distribution', FALSE),
            checkboxInput('custplot', 'Show and modify default graphical options', F),
            conditionalPanel(condition = "input.custplot == true",
                             fluidRow(
                                column(4, textInput("xlab", "xlabel", "exposure")),
                                column(4, uiOutput("ylab")),
                                column(4, textInput("title", "title", " "))
                             ),
                             fluidRow(
                                column(4, uiOutput("y_range")),
                                column(4, uiOutput("x_range"))
                             ),
                             fluidRow(
                                column(4, sliderInput("alpha", "Transparency", min = 0, max = 1, 
                                                      format = "##0.##", value = .2, step = .01)),
                                column(4, checkboxInput('lines', 'Display lines for CI', FALSE)),
                                column(4,selectizeInput('cilty',  'CIs', choices = lty, "dotted"))
                             ),
                             checkboxInput('breaks', 'Define axis breaks', FALSE),
                             conditionalPanel(condition = "input.breaks == true",
                                              fluidRow(
                                                 column(4, textInput("xbreaks", "X-breaks:", "")),
                                                 column(4, textInput("ybreaks", "Y-breaks:", ""))
                                              ),
                                              helpText("Note: breaks should be passed as numbers separated by , ex: .5, 1, 1.5, 2"),
                                              actionButton("update", "Update")
                             ),
                             conditionalPanel(condition = "input.hist == true",
                                              hr(),
                                              uiOutput("binwidth")
                                              ),
                             fluidRow(
                                column(4,
                                       conditionalPanel(condition = "input.mod_categorical == true",
                                                        selectizeInput('catlty',  'Categories', choices = lty))
                                ),
                                column(4,
                                       conditionalPanel(condition = "input.mod_linear == true",
                                                        selectizeInput('linlty',  'Linear Trend', choices = lty))
                                ),                                
                                column(4,
                                       conditionalPanel(condition = "input.linspl == true",
                                                        selectizeInput('linspllty',  'Linear splines', choices = lty))
                                )
                             ),
                             fluidRow(
                                column(4,
                                       conditionalPanel(condition = "input.quadrspl == true",
                                                        selectizeInput('quadrspllty',  'Quadratic Splines', choices = lty))
                                ),
                                column(4,
                                       conditionalPanel(condition = "input.cubspl == true",
                                                        selectizeInput('cubspllty',  'Cubic Splines', choices = lty))
                                ),                                
                                column(4,
                                       conditionalPanel(condition = "input.rcubspl == true",
                                                        selectizeInput('rcubspllty',  'Restricted Cubic Splines', choices = lty))
                                )
                             ),
                             fluidRow(
                                column(4,
                                       conditionalPanel(condition = "input.mod_polynomial == true",
                                                        selectizeInput('pollty',  'Polynomials', choices = lty))
                                ),
                                column(4,
                                       selectizeInput('knotlty',  'Knots', choices = lty)
                                )
                             )
            )
         )
      )
   ),
   
   tabPanel(
      "Predictions",
      mainPanel(
         downloadButton('downloadPred', 'Download Prediction'),
         hr(),
         checkboxInput('valuePred', 'Choose predictions', FALSE),
         conditionalPanel(condition = "input.valuePred == true",
                          textInput("valpred", "Choose values:", ""),
                          helpText("Note: values should be passed as numbers separated by , ex: .5, 1, 1.5, 2"),
                          actionButton("updatePred", "Update")),
         hr(),
         tableOutput("prediction")
      )
   ),
   
   ## Analytical results
   tabPanel(
      "R output",
      mainPanel(
         h2(textOutput("Test dose-response")),
         tableOutput("test"),
         hr(),
         h2(textOutput("selectmod")),
         conditionalPanel("input.mod_categorical == true",
                          p(strong("Categorical Trend")),
                          verbatimTextOutput(outputId = "mod_categorical_text")
         ),
         conditionalPanel("input.mod_linear == true",
                          p(strong("Linear Trend")),
                          verbatimTextOutput(outputId = "mod_linear_text")
         ),
         conditionalPanel("input.linspl == true",
                          p(strong("Linear Spline Trend")),
                          verbatimTextOutput(outputId = "mod_linspl_text")
         ),
         conditionalPanel("input.quadrspl == true",
                          p(strong("Quadratic Spline Trend")),
                          verbatimTextOutput(outputId = "mod_quadrspl_text")
         ),
         conditionalPanel("input.cubspl == true",
                          p(strong("Cubic Spline Trend")),
                          verbatimTextOutput(outputId = "mod_cubspl_text")
         ),
         conditionalPanel("input.rcubspl == true",
                          p(strong("Restricted Cubic Spline Trend")),
                          verbatimTextOutput(outputId = "mod_rcubspl_text")
         ),
         conditionalPanel("input.mod_polynomial == true",
                          p(strong("Polynomial Trend")),
                          verbatimTextOutput(outputId = "mod_poly_text")
         )
      )
   )

))
