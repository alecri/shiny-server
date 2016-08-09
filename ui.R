require(shiny)
require(plotly)

# Define the overall UI
shinyUI(

   # Use a fluid Bootstrap layout
   fluidPage(   
      
      # Give the page a title
      headerPanel(withMathJax("$$\\text{Simulation results for } R_b, I^2, \\text{ and } R_I$$")),
      
      # Generate a row with a sidebar
      sidebarLayout(      
         
         # Define the sidebar with one input
         sidebarPanel(
            sliderInput("R", label = h3("True heterogeneity (R)"),
                        min = 0.1, max = .9, step = .2, value = .5),
            hr(),
            selectInput("k", label = h3("Study size (K)"), 
                        choices = list("5" = 5, "10" = 10, "15" = 15, "20" = 20,
                                       "25" = 25, "50" = 50, "100" = 100), selected = 50),
            hr(),
            selectInput("scale", label = h3("Simulation outcome"), 
                        choices = list("Percent bias" = "bias", "Mean value" = "means"),
                        selected = "bias"),
            checkboxInput("plotly", label = "3d-plotly", value = TRUE),
            checkboxInput("loess", label = "Loess", value = TRUE),
            checkboxInput("overlaid", label = "Overlaid", value = TRUE),
            #conditionalPanel(condition = "input.overlaid == false",
            #                 checkboxInput("range", label = "Comparable range", value = TRUE)
            #                 ),
            width = 2
         ),
         
         # Create a spot for the barplot
         mainPanel(
            uiOutput("lab1"),
            uiOutput("threeD_plot"),
            width = 9)
      )
   )
)