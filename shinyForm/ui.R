# Alessio Crippa
# September 2016

# This is the ui portion of a shiny app that "mimics" a Google form, in the
# sense that it lets users enter some predefined fields and saves the answer
# as a .csv file. Every submission is saved in its own file, so the results
# must be concatenated together at the end

library(shiny)
library(shinyjs)
source("www/helpers.R")


shinyUI(fluidPage(

   shinyjs::useShinyjs(),
   shinyjs::inlineCSS(appCSS),
   
   title = "Course Basic Student Info",
   h2("Course Basic Student Info"),
   
   div(
   id = "form",
   textInput("name", "Name *", ""),
   textInput("favourite_pkg", "Favourite R package"),
   checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
   sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
   selectInput("os_type", "Operating system used most frequently",
               c("",  "Windows", "Mac", "Linux")),
   actionButton("submit", "Submit", class = "btn-primary"),
   downloadButton("downloadBtn", "Download responses"),
   DT::dataTableOutput("responsesTable"),
   shiny::hr(),
   shinyjs::hidden(
      span(id = "submit_msg", "Submitting..."),
      div(id = "error",
          div(br(), tags$b("Error: "), span(id = "error_msg"))
      )
   )),
   shiny::hr(),
   
   shinyjs::hidden(
      div(
         id = "thankyou_msg",
         h3("Thanks, your response was submitted successfully!"),
         actionLink("submit_another", "Submit another response")
      )
   )
))
