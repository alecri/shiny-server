# Alessio Crippa
# September 2016

# This is the server portion of a shiny app that "mimics" a Google form, in the
# sense that it lets users enter some predefined fields and saves the answer
# as a .csv file. Every submission is saved in its own file, so the results
# must be concatenated together at the end

library(shiny)
library(shinyjs)
library(digest)
library(dplyr)
library(DT)
source("www/helpers.R")


shinyServer(function(input, output, session) {
   
   observe({
      # check if all mandatory fields have a value
      mandatoryFilled <-
         vapply(fieldsMandatory,
                function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
                },
                logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
   })
   
   formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
   })
   
   saveData <- function(data) {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))
      
      write.csv(x = data, file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
   }
   
   # action to take when submit button is pressed
   observeEvent(input$submit, {
      saveData(formData())
   })
   
   # action to take when submit button is pressed
   observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
         saveData(formData())
         shinyjs::reset("form")
         shinyjs::hide("form")
         shinyjs::show("thankyou_msg")
      },
      error = function(err) {
         shinyjs::html("error_msg", err$message)
         shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
         shinyjs::enable("submit")
         shinyjs::hide("submit_msg")
      })
   })
   
   observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
   }) 
   
   output$responsesTable <- DT::renderDataTable(
      loadData(),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
   )
   
   output$downloadBtn <- downloadHandler(
      filename = function() { 
         sprintf("mimic-google-form_%s.csv", humanTime())
      },
      content = function(file) {
         write.csv(loadData(), file, row.names = FALSE)
      }
   )   
})
