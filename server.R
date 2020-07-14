library(shiny)
library(dplyr)
library(tidyr)
library(tm)

source("next_word_model.R")

shinyServer(function(input, output) {
    output$out <- reactive({
            validate(
                    need(input$box1, "Please type in your words in the above textbox")
            )
            next_word <- next_word(input$box1)
    })
    
})
