library(shiny)
library(dplyr)
library(tidyr)
library(tm)

source("next_word_model.R")

shinyServer(function(input, output) {
    output$out <- renderText({
        next_word <- next_word(input$box1)
    })
    
})
