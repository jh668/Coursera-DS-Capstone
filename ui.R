library(shiny)

shinyUI(fluidPage(
    titlePanel("Prediction of Next Word"),
    sidebarLayout(
        sidebarPanel(
            h3("Instructions"),
            h5("1. Please type your phrase in the text box and press 'Submit'."),
            h5("2. The app will predict the next word showing under the text box."),
            a("Github Link", href = "https://github.com/jh668/Coursera-DS-Capstone")
            ),
        mainPanel(
            h3("Type in your words"),
            textInput("box1", "Enter Text:", value = ""),
            submitButton("Submit"),
            h4("Prediction of next word"),
            textOutput("out")
        )
    )
))



