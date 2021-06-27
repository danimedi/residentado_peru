library(shiny)

explore_gender_app <- function() {

  ui <- fluidPage(
    radioButtons("incoming", "Only those who got into the specialty?",
                 choices = c(TRUE, FALSE)),
    selectInput("var", "Variable", choices = names(peru_residentado)),
    actionButton("button", "Generate graph"),
    plotOutput("graph")
  )

  server <- function(input, output, session) {

  }

  shinyApp(ui, server)
}
