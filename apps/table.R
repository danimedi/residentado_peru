library(shiny)
library(here)

dat <- read.csv(here("data", "clean", "final_data.csv"))

ui <- fluidPage(
  dataTableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderDataTable(dat)
}

shinyApp(ui, server)
