library(shiny)
library(dplyr)
library(ggplot2)
library(here)

dat <- read.csv(here("data", "clean", "final_data.csv"))
dat <- mutate(dat, match = !is.na(Sede))

cols_to_select <- c("Universidad", "Especialidad.SubEspecialidad")

ui <- fluidPage(
  selectInput("variable", "Select variable", cols_to_select),
  plotOutput("barplot")
)

server <- function(input, output, session) {
  output$barplot <- renderPlot({
    ggplot(dat) + 
      geom_bar(aes(.data[[input$variable]], fill = match), position = "dodge") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

shinyApp(ui, server)
