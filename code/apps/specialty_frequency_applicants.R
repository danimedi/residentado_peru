library(shiny)
library(ggplot2)
library(dplyr)

dat <- read.csv("data/clean/deidentified/especialidad_aplicantes.csv")

ui <- fluidPage(
  selectInput("year", "Select year", unique(dat$year)),
  plotOutput("bar_chart")
)

server <- function(input, output, session) {
  # obtain the counts of the specialties
  proces_dat <- reactive({
    dat %>% 
      filter(year == input$year) %>% 
      group_by(Especialidad.SubEspecialidad) %>% 
      count() %>% 
      arrange(n)
  })
  
  output$bar_chart <- renderPlot({
    ggplot(proces_dat(), aes(reorder(Especialidad.SubEspecialidad, n), n)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

shinyApp(ui, server)

