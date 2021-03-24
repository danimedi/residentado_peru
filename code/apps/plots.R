library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(here)

dat <- tibble(read.csv(here("data", "clean", "final_data.csv"))) %>% 
  filter(!is.na(Nota.Final))

i <- dat$Sede %>% 
  str_detect("Instituto Nacional de RehabilitaciÃ³n \"Dra. Adriana Rebaza Flores\" Amistad PerÃº - JapÃ³n") %>% 
  which()
dat$Sede[i] <- "Instituto Nacional de Rehabilitacion Peru-Japon"

i <- dat$Sede %>% 
  str_detect("Instituto Nacional Cardiovascular Carlos A. Peschiera - INCOR") %>% 
  which()
dat$Sede[i] <- "INCOR"

cols_var <- c("Universidad", "Especialidad.SubEspecialidad", "Sede")
cols_num <- c("Nota.Final", "Serum", "Prom.Pre", "ENAM", "Examen")

ui <- fluidPage(
  selectInput("variable", "Select variable", cols_var),
  selectInput("score", "Select qualification", cols_num),
  plotOutput("boxplot")
)

server <- function(input, output, session) {

  group_dat <- reactive({
    x <- dat %>% 
      filter(!is.na(.[[input$variable]]))
    i <- x %>% 
      group_by(.[[input$variable]]) %>% 
      summarize(median = median(Nota.Final)) %>% 
      arrange(desc(median)) %>% 
      {if (nrow(.) > 80) .[1:80,1] else .[,1]} %>% 
      unlist() %>% 
      .[!is.na(.)]
    filter(x, x[[input$variable]] %in% i) %>% group_by(.[[input$variable]])
  })
  output$boxplot <- renderPlot(
    ggplot(group_dat()) + 
      geom_boxplot(aes(reorder(.data[[input$variable]], .data[[input$score]], FUN = median), .data[[input$score]])) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

shinyApp(ui, server)
