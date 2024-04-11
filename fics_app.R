library(shiny)
library(shinyWidgets)
#library(shinydashboard)

options(scipen = 999)

library(tidyverse)
library(lubridate)
library(arrow)
library(DT)

source("scripts/facts_dims_filters.R")
 
fics_disponibles <-  read_parquet("datos_tablero/fics_disponibles.parquet")
fics_dims <-  read_parquet("datos_tablero/fics_dims.parquet")


ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
             box(width = 12,DTOutput('plot1'))
       
        )
    )
)

server <- function(input, output) {
    output$plot1 <- renderDT(
        fics_disponibles,
        options = list(dom = 'Bfrtip',
                       scrollY = 750,
                       #scroller = TRUE,
                       scrollX=TRUE,
                       pageLength = 100
        )
    )
}

shinyApp(ui, server)
