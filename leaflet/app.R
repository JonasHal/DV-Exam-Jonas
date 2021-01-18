#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(leafletDK)
library(rgdal)
library(dplyr)
library(DT)

dc <- read_delim(here("Data-Epidemiologiske-Rapport-05102020-kl14", "Municipality_cases_time_series.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dc %<>%
    # use pivot_longer to transpose all columns (apart from date_sample) to two columns ('kommune',
    #  'testsConducted'
    #'casesDiagnosed')
    #that hold the name of the kommune and the values of the number of diagnosed cases
    pivot_longer(cols = !date_sample, names_to = "kommune", values_to = "casesDiagnosed") %>%
    arrange(kommune, date_sample)

dc$kommune <- str_replace(dc$kommune, "Copenhagen", "København")

bins <- c(1, 2, 5, 10, 25, 50, 100, 500)
pal <- colorBin("YlOrRd", domain = c(0, 500), bins = bins)

#dashboard
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Covid-19 Cases Diagnosed Denmark"),
    dashboardSidebar(
        sliderInput("date", label = "Date",
                    min = min(dc$date_sample),
                    max = max(dc$date_sample),
                    value = min(dc$date_sample),
                    sep = "",
                    step = 1
                        )
    ),
    dashboardBody(
        fluidRow(box(width = 12, leafletOutput(outputId = "covidmap"))),
        fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data_input <- dc %>%
        filter(date_sample >= input$date_sample)

    data_input_ordered <- reactive({
        data_input()[order(match(data_input()$date_sample, dc$kommune))]
    })

    labels <- reactive({
        paste("<p>", data_input_ordered()$kommune, "</p>",
              "<p>", "Cases Diagnosed:", data_input_ordered()$casesDiagnosed, "</p>")
    })

    output$covidmap <- renderLeaflet(
        municipalityDK("casesDiagnosed", "kommune", data = dc, legend=T)
    )

    output$summary_table <- renderDataTable(data_input())
}

# Run the application
shinyApp(ui = ui, server = server)
