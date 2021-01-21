#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(
  fluidPage( includeCSS("custom.css"),
  headerPanel("DK Covid cases overview"),
  sidebarPanel(
    dateInput("date", label = "Date",
                min = min(dc$date_sample),
                max = max(dc$date_sample),
                value = max(dc$date_sample),
    ),
    tags$hr()
    # checkboxInput('header', 'Header', TRUE),
    # radioButtons('sep', 'Separator',
    #              c(Comma=',',
    #                Semicolon=';',
    #                Tab='\t'),
    #              selected = ';'),
    # radioButtons('quote', 'Quote',
    #              c(None='',
    #                'Double Quote'='"',
    #                'Single Quote'="'"),
    #              'Double Quote')
  ),
  mainPanel(
    leafletOutput("map", height = "900"),
   tags$footer()
  )
))
