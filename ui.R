#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Covid Overview"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Incidence Map", tabName = "incidence"),
        menuItem("Restrictions and R-number", tabName = "restrictions"),
        menuItem("Admitted and New Cases", tabName = "admitted")
      )
    ),
    dashboardBody(
      # tags$head(tags$style(HTML('
      #   .skin-blue .main-header .logo {
      #     background-color: #e16d3d;
      #   }
      #   .skin-blue .main-header .navbar {
      #                         background-color: #fea469;
      #   }
      # '))),
      tabItems(
        tabItem(
          tabName = "incidence",
          fluidRow(
            box(
              width = 3,
              dateInput("date", label = "Select a Date:",
                      min = min(dc$date_sample),
                      max = max(dc$date_sample) - 2,
                      value = max(dc$date_sample) - 2
              ),
              h3("Top 5 Incidence", align="center"),
              tableOutput("top") %>%
                withSpinner(color = "red", type = 4),
              br(), br(), br(),
              h4("Municipality Overtime", align="center"),
              plotlyOutput("kommune", height = "180")
            ),
            box(
              width = 9,
              leafletOutput("map", height = "650") %>%
                withSpinner(color = "red", type = 4)
            )
          )
        ),
        tabItem(
          tabName = "restrictions",
          fluidRow(
            box(
              width = 12,
              plotlyOutput("rplot", height = "320") %>%
                withSpinner(color = "red", type = 4),
              plotOutput("timeline", height = "280") %>%
                withSpinner(color = "red", type = 4),
              h4("Timeline of Restrictions")
            )
          )
        ),
        tabItem(
          tabName = "admitted",
          fluidRow(
            box(
              width = 3,
              dateRangeInput("dateRange2", label = "Select a range:",
                             start = min(dm$Dato),
                             end = max(dm$Dato),
                             min = min(dm$Dato),
                             max = max(dm$Dato),
              ),
              br(),
              checkboxGroupInput("selectAdmitted", label = "Currently Showing:",
                                 choices = list("New Cases" = 1,
                                                "New Admitted" = 2),
                                 selected = c(1,2)
              ),
              br(),
              selectInput("selectAgg", label = "Select datatype:",
                          c("Daily" = "D",
                            "Cumulative 1 week" = "W",
                            "Cumulative Total" = "T"),
                          selected = "W"
              )
            ),
            box(
              width = 9,
              plotOutput("admittedPlot", height = "650") %>%
                withSpinner(color = "red", type = 1)
            )
          )
        )

        #Insert Here for more Pages
      )
    )
  )
)
