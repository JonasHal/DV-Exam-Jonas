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
        menuItem("Restrictions and R-number", tabname = "restrictions"),
        menuItem("Admitted and New Cases")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "incidence",
          fluidRow(
            box(
              width = 3,
              dateInput("date", label = "Select a Date:",
                      min = min(dc$date_sample),
                      max = max(dc$date_sample) - 2,
                      value = max(dc$date_sample) - 2,
              ),
              h2("Top 10 Incidence", align="center"),
              tableOutput("top")
            ),
            box(
              width = 9,
              leafletOutput("map", height = "650")
            )
          )
        ),
        tabItem(
          tabName = "restrictions",
          fluidRow(
            box(
              h1("R and restrictions")
            )
          )
        )



      )
    )
  )
)
