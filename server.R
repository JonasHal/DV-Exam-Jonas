#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
server <- function(input, output) {

    # input$file1 will be NULL initially. After the user selects and uploads a
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
    # columns. The 'datapath' column will contain the local filenames where the
    # data can be found.

    dk_data <- ProcessData(dc)

    sf_dk <- Process_sf(dk)


    filtered <- reactive({
        df_dk_covid[df_dk_covid$date_sample == input$date[1], ]
    })

    # merging the coords/kommunes with the covid data
    dk_merge_coords_test <-
        dk_data %>%
        merge(sf_dk)

    # merging the covid data into the shapefile to plot it
    df_dk_covid <-
        dk_data %>%
        group_by(kommune) %>%
        merge(sf_dk)

    # to plot the data it needs to be a shapefile (sf) again - creating shapefile
    df_dk_covid <-
        st_as_sf(df_dk_covid, sf_column_name = "geometry")

    bins <- c(0,2,4,8,16,32,64,128,256)
    # seq(min(df_dk_covid$dcr7dPer100k)*100, max(df_dk_covid$dcr7dPer100k)*100, max(df_dk_covid$dcr7dPer100k)*100 / 5)
    #
    pal <- colorBin(
        palette = c("#ffe0c4", "#ffd3ab","#febe8d","#fea469", "#f4874c", "#e16d3d", "#a3573a", "#8D2D2B"),
        bins = bins
    )

    old = c("kommune", "casesDiagnosed", "casesDPer100k")
    new = c("Kommune", "New Cases", "Pr. 100.000")

    output$top <- renderTable({
        filter <- dk_data[dk_data$date_sample == input$date[1], ]
        table_top <- filter[ ,c("kommune", "casesDiagnosed", "casesDPer100k")]
        table_top %>%
            arrange(desc(casesDPer100k)) %>%
            rename_at(vars(all_of(old)), ~ new) %>%
            head()
        },
        spacing = "xs"
    )

    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
            setView(lng = 11.001785, lat = 56.26392, zoom = 7) %>%
            addPolygons(
                data = filtered(), color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 1,
                fillColor = ~ pal(filtered()$casesDPer100k),
                highlight = highlightOptions(weight = 3, color = "blue"),
                popup = paste0(
                    "<h5>", filtered()$kommune, "</h5>",
                    "<b>New Cases:</b> ",
                    filtered()$casesDiagnosed,
                    "<br>",
                    "<b>Incidence pr. 100000:</b> ",
                    round(filtered()$casesDPer100k, digits=0)
                )
            ) %>%
            addLegend(
                data = filtered(),
                position = "topright",
                pal = pal,
                values = ~ pal(filtered()$casesDPer100k),
                title = "Incidence pr 100.000"
            )

    })
}
