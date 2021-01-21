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

    bins <- c(0,1,4,10,25,100,250)
    # seq(min(df_dk_covid$dcr7dPer100k)*100, max(df_dk_covid$dcr7dPer100k)*100, max(df_dk_covid$dcr7dPer100k)*100 / 5)
    #
    pal <- colorBin(
        palette = c("#ffe0c4", "#ffd3ab","#febe8d","#fea469", "#f4874c", "#e16d3d", "#a3573a"),
        bins = bins
    )

    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
            setView(lng = 11.001785, lat = 56.26392, zoom = 7.5) %>%
            addPolygons(
                data = filtered(), color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 1,
                fillColor = ~ pal(filtered()$casesDiagnosed),
                popup = paste0(
                    "<h5>Date: ", format(filtered()$date_sample, "%d-%b-%y"), "</h5>",
                    "<b>Kommune:</b> ",
                    filtered()$kommune,
                    "<br>",
                    "<b>Tilfælde:</b> ",
                    filtered()$casesDiagnosed,
                    "<br>",
                    "<b>Incidens:</b> ",
                    round(filtered()$casesDPer100k, digits=0)
                )
            ) %>%
            addLegend(
                data = filtered(),
                position = "topright",
                pal = pal,
                values = ~ pal(filtered()$casesDiagnosed),
                title = "New Cases"
            )

    })
}
