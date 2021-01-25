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


    #Incidence Code

    #Creates the coloring schema
    bins <- c(0,2,4,8,16,32,64,128,256)
    pal <- colorBin(
        palette = c("#ffe0c4", "#ffd3ab","#febe8d","#fea469", "#f4874c", "#e16d3d", "#a3573a", "#8D2D2B"),
        bins = bins
    )

    #Creates the reactiveness
    filtered <- reactive({
        df_dk_covid[df_dk_covid$date_sample == input$date[1], ]
    })

    #Renaming for the showing Top 10 incidence
    old = c("kommune", "casesDiagnosed", "casesDPer100k")
    new = c("Kommune", "New Cases", "Pr. 100.000")

    #Top 10 Incidence table on the select date
    output$top <- renderTable({
        filter <- dk_data[dk_data$date_sample == input$date[1], ]
        table_top <- filter[ ,c("kommune", "casesDiagnosed", "casesDPer100k")]
        table_top %>%
            arrange(desc(casesDPer100k)) %>%
            rename_at(vars(all_of(old)), ~ new) %>%
            head(n = 10L)
        },
        width = "100%",
        spacing = "s"
    )

    #Creates the structure for the Leaflet
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
            setView(lng = 11.001785, lat = 56.26392, zoom = 7) %>%
            addLegend(
                data = filtered(),
                position = "topright",
                pal = pal,
                values = ~ pal(filtered()$casesDPer100k),
                title = "Incidence pr 100.000"
            )
    })

    #Draws the map with the given dateinput
    observe({
        leafletProxy("map", data = filtered()) %>%
            clearShapes() %>%
            addPolygons(
                color = "#444444", weight = 1, smoothFactor = 0.5,
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
            )
    })


    #Restrictions Code

    #Create the reactiveness on the DateRange input
    daterange <- reactive({
        filter(rt, between(date_sample, input$dateRange[1], input$dateRange[2]))
    })

    #Create the plot with the given inputs from DateRange
    output$rplot <- renderPlot({
        daterange() %>%
            ggplot(aes(x=date_sample,
                       y=estimate,
                       ymin=uncertainty_lower,
                       ymax=uncertainty_upper,
                       group = 1)) +
            geom_hline(yintercept = 1.0,
                       color = "red") +
            geom_line() +
            geom_ribbon(alpha=0.2) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
            labs(x = "", y = "R-Number")
    })

    output$TBA <- renderPrint(
        print("To Be Arrived")
    )


    #Admitted Code
    dm <- Process_dm(dm)

    regions_merge <- as.data.table(df_dk_covid[ ,c("date_sample", "casesDiagnosed", "NAME_1")])
    regions_merge$region <- as.character(regions_merge$NAME_1)

    dm_merged <- regions_merge %>%
        group_by(date_sample, region) %>%
        summarise(newCases = sum(casesDiagnosed)) %>%
        inner_join(dm, by = c("region" = "region", "date_sample" = "Dato"))

    daterange2 <- reactive({
        dm_agg <- filter(dm_merged, between(date_sample, input$dateRange2[1], input$dateRange2[2]))
        if(input$selectAgg=="D"){
            dm_agg
        } else if(input$selectAgg=="W"){
            dm_agg %<>%
                group_by(region) %>%
                mutate(newCases = roll_sum(newCases, width = 7, min_obs = 1),
                       newlyAdmitted = roll_sum(newlyAdmitted, width = 7, min_obs = 1)
                )
        } else if(input$selectAgg=="T"){
            dm_agg %<>%
                group_by(region) %>%
                mutate(newCases = cumsum(newCases),
                       newlyAdmitted = cumsum(newlyAdmitted)
                )
        }
    })

    output$admittedPlot <- renderPlot({
        if(is.null(input$selectAdmitted)){
            daterange2() %>%
                ggplot() +
                facet_wrap(~region, nrow = 5, ncol = 1)
        }
        else if(length(input$selectAdmitted)==1){
            if(input$selectAdmitted==1){
                daterange2() %>%
                    ggplot() +
                    facet_wrap(~region, nrow = 5, ncol = 1) +
                    geom_area(aes(x=date_sample, y=newCases), alpha=0.2, color="red") +
                    geom_line(aes(x=date_sample, y=newCases), color="red") +
                    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
                    labs(x = "", y = "Cases")
            } else if (input$selectAdmitted==2){
                daterange2() %>%
                    ggplot() +
                    facet_wrap(~region, nrow = 5, ncol = 1) +
                    geom_area(aes(x=date_sample, y=newlyAdmitted), alpha=0.2, color="blue") +
                    geom_line(aes(x=date_sample, y=newlyAdmitted), color="blue") +
                    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
                    labs(x = "", y = "Cases")
            }
        } else {
            daterange2() %>%
                ggplot() +
                facet_wrap(~region, nrow = 5, ncol = 1, scales = "free_y") +
                geom_area(aes(x=date_sample, y=newCases), alpha=0.2, color="red") +
                geom_line(aes(x=date_sample, y=newCases), color="red") +
                geom_area(aes(x=date_sample, y=newlyAdmitted * 15), alpha=0.2, color="blue") +
                geom_line(aes(x=date_sample, y=newlyAdmitted * 15), color="blue") +
                scale_y_continuous(sec.axis = sec_axis(~./15, name = "Admitted")) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
                labs(x = "", y = "Cases")
        }
    })

}
