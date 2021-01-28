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

    cs <- Process_timeline(cs)

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
    new = c("Municipality", "New Cases", "Pr. 100.000")

    #Top 10 Incidence table on the select date
    output$top <- renderTable({
        filter <- dk_data[dk_data$date_sample == input$date[1], ]
        table_top <- filter[ ,c("kommune", "casesDiagnosed", "casesDPer100k")]

        table_top %>%
            arrange(desc(casesDPer100k)) %>%
            rename_at(vars(all_of(old)), ~ new) %>%
            head(n = 5L)
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
                layerId = ~kommune,
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

    observeEvent(input$map_shape_click, {
        output$kommune <- renderPlotly({
            kommune_data <- dk_data[dk_data$kommune == input$map_shape_click$id, c("kommune", "date_sample", "casesDPer100k")]
            kommune_data %<>%
                arrange(date_sample) %>%
                mutate(Percent_Infected = cumsum(casesDPer100k) / 1000)
            kommune_data <- kommune_data[kommune_data$date_sample < input$date[1], ]
            kommune_plot <- ggplot(kommune_data, aes(x=date_sample,
                                                     y=Percent_Infected)) +
                geom_line(color = "#a3573a") +
                ggtitle(input$map_shape_click$id) +
                labs(x = "", y = "Population Infected (%)")

        })
    })



    #Restrictions Code

    output$timeline <- renderPlot({
        #Creates the months
        day_buffer <- 1
        month_date_range <- seq(min(cs$date_sample) - days(day_buffer),
                                max(cs$date_sample) + days(day_buffer),
                                by='month')
        month_format <- format(month_date_range, '%b')
        month_df <- data.frame(month_date_range, month_format)

        #Creates the years
        year_date_range <- seq(min(cs$date_sample) - months(day_buffer),
                               max(cs$date_sample) + months(day_buffer),
                               by='year')
        year_date_range <- as.Date(intersect(
             ceiling_date(year_date_range, unit="year"),
             floor_date(year_date_range, unit="year")
         ),
         origin = "1970-01-01"
        )
        year_format <- format(year_date_range, '%Y')
        year_df <- data.frame(year_date_range, year_format)


        #### PLOT ####

        timeline_plot <- ggplot(cs, aes(x=date_sample,
                                        y=0,
                                        label=event_description)) +
            labs(col="event_description") +

            # Plot horizontal black line for timeline
            geom_hline(yintercept=0,
                       color = "black",
                       size=0.3) +
            theme_classic() +
             # Plot vertical segment lines for milestones
            geom_segment(data=cs, aes(y=position,
                                      yend=0,
                                      xend=date_sample),
                         color='black',
                         size=0.2) +
            # Plot scatter points at zero and date
            geom_point(aes(y=0), size=3) +
            # Remove axes
            theme(axis.line.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x =element_blank(),
                  axis.ticks.x =element_blank(),
                  axis.line.x =element_blank(),
                  legend.position = "bottom") +
            # Show text for each month
            geom_text(data=month_df, aes(x=month_date_range,
                                         y=-0.16,
                                         label=month_format),
                      size=4,
                      vjust=0.5,
                      color='black',
                      angle=90) +
            # Show year text
            geom_text(data=year_df, aes(x=year_date_range,
                                        y=-0.4,
                                        label=year_format,
                                        fontface="bold"),
                      size=4,
                      color='black') +
            # Show text for each milestone
            geom_text(aes(y=text_position,
                          label=event_description),
                      size=4)
        timeline_plot
    })

    #Create the reproduction number graf with uncertainties
    output$rplot <- renderPlotly({
        plot <- rt %>%
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

    #Admitted Code
    dm <- Process_dm(dm)

    #Extract the regions data from df_dk_covid
    regions_merge <- as.data.table(df_dk_covid[ ,c("date_sample", "casesDiagnosed", "NAME_1")])
    regions_merge$region <- as.character(regions_merge$NAME_1)

    #merging the admitted table with the region data and group with aggregation
    dm_merged <- regions_merge %>%
        group_by(date_sample, region) %>%
        summarise(newCases = sum(casesDiagnosed)) %>%
        inner_join(dm, by = c("region" = "region", "date_sample" = "Dato"))

    #Determine the data for 1 day, 1 week cum or cumsum of entire time
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

    #Selects what data that should be plotted
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
                    geom_area(aes(x=date_sample, y=newCases), alpha=0.2, color="#8D2D2B") +
                    geom_line(aes(x=date_sample, y=newCases), color="#8D2D2B") +
                    labs(x = "", y = "Cases") +
                    theme(axis.title.y = element_text(color="#8D2D2B", size = 14))
            } else if (input$selectAdmitted==2){
                daterange2() %>%
                    ggplot() +
                    facet_wrap(~region, nrow = 5, ncol = 1) +
                    geom_area(aes(x=date_sample, y=newlyAdmitted), alpha=0.2, color="blue") +
                    geom_line(aes(x=date_sample, y=newlyAdmitted), color="blue") +
                    labs(x = "", y = "Admitted") +
                    theme(axis.title.y = element_text(color="blue", size = 14))
            }
        } else {
            daterange2() %>%
                ggplot() +
                facet_wrap(~region, nrow = 5, ncol = 1, scales = "free_y") +
                geom_area(aes(x=date_sample, y=newCases), alpha=0.2, color="#8D2D2B") +
                geom_line(aes(x=date_sample, y=newCases), color="#8D2D2B") +
                geom_area(aes(x=date_sample, y=newlyAdmitted * 20), alpha=0.2, color="blue") +
                geom_line(aes(x=date_sample, y=newlyAdmitted * 20), color="blue") +
                scale_y_continuous(sec.axis = sec_axis(~./20, name = "Admitted")) +
                labs(x = "", y = "Cases") +
                theme(axis.title.y = element_text(color="blue", size = 14),
                      axis.title.y.left = element_text(color="#8D2D2B",  size = 14),
                      axis.text.y = element_text(color="blue"),
                      axis.text.y.left = element_text(color="#8D2D2B"))
        }
    })

}
