library(here)
library(tidyverse)
library(roll)
library(dplyr)
library(magrittr)
library(leaflet)
library(sf)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(data.table)

# Load Used Data
dc <- read_delim(here("Data", "Municipality_cases_time_series.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dt <- read_delim(here("Data", "Municipality_tested_persons_time_series.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dsize <- read_delim(here("Data", "Municipality_test_pos.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dk <- st_read("shapefiles/gadm36_DNK_2.shp")
rt <- read_delim(here("Data", "Rt_cases.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dm <- read_delim(here("Data", "Newly_admitted_over_time.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
#coronasource <- read_delim("kilder.csv", ",", escape_double = FALSE, trim_ws = TRUE)

#Replace names to fit the Polygons
dk$NAME_2 <- str_replace(dk$NAME_2, "Århus", "Aarhus")
dk$NAME_2 <- str_replace(dk$NAME_2, "Høje Taastrup", "Høje-Taastrup")
dk$NAME_2 <- str_replace(dk$NAME_2, "Vesthimmerland", "Vesthimmerlands")

#Fix the column with the total population of each municipaty
dsize <- dsize %>%
  select(contains("Kom"), Befolkningstal) %>%
  rename("kID" = `Kommune_(id)`, "kommune" = `Kommune_(navn)`, population = Befolkningstal) %>%
  mutate(population = population * 1000) %>%
  select(-kID)

#Fix the kommaseperator
rt$estimate <- as.double(gsub("\\,", ".", rt$estimate))
rt$uncertainty_lower <- as.double(gsub("\\,", ".", rt$uncertainty_lower))
rt$uncertainty_upper <- as.double(gsub("\\,", ".", rt$uncertainty_upper))

ProcessData <- function(dc) {
  # re-formatting the dc dataframe
  dc %<>%
    pivot_longer(cols = !date_sample, names_to = "kommune", values_to = "casesDiagnosed") %>%
    arrange(kommune, date_sample)

  # replacing dots with hyphen
  dc$kommune <- gsub("\\.", "-", dc$kommune)

  # # re-formatting the dt dataframe
  # dt %<>%
  #   pivot_longer(cols = !PrDate_adjusted, names_to = "kommune", values_to = "testsConducted") %>%
  #   arrange(kommune, PrDate_adjusted) %>%
  #   rename(date_sample = PrDate_adjusted)
  #
  # # check Kommune name can be used as key
  # unique(dc[!(dc$kommune %in% dsize$kommune), ]$kommune)
  # unique(dsize[!(dsize$kommune %in% dc$kommune), ]$kommune)
  # unique(dt[!(dt$kommune %in% dsize$kommune), ]$kommune)
  #
  # # ooops!
  # for some reason one data frame uses København the other Copenhagen),
  dc$kommune <- str_replace(dc$kommune, "Copenhagen", "København")

  # merge data together
  dc <- merge(dc, dsize)

  # create new variables
  dc %<>%
    group_by(kommune) %>%
    mutate(
      casesDPer100k = casesDiagnosed / (population / 100000),
    )

  dc$casesDiagnosed <- as.integer(dc$casesDiagnosed)

  return(dc)
}

Process_sf <- function(dk) {
  # including population to the shapefile
  dk_pop <-
    dsize %>%
    left_join(dk, by = c("kommune" = "NAME_2"))

  # transforming dk_pop to a shapefile
  sf_dk <- st_as_sf(dk_pop, sf_column_name = "geometry")

  # getting the centroids to grab the coordinates from the shapefile - results
  # in a matrix
  dk_cent <- st_centroid(sf_dk)
  dk_coords <- st_coordinates(dk_cent)

  # converting the matrix into a df again
  dk_coords_next <- as.data.frame(dk_coords)

  # adding the coordinates to the kommunes and their population
  dk_merge_coords <-
    dsize %>%
    cbind(dk_coords_next)

  # merging the coordinates into the shapefile
  dk_merge_coords <-
    dk_merge_coords %>%
    merge(sf_dk)

  return(dk_merge_coords)
}

Process_dm <- function(dm){
  dm[["Ukendt Region"]] <- NULL

  dm %<>%
    pivot_longer(cols = !Dato, names_to = "region", values_to = "newlyAdmitted") %>%
    arrange(region, Dato)

}

# Process_timeline <- function(coronasource){
#   positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
#   directions <- c(1, -1)
#
#   line_pos <- data.frame(
#       "date"
#   )
#
#   Hvis der er mere tid så: https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
# }

