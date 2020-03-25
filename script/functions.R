
library(tidyverse)
library(readxl)

library(leaflet)
library(RCurl)
library(rjson)
library(jsonlite)



#### functions for tab 1: background ------------------ ####




#### functions for tab 2: station mapper --------------- ####

tab2_call_map <- function(type, year){
    tab2_mapper(read.csv("./data/2018/Ocean/ocean_2018_map.csv"))
}

tab2_mapper <- function(data){
  data %>% 
    leaflet() %>% 
    addTiles() %>%
    addMarkers(lng = ~ longitude, 
               lat = ~ latitude,
               popup = ~ abundance)
}



#### functions for tab 3 ---------------------------- ####

tab3_call_river_2013 <-
  function() {
    read.csv(
      "./data/2013/River/Bight_2013_Regional_Survey__Trash_and_Debris_in_Rivers.csv"
    ) %>%
      mutate(
        sampledate = as.Date(sampledate),
        stratum = if_else(
          as.character(stratum) == 'Ag',
          'Agriculture',
          as.character(stratum)
        )
      )
  }


#### functions for tab 4: distance to nearest road -------- ####



#### functions for tab 5: data ------------------------ ####

tab5_call_tb <- function(type, year){
  if (year == 2013 & type == "River") {
    read.csv("./data/2013/River/Bight_2013_Regional_Survey__Trash_and_Debris_in_Rivers.csv")
  }
  else if (year == 2013 & type == "Ocean") {
    read_excel("./data/2013/Ocean/Debris Ocean 2013.xlsx")
  }
  else if (year == 2018 & type == "River") {
    read.csv("./data/2018/River/river_2018.csv")
  }
  else if (year == 2018 & type == "Ocean") {
    read_excel("./data/2018/Ocean/StationCompletionV8.xlsx")
  }
  else{
    NULL
  }
}


#### functions for tab 6: summary -------------------- ####







