
library(tidyverse)
library(readxl)

library(plotrix)
library(reshape2)

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
    filter(trashcount < 50) %>% 
    leaflet() %>% 
    addProviderTiles("Esri.WorldImagery") %>% 
    # addTiles() %>%
    addCircleMarkers(lng = ~ longitude, 
                     lat = ~ latitude,
                     popup = ~ stratum,
                     radius = 15,
                     color = NA,
                     fillColor = "#CC4C02",
                     fillOpacity = ~ rescale(abundance, c(0.2,0.8)))
}

tab2_mean_count_plotter <- function(){
  data.frame(year = c(1998,2003,2008,2013,2018),
             ocean = c(10,20,30,40,50) + rnorm(5,10,10),
             river = c(1,2,3,4,10) + rnorm(5,2,3)) %>%
    melt(id.vars = "year") %>% 
    ggplot(aes(x = year, y = value, color = variable, shape = variable)) +
    geom_line(size = 1, alpha = 0.5, linetype = "dashed") +
    geom_point(stat = "identity", size = 3.5) +
    scale_color_manual(values = c("ocean"="#CC4C02", "river"="#662506")) +
    labs(x = "Year", y = "Area weighted mean count") +
    facet_wrap(variable~., scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(legend.position = "none")
}


#### functions for tab 3 ---------------------------- ####

tab3_call_river_2013 <- function() {
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







