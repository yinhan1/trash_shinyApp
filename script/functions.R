
library(tidyverse)
library(magrittr)
library(readxl)

library(plotrix)
library(reshape2)

library(leaflet)
library(RCurl)
library(rjson)
library(jsonlite)

trash_df = read.csv("./data/2013/River/Bight_2013_Regional_Survey__Trash_and_Debris_in_Rivers.csv") %>%
  mutate(
    sampledate = as.Date(sampledate),
    stratum = if_else(as.character(stratum) == 'Ag', 'Agriculture', as.character(stratum))
  )

trash_total <- trash_df %>% 
  select(
    objectid,
    stationid,
    sampledate,
    longitude,
    latitude,
    stratum,
    areaweight,
    county,
    smcshed,
    contains('total')
  )

trash_weight_county <- trash_total %>% 
  group_by(county) %>% 
  summarise(
    count = sum(totalcount * areaweight)/sum(areaweight),
    biodegradeable = sum(totalbiodegradeable * areaweight)/sum(areaweight),
    biohazard = sum(totalbiohazard * areaweight)/sum(areaweight),
    construction = sum(totalconstruction * areaweight)/sum(areaweight),
    fabric = sum(totalfabricandcloth * areaweight)/sum(areaweight),
    glass = sum(totalglass * areaweight)/sum(areaweight),
    large = sum(totallarge * areaweight)/sum(areaweight),
    metal = sum(totalmetal * areaweight)/sum(areaweight),
    misc = sum(totalmisc * areaweight)/sum(areaweight),
    plastic = sum(totalplastic * areaweight)/sum(areaweight),
    toxic = sum(totaltoxic * areaweight)/sum(areaweight)
  ) 


trash_weight_stratum <- trash_total %>% 
  group_by(stratum) %>% 
  summarise(
    count = sum(totalcount * areaweight)/sum(areaweight),
    biodegradeable = sum(totalbiodegradeable * areaweight)/sum(areaweight),
    biohazard = sum(totalbiohazard * areaweight)/sum(areaweight),
    construction = sum(totalconstruction * areaweight)/sum(areaweight),
    fabric = sum(totalfabricandcloth * areaweight)/sum(areaweight),
    glass = sum(totalglass * areaweight)/sum(areaweight),
    large = sum(totallarge * areaweight)/sum(areaweight),
    metal = sum(totalmetal * areaweight)/sum(areaweight),
    misc = sum(totalmisc * areaweight)/sum(areaweight),
    plastic = sum(totalplastic * areaweight)/sum(areaweight),
    toxic = sum(totaltoxic * areaweight)/sum(areaweight)
  ) 

#### functions for tab 1: background ------------------ ####




#### functions for tab 2: station mapper --------------- ####

pchIcons <- function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", ...){
  n = length(pch)
  files = character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = bg)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] = f
  }
  files
}

shapes = c(1, 2)
iconFiles = pchIcons(shapes, 10, 10, col = c("blue", "red"), lwd = 4)

tab2_call_map <- function(type, year){
  cols_to_take_ocean <- c("stationid","latitude","longitude","stratum")
  cols_to_take_river <- c("stationid","latitude"="new_lat","longitude"="new_long","stratum"="land_use")
  bind_rows(
    read.csv("./data/2018/Ocean/ocean_2018_map.csv") %>% select(cols_to_take_ocean) %>% mutate(water = "Ocean"),
    read.csv("./data/2018/River/river_2018_map.csv") %>% select(cols_to_take_river) %>% mutate(water = "River")
  ) %>% 
    mutate(water = as.factor(water)) %>% 
    tab2_mapper()
}

tab2_mapper <- function(data){
  leaflet(data) %>% 
    addTiles() %>% 
    addProviderTiles("Esri.WorldImagery") %>% 
    addMarkers(
      data = data,
      lng = ~ longitude, 
      lat = ~ latitude,
      popup = ~ paste("Stratum", stratum),
      icon = ~ icons(
        iconUrl = iconFiles[water],
        popupAnchorX = 20, popupAnchorY = 0
      ))
}

tab2_site_count_plotter <- function(){
  data.frame(year = c(1998,2003,2008,2013,2018),
             Ocean = c(10,20,30,25,40) + rnorm(5,10,10),
             River = c(50,50,80,90,118) + rnorm(5,2,3)) %>%
    melt(id.vars = "year") %>% 
    ggplot(aes(x = year, y = value, fill = variable, shape = variable)) +
    geom_bar(stat="identity", position ="dodge", alpha = 0.5, linetype = "dashed") +
    scale_fill_manual(values = c("Ocean"="blue", "River"="red")) +
    labs(x = "Year", y = "Number of Sites Sampled") +
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


area_w_stratum <- trash_df %>% 
  group_by(stratum) %>% 
  summarise(
    areaweight = sum(areaweight),
    count = sum(totalcount)
  ) %>% 
  ggplot() +
  geom_col(aes(x = stratum, y = areaweight, fill = stratum)) +
  # facet_grid(.~county) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

total_count_area <- trash_df %>% 
  group_by(county, stratum) %>% 
  summarise(
    `Total Area` = sum(areaweight),
    `Total Count` = sum(totalcount)
  ) %>% 
  pivot_longer(
    cols = c(`Total Count`, `Total Area`)
  ) %>% 
  mutate_at('name', factor, levels = c('Total Count', 'Total Area')) %>% 
  ggplot() +
  geom_col(aes(x = stratum, y = value, group = county, fill = stratum)) +
  facet_grid(name ~ county, scales = 'free_y') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  labs(
    x = '',
    y = ''
  ) 


trash_w_stratum <- trash_weight_stratum %>%
  pivot_longer(
    cols = -c(stratum)
  ) %>% 
  filter(name != 'plastic') %>% 
  ggplot(aes(x = stratum, y = value, group = stratum, fill = stratum)) +
  geom_col(position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  # facet_grid(.~ county) +
  labs(
    x = '',
    y = 'Area Weighted Mean Counts'
  )

trash_w_county <- trash_weight_county %>%
  pivot_longer(
    cols = -c(county)
  ) %>% 
  filter(name != 'plastic') %>% 
  ggplot(aes(x = county, y = value, group = county, fill = county)) +
  geom_col(position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  # facet_grid(.~ county) +
  labs(
    x = '',
    y = 'Area Weighted Mean Counts'
  )



#### functions for tab 4: distance to nearest road -------- ####



#### functions for tab 5: data ------------------------ ####

tab5_call_tb <- function(type, year){
  if (year == 2013 & type == "River") {
    read.csv("./data/2013/River/Bight_2013_Regional_Survey__Trash_and_Debris_in_Rivers.csv") %>% 
      select(-X, -Y)
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







