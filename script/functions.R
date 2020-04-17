
library(tidyverse)
library(magrittr)
library(readxl)

library(plotrix)
library(reshape2)

library(leaflet)
library(RCurl)
library(rjson)
library(jsonlite)

source('script/tab3/area-weighted-mean-count_ocean_2018.R')
source('script/tab3/calculate-areaweight-SMC_2018.R')
source('script/tab3/river_2018_relative-percent-plot.R')
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

tab2_ocean_sites <- function(year){
  read.csv("./data/map_data_04_16_2020.csv") %>% filter(tag == paste("Ocean",year)) %>% nrow() %>% paste(., "ocean sites")
}

tab2_river_sites <- function(year){
  read.csv("./data/map_data_04_16_2020.csv") %>% filter(tag == paste("River",year)) %>% nrow() %>% paste(., "river sites")
}


tab2_call_map <- function(year){
  map_data = read.csv("./data/map_data_04_16_2020.csv") %>% 
    filter(str_detect(tag,as.character(year))) %>% 
    mutate(water = ifelse(str_detect(tag,"Ocean"), "Ocean", "River"))
  map_data %>% mutate(water = factor(water, levels = c("Ocean","River"))) %>% tab2_mapper()
}

tab2_mapper <- function(data){
  shapes = c(16, 17)
  iconFiles = pchIcons(shapes, 10, 10, col = c("blue", "red"), lwd = 2)
  
  leaflet(data) %>% 
    addTiles() %>% 
    # addMapPane(name = "OpenTopoMap", zIndex = 420) %>% 
    addMapPane(name = "CartoDB_VoyagerNoLabels", zIndex = 420) %>% 
    # addProviderTiles("Esri.WorldImagery") %>% 
    addMarkers(
      data = data,
      lng = ~ longitude, 
      lat = ~ latitude,
      popup = ~ stratum,
      icon = ~ icons(
        iconUrl = iconFiles[water],
        popupAnchorX = 20, popupAnchorY = 0
      ))
}

tab2_site_count_plotter <- function(){
  data.frame(year = c(1994,1998,2008,2013,2018),
             Ocean = c(113,242,140,164,164),
             River = c(70,70,70,273,118)) %>%
    melt(id.vars = "year") %>% 
    ggplot(aes(x = as.character(year), y = value, fill = variable, shape = variable)) +
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







