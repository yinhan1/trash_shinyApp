
library(tidyverse)
library(magrittr)
library(readxl)

library(plotrix)
library(reshape2)

library(leaflet)
library(RCurl)
library(rjson)
library(jsonlite)

# 
# source('script/tab3/area-weighted-mean-count_ocean_2018.R')
# source('script/tab3/calculate-areaweight-SMC_2018.R')
# source('script/tab3/river_2018_relative-percent-plot.R')

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
  read.csv("./data/map_data_04_30_2020.csv") %>% filter(tag == paste("Ocean",year)) %>% nrow() %>% paste(., "ocean sites")
}

tab2_river_sites <- function(year){
  read.csv("./data/map_data_04_30_2020.csv") %>% filter(tag == paste("River",year)) %>% nrow() %>% paste(., "river sites")
}


tab2_call_map <- function(year){
  map_data = read.csv("./data/map_data_04_30_2020.csv") %>% 
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
             Ocean = c(158,169,140,164,139),
             River = c(70,70,70,273,133)) %>%
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


# area_w_stratum <- trash_df %>% 
#   group_by(stratum) %>% 
#   summarise(
#     areaweight = sum(areaweight),
#     count = sum(totalcount)
#   ) %>% 
#   ggplot() +
#   geom_col(aes(x = stratum, y = areaweight, fill = stratum)) +
#   # facet_grid(.~county) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# total_count_area <- trash_df %>% 
#   group_by(county, stratum) %>% 
#   summarise(
#     `Total Area` = sum(areaweight),
#     `Total Count` = sum(totalcount)
#   ) %>% 
#   pivot_longer(
#     cols = c(`Total Count`, `Total Area`)
#   ) %>% 
#   mutate_at('name', factor, levels = c('Total Count', 'Total Area')) %>% 
#   ggplot() +
#   geom_col(aes(x = stratum, y = value, group = county, fill = stratum)) +
#   facet_grid(name ~ county, scales = 'free_y') +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
#   labs(
#     x = '',
#     y = ''
#   ) 


# trash_w_stratum <- trash_weight_stratum %>%
#   pivot_longer(
#     cols = -c(stratum)
#   ) %>% 
#   filter(name != 'plastic') %>% 
#   ggplot(aes(x = stratum, y = value, group = stratum, fill = stratum)) +
#   geom_col(position = "dodge") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
#   # facet_grid(.~ county) +
#   labs(
#     x = '',
#     y = 'Area Weighted Mean Counts'
#   )
# 
# trash_w_county <- trash_weight_county %>%
#   pivot_longer(
#     cols = -c(county)
#   ) %>% 
#   filter(name != 'plastic') %>% 
#   ggplot(aes(x = county, y = value, group = county, fill = county)) +
#   geom_col(position = "dodge") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
#   # facet_grid(.~ county) +
#   labs(
#     x = '',
#     y = 'Area Weighted Mean Counts'
#   )


#### function for tab 3.5: magnititude and trend ---------- ####

tab3_pick_data <- function(year, type){
  
  new_names = c("stationid","stratum","region","area_weight","type","count")
  
  if (year == 2018 & type == "Ocean"){
    read.csv("data/Ocean/2018_debris_counts_in_details.csv") %>%
      filter(abandoned == "No") %>% 
      select(stationid, final_stratum, region, area_weight, debris_type, debriscount) %>% 
      set_colnames(new_names) %>% 
      mutate(type = stringr::str_to_title(type))
    
  } else if (year == 2018 & type == "River") {
    read.csv("data/River/2018_river_debris_count.csv") %>% 
      select(stationid, stratum, county, area_weight, debriscategory, total_count) %>% 
      set_colnames(new_names) %>% 
      mutate(
        stratum = as.character(stratum),
        stratum = ifelse(stratum == 'Ag', 'Agriculture', stratum),
        type = stringr::str_to_title(type)
      ) %>% 
      select(new_names) %>% 
      mutate(type = stringr::str_to_title(type))
    
  } else if (year == 2013 & type == "Ocean") {
    read.csv("data/Ocean/2013_ocean_debris_count.csv") %>% 
      filter(count >= 0) %>% 
      select(stationid, stratum, region, areaweight, debriscategory, count) %>% 
      mutate(region = str_to_title(region)) %>% 
      set_colnames(new_names) %>% 
      mutate(type = stringr::str_to_title(type))
    
  } else if (year == 2013 & type == "River") {
    read.csv("data/River/2013_river_debris_count.csv") %>% 
      filter(totalcount >= 0) %>% 
      select(-c(sample_date,latitude,longitude,smcshed,location,totalcount)) %>% 
      melt(id.vars = c("stationid","stratum","area_weight","county"),
                     value.name = "count") %>% 
      mutate(type = str_remove(variable, "total"),
             region = county) %>%
      select(new_names) %>% 
      mutate(
        stratum = as.character(stratum),
        stratum = if_else(stratum == 'Ag', 'Agriculture', stratum),
        region = str_to_title(region),
        type = stringr::str_to_title(type))
  } else {
    NULL
  } 
}

# debristype = case_when(
#   debristype %in% c("Bag","Bottle","Cap/Lid","Cup", "Fishing Line/Net",
#                     "Plastic Piece (unid.)","Tire", "Polypropylene Rope",
#                     "Other Plastic (comment req.)") ~ 'Plastic',
#   debristype %in% c("Beer Bottle","Glass Bottle/Jar -other",
#                     "Can - other", "Drink Can") ~ 'Recyclable',
#   debristype %in% c("Fishing Gear", "Food Bag/Wrapper",
#                     "Other Misc. (comment req.)") ~ 'Trash',
#   debristype %in% c("Clothing","Rag/Cloth") ~ 'Fabric_Cloth',
#   debristype %in% c("Other Metal (comment req.)") ~ 'Metal',
#   debristype %in% c("Lumber","Paper","Stick/Branch/Driftwood",
#                     "Leaves/Seed Pod") ~ 'Biodegradable',
#   debristype %in% c("Other Terrestrial (comment req.)","Rock") ~ 'Rock',
#   debristype %in% c("Foliose Algae - not kelp","Gorgonian Sea Fan (dead)",
#                     "Kelp Holdfast", "Kelp Stipe/Blade","Seagrass",
#                     "Other Foliose Algae", "Other Marine (comment req.)") ~ 'Marine_Debris',
#   TRUE ~ 'Dunno'
# )



tab3_by_year_plotter <- function(year, type, groupBy, plot_tt_cnt){
  if (groupBy == "stratum"){
    tb = tab3_pick_data(year, type) %>% 
      group_by(stratum) %>%
      summarise(
        count = sum(count),
        area_weight = mean(area_weight),
        average_count = count/area_weight)
    if (plot_tt_cnt==T){
      plot = ggplot(tb) +
        geom_col(aes(x = stratum, y = count)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = '', y = 'Total count', title = 'Total count of trash per stratum') 
    } else {
      plot = ggplot(tb) +
        geom_col(aes(x = stratum, y = average_count)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = '', y = 'Count per sq.km', title = 'Area weighted mean count per stratum')
    }
  }
  if (groupBy == "county"){
    tb = tab3_pick_data(year, type) %>% 
      group_by(region) %>%
      summarise(
        count = sum(count),
        area_weight = mean(area_weight),
        average_count = count/area_weight)
    if (plot_tt_cnt==T){
      plot = ggplot(tb) +
        geom_col(aes(x = region, y = count)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = '', y = 'Total count', title = 'Total count of trash per county') 
    } else {
      plot = ggplot(tb) +
        geom_col(aes(x = region, y = average_count)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = '', y = 'Count per sq.km', title = 'Area weighted mean count per county')
    }
  }
  if (groupBy == "trashType"){
    tb = tab3_pick_data(year, type) %>% 
      group_by(type) %>%
      summarise(
        count = sum(count),
        area_weight = mean(area_weight),
        average_count = count/area_weight)
    if (plot_tt_cnt==T){
      plot = ggplot(tb) +
        geom_col(aes(x = type, y = count)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = '', y = 'Total count', title = 'Total count of trash per trash category') 
    } else {
      plot = ggplot(tb) +
        geom_col(aes(x = type, y = average_count)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = '', y = 'Count per sq.km', title = 'Area weighted mean count per trash category')
    }
  }
  return(plot)
} 


tab3_relative_plotter <- function(year, type){
  plot = tab3_pick_data(year, type) %>% 
    filter(type != 'None') %>% 
    group_by(stratum, type) %>%
    summarise(
      count = sum(count),
      area_weight = mean(area_weight)
    ) %>% 
    group_by(stratum) %>% 
    mutate(relative_p = count/sum(count)) %>%
    ggplot() +
    geom_col(aes(x = stratum, y = relative_p, fill = type), position = 'stack') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsci::scale_fill_jco() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = '',
      y = 'Relative %',
      title = 'Relative Percentage of Trash Type per Land Use Stratum',
      fill = ''
    )
  return(plot)
}

tab3_compare_plotter <- function(type, groupBy){
  data_2013 = tab3_pick_data(2013, type) %>% filter(type != 'None' & stratum != "MPA")
  data_2018 = tab3_pick_data(2018, type) %>% filter(type != 'None' & stratum != "MPA")
  if (groupBy == "stratum"){
    tb_2013 = group_by(data_2013, stratum) %>% summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
    tb_2018 = group_by(data_2018, stratum) %>% summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
    plot = 
      bind_rows(
      tb_2013 %>% mutate(year = 2013),
      tb_2018 %>% mutate(year = 2018)
    ) %>% 
      mutate(year = as.factor(year)) %>% 
      ggplot() + 
      geom_col(aes(x = stratum, y = count, group = year, fill = year), position = 'dodge') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggsci::scale_fill_jco() +
      labs(x = '', y = 'Total count', title = 'Total count of trash per stratum') 
  }
  if (groupBy == "county"){
    tb_2013 = group_by(data_2013, region) %>% summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
    tb_2018 = group_by(data_2018, region) %>% summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
    plot = 
      bind_rows(
        tb_2013 %>% filter(region %in% tb_2018$region) %>%mutate(year = 2013),
        tb_2018 %>% filter(region %in% tb_2013$region) %>% mutate(year = 2018)
      ) %>% 
      mutate(year = as.factor(year)) %>% 
      ggplot() + 
      geom_col(aes(x = region, y = count, group = year, fill = year), position = 'dodge') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggsci::scale_fill_jco() +
      labs(x = '', y = 'Total count', title = 'Total count of trash per county') 
  }
  if (groupBy == "trashType"){
    tb_2013 = group_by(data_2013, type) %>% summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
    tb_2018 = group_by(data_2018, type) %>% summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
    plot = 
      bind_rows(
        tb_2013 %>% mutate(year = 2013),
        tb_2018 %>% mutate(year = 2018)
      ) %>% 
      mutate(year = as.factor(year)) %>% 
      ggplot() + 
      geom_col(aes(x = type, y = count, group = year, fill = year), position = 'dodge') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggsci::scale_fill_jco() +
      labs(x = '', y = 'Total count', title = 'Total count of trash per trash category') 
  }
  return(plot)
}




#### functions for tab 4: distance to nearest road -------- ####



#### functions for tab 5: data ------------------------ ####



#### functions for tab 6: summary -------------------- ####







