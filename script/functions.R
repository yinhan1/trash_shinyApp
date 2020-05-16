
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





#### function for tab 3.5: magnititude and trend ---------- ####

tab3_pick_data <- function(year, type){
  
  cols_to_keep = c("stationid","stratum","county","areaweight","type","count")
  
  if (year == 2018 & type == "Ocean"){
    read.csv("data/ocean_2018.csv") %>% select(cols_to_keep) %>% na.omit()
  } else if (year == 2018 & type == "River") {
    read.csv("data/river_2018.csv") %>% select(cols_to_keep) %>% na.omit()
  } else if (year == 2013 & type == "Ocean") {
    read.csv("data/ocean_2013.csv") %>% select(cols_to_keep) %>% na.omit()
  } else if (year == 2013 & type == "River") {
    read.csv("data/river_2013.csv") %>% select(cols_to_keep) %>% na.omit()
  } else {
    NULL
  } 
}

#### total trash count #### 

define_By = function(data, groupBy){
  data$By = 
    if(groupBy == "county"){
      as.character(data$county)
    } else if (groupBy == "stratum"){
      as.character(data$stratum)
    }else{as.character(data$type)}
  return(data)
}

tab3_total_cnt_tb = function(data, groupBy){
  tb = data %>% 
    filter(type != "None") %>% 
    define_By(data = ., groupBy = groupBy) %>% 
    rowwise() %>% 
    group_by(By) %>% 
    summarise(count = sum(count)) %>% 
    ungroup()
  tb %>% 
    add_row(
      By = "Total", 
      count = sum(tb$count),
    )
}

tab3_total_cnt_plotter = function(Type, groupBy){
  compare_counties = 
    if(Type == "River"){c('Ventura', 'Los Angeles', 'Orange', 'San Diego', 'Total')
    }else{"unknown"}
  compare_stratums = 
    if(Type == "River"){c('Agriculture', 'Open', 'Urban', 'Total')
    }else{c('Bays', 'Inner Shelf', 'Mid Shelf','Outer Shelf', 'Upper Slope','Total')}
  compare_trashType = 
    if(Type == "River"){c("Biodegradable","Biohazard","Construction","Fabric Cloth","Glass","Large","Metal","Miscellaneous","Plastic","Toxic","Total")
      }else{c("Biodegradable","Fabric Cloth", "Metal","Miscellaneous","Plastic","Recyclable","Total")}
  
  set_levels = 
    if (groupBy == "county") {
      compare_counties
    } else if (groupBy == "stratum") {
      compare_stratums
    } else {
      compare_trashType
    }
  
  bind_rows(
    tab3_pick_data(2018, Type) %>% tab3_total_cnt_tb(groupBy = groupBy) %>% mutate(year = 2018),
    tab3_pick_data(2013, Type) %>% tab3_total_cnt_tb(groupBy = groupBy) %>% mutate(year = 2013)
  ) %>% 
    filter(By != "MPA") %>% 
    filter(By %in% set_levels) %>% 
    filter(By != 'Total') %>% 
    mutate(By = factor(By, levels = set_levels)) %>% 
    ggplot(aes(x = By, y = count, group = year, fill = as.character(year))) + 
    geom_col(position = position_dodge(preserve = "single")) +
    geom_text(aes(label = count), position = position_dodge(width = .9), vjust = -0.1, size = 3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsci::scale_fill_jco() +
    labs(
      x = '',
      y = 'Total Count',
      title = 'Raw Counts Comparison 2013 vs 2018',
      fill = 'Year'
    )
}

#### percent of area covered by trash #### 

tab3_pArea_covered_tb = function(data, groupBy){
  tb = data %>% 
    define_By(groupBy = groupBy) %>% 
    group_by(stationid, By) %>%
    summarise(
      count = sum(count),
      area = sum(areaweight)
    ) %>%
    mutate(trash_present = ifelse(count == 0, 0, 1)) %>%
    group_by(By) %>% 
    summarise(
      total_area = sum(area), 
      area_no_trash = sum(area[trash_present == 0]),
      area_with_trash = sum(area[trash_present == 1])
    ) 
  
  tb %>% 
    add_row(
      By = ifelse(groupBy=="county", 'All counties', 'All stratum'), 
      total_area = sum(tb$total_area),
      area_no_trash = sum(tb$area_no_trash),
      area_with_trash = sum(tb$area_with_trash)
    ) %>% 
    mutate(
      p_area_trash = area_with_trash/total_area,
      p_label = scales::percent(p_area_trash)
    ) 
}

tab3_pArea_covered_plotter = function(Type, groupBy){
  compare_counties = 
    if(Type == "River"){c('Ventura', 'Los Angeles', 'Orange', 'San Diego', 'All counties')
    }else{"unknown"}
  compare_stratums = 
    if(Type == "River"){c('Agriculture', 'Open', 'Urban', 'All stratum')
    }else{c('Bays', 'Inner Shelf', 'Mid Shelf', 'Outer Shelf', 'Upper Slope', "All stratum")}
  
  set_levels = if(groupBy=="county"){compare_counties}else{compare_stratums}
  
  bind_rows(
    tab3_pick_data(2018, Type) %>% tab3_pArea_covered_tb(groupBy = groupBy) %>% mutate(year = 2018),
    tab3_pick_data(2013, Type) %>% tab3_pArea_covered_tb(groupBy = groupBy) %>% mutate(year = 2013)
    ) %>% 
    filter(By != "MPA") %>%
    filter(By %in% set_levels) %>% 
    mutate(
      year = factor(year, levels = c('2013', '2018')),
      By = factor(By, levels = set_levels)
    ) %>% 
    ggplot(aes(x = By, y = p_area_trash, fill = year, group = year)) +
    geom_col(position = 'dodge') +
    geom_text(aes(label = p_label), position = position_dodge(0.9), vjust = -0.1, size = 3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    ggsci::scale_fill_jco() +
    labs(
      x = '',
      y = 'Relative Percent Trash Area',
      title = 'Percentage of Area with Trash',
      fill = "Year"
    )
}

#### relative abundance #### 

tab3_rel_abun_plotter = function(Year, Type){
  tab3_pick_data(Year, Type) %>%
    filter(type != 'None') %>% 
    filter(type != "Marine_Origniated") %>% 
    group_by(stratum, type) %>%
    summarise(count = sum(count)) %>% 
    group_by(stratum) %>%
    mutate(relative_p = count/sum(count)) %>%
    filter(count>0) %>% 
    ggplot(aes(x = stratum, y = relative_p, fill = type)) +
    geom_col(position = 'stack') +
    geom_text(aes(label = paste0(round(relative_p,4)*100,"%")), size = 3,
              position = position_stack(vjust = 0.5)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsci::scale_fill_jco() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = '',
      y = 'Relative %',
      title = paste(Type, Year),
      fill = ''
    )
}















#### functions for tab 4: distance to nearest road -------- ####


#### functions for tab 5: data ------------------------ ####



#### functions for tab 6: summary -------------------- ####







