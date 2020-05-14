
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
  
  cols_to_keep = c("stationid","stratum","region","areaweight","type","count")
  
  if (year == 2018 & type == "Ocean"){
    read.csv("data/ocean_2018.csv") %>% select(cols_to_keep) 
  } else if (year == 2018 & type == "River") {
    read.csv("data/river_2018.csv") %>% select(cols_to_keep)
  } else if (year == 2013 & type == "Ocean") {
    read.csv("data/ocean_2013.csv") %>% select(cols_to_keep) 
  } else if (year == 2013 & type == "River") {
    read.csv("data/river_2013.csv") %>% select(cols_to_keep) 
  } else {
    NULL
  } 
}


tab3_by_year_plotter <- function(year, type, groupBy, plot_tt_cnt){
  if (groupBy == "stratum"){
    tb = tab3_pick_data(year, type) %>% 
      group_by(stratum) %>%
      summarize(
        total_count = sum(count),
        mean = sum(count *area_weight, na.rm =T)/
          sum(area_weight, na.rm = T),
        sd = sqrt(sum(((count - mean)*area_weight)^2, na.rm = T)/
                    (sum(area_weight, na.rm = T))^2),
        CI_95 = 1.96*sd,
        lb = max(0, mean - CI_95),
        ub = mean + CI_95,
        mean_debriscount = mean(count, na.rm = T),
        sd_debriscount = sd(count, na.rm = T),
        CI_95_debriscount = 1.96*sd_debriscount,
        Min = min(na.omit(count)),
        Max = max(na.omit(count)),
        Median = median(na.omit(count)),
        Pcnt_10 = quantile(na.omit(count), .1),
        Pcnt_90 = quantile(na.omit(count), .9)
      )
    if (plot_tt_cnt==T){
      plot = ggplot(tb) +
        geom_col(aes(x = stratum, y = total_count)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = 'none') +
        labs(x = '', y = 'Total count', 
             title = 'Total count of trash per stratum') 
    } else {
      plot = ggplot(tb) +
        geom_col(aes(x = stratum, y = mean, fill = stratum)) +
        geom_errorbar(aes(ymin = lb, 
                          ymax = ub, x = stratum),
                      width = 0.2, alpha = 0.6, size = 1.5) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = 'none') +
        labs(x = '', y = 'Count per sq.km', 
             title = 'Area weighted mean count per stratum')
    }
  }
  if (groupBy == "county"){
    tb = tab3_pick_data(year, type) %>% 
      group_by(region) %>%
      summarize(
        total_count = sum(count),
        mean = sum(count *area_weight, na.rm =T)/
          sum(area_weight, na.rm = T),
        sd = sqrt(sum(((count - mean)*area_weight)^2, na.rm = T)/
                    (sum(area_weight, na.rm = T))^2),
        CI_95 = 1.96*sd,
        lb = max(0, mean - CI_95),
        ub = mean + CI_95,
        mean_debriscount = mean(count, na.rm = T),
        sd_debriscount = sd(count, na.rm = T),
        CI_95_debriscount = 1.96*sd_debriscount,
        Min = min(na.omit(count)),
        Max = max(na.omit(count)),
        Median = median(na.omit(count)),
        Pcnt_10 = quantile(na.omit(count), .1),
        Pcnt_90 = quantile(na.omit(count), .9)
      )
    if (plot_tt_cnt==T){
      plot = ggplot(tb) +
        geom_col(aes(x = region, y = total_count, fill = region)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = 'none') +
        labs(x = '', y = 'Total count', title = 'Total count of trash per county') 
    } else {
      plot = ggplot(tb) +
        geom_col(aes(x = region, y = mean, fill = region)) +
        geom_errorbar(aes(ymin = lb, ymax = ub, x = region),
                      width = 0.2, alpha = 0.6, size = 1.5) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = 'none') +
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

calculate_area_wTrash = function(data){
  percent_trash_area =
    data %>% 
    group_by(stationid, stratum) %>% 
    summarise(
      total_count = sum(count),
      area = sum(area_weight)
    ) %>% 
    mutate(trash_present = ifelse(total_count == 0, 0, 1)) %>% 
    group_by(stratum) %>% 
    summarise(
      total_area = sum(area), 
      area_no_trash = sum(area[trash_present == 0]),
      area_with_trash = sum(area[trash_present == 1])
    ) 
  p_area =
    percent_trash_area %>% 
    add_row(stratum = 'Bight', 
            total_area = sum(percent_trash_area$total_area),
            area_no_trash = sum(percent_trash_area$area_no_trash),
            area_with_trash = sum(percent_trash_area$area_with_trash)
    ) %>% 
    mutate(
      stratum = if_else(stratum == 'Bay', 'Bays', stratum),
      p_area_trash = area_with_trash/total_area,
      p_label = scales::percent(p_area_trash),
      stratum = factor(stratum, levels = c('Bays', 'Inner Shelf', 'Mid Shelf', 
                                           'Outer Shelf', 'Upper Slope', 'MPA', 'Bight')))
  
  return(p_area)
}

tab3_percent_wTrash_plotter <- function(year, type){
  tab3_pick_data(year, type) %>% 
    calculate_area_wTrash() %>% 
    filter(stratum != 'MPA') %>% 
    ggplot(aes(x = stratum, y = p_area_trash, fill = stratum)) +
    geom_col() +
    geom_text(aes(label = p_label), position = position_dodge(0.9), vjust = -1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    ggsci::scale_fill_jco() +
    labs(
      x = 'Stratum',
      y = 'Relative %'
    )    
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
      labs(x = '', y = 'Total count', title = 'Total count of trash per stratum', fill = "Year") 
  }
  if (groupBy == "county"){
    tb_2013 = group_by(data_2013, region) %>% 
      summarise(
        count = sum(count), 
        area_weight = mean(area_weight), 
        average_count = count/area_weight)
    tb_2018 = group_by(data_2018, region) %>% 
      summarise(
        count = sum(count), 
        area_weight = mean(area_weight), 
        average_count = count/area_weight)
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
      labs(x = '', y = 'Total count', title = 'Total count of trash per county', fill = "Year") 
  }
  if (groupBy == "trashType"){
    tb_2013 = group_by(data_2013, type) %>% 
      summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
    tb_2018 = group_by(data_2018, type) %>% 
      summarise(count = sum(count), area_weight = mean(area_weight), average_count = count/area_weight)
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
      labs(x = '', y = 'Total count', title = 'Total count of trash per trash category', fill = "Year") 
  }
  return(plot)
}

tab3_compare_area_wTrash_plotter <- function(type){
  tb_2013 = tab3_pick_data(2013, type) %>% filter(stratum != "MPA") %>% calculate_area_wTrash()
  tb_2018 = tab3_pick_data(2018, type) %>% filter(stratum != "MPA") %>% calculate_area_wTrash()
  plot = 
    bind_rows(
    tb_2013 %>% mutate(year = 2013),
    tb_2018 %>% mutate(year = 2018)
  ) %>% 
    mutate(year = as.factor(year)) %>% 
    ggplot(aes(x = stratum, y = p_area_trash, fill = year, group = year)) +
    geom_col(position = 'dodge') +
    geom_text(aes(label = p_label), position = position_dodge(0.9), vjust = -1, size = 3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    ggsci::scale_fill_jco() +
    labs(
      x = 'Stratum',
      y = 'Relative Percent Trash Area'
    )
  return(plot)
}




#### functions for tab 4: distance to nearest road -------- ####



#### functions for tab 5: data ------------------------ ####



#### functions for tab 6: summary -------------------- ####







