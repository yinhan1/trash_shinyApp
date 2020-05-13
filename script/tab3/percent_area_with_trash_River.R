library(tidyverse)
library(ggplot2)

## River 2013 ##

river_2013 <- read.csv('data/River/2013_river_debris_count.csv',h=T)

trash_area_percentage_river_13 <- river_2013 %>%
  group_by(stationid,stratum) %>%
  summarise(
    total_count = sum(totalcount),
    area = sum(area_weight)
  ) %>%
  mutate(trash_present = if_else(total_count == 0, 0, 1)) %>% 
  group_by(stratum) %>% 
  summarise(
    total_area = sum(area), 
    area_no_trash = sum(area[trash_present == 0]),
    area_with_trash = sum(area[trash_present == 1])
  ) 

percentage_area_2013 <- trash_area_percentage_river_13 %>% 
  add_row(stratum = 'Total Land Use', 
          total_area = sum(trash_area_percentage_river_13$total_area),
          area_no_trash = sum(trash_area_percentage_river_13$area_no_trash),
          area_with_trash = sum(trash_area_percentage_river_13$area_with_trash)
  ) %>% 
  mutate(
    p_area_trash = area_with_trash/total_area,
    p_label = scales::percent(p_area_trash),
    stratum = factor(stratum, levels = c('Agriculture', 'Urban', 'Open',
                                         'Total Land Use'))
  ) 

percentage_area_2013 %>% 
  ggplot(aes(x = stratum, y = p_area_trash, fill = stratum)) +
  geom_col() +
  geom_text(aes(label = p_label), position = position_dodge(0.9), vjust = -1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none') +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ggsci::scale_fill_jco() +
  labs(
    x = 'Stratum',
    y = 'Relative %',
    title = 'Percentage of Area with Trash 2013',
    caption = 'There are significant amount of trash in the Urban areas'
  )

##--------------------------------------------------------------------------------##

## River 2018 ##

river_2018 <- read.csv('data/River/2018_river_debris_count.csv',h=T)

trash_area_percentage_river_18 <- river_2018 %>%
  group_by(stationid,stratum) %>%
  summarise(
    total_count = sum(total_count),
    area = sum(area_weight)
  ) %>%
  mutate(trash_present = if_else(total_count == 0, 0, 1)) %>% 
  group_by(stratum) %>% 
  summarise(
    total_area = sum(area), 
    area_no_trash = sum(area[trash_present == 0]),
    area_with_trash = sum(area[trash_present == 1])
  ) 

percentage_area_2018 <- trash_area_percentage_river_18 %>% 
  add_row(stratum = 'Total Land Use', 
          total_area = sum(trash_area_percentage_river_18$total_area),
          area_no_trash = sum(trash_area_percentage_river_18$area_no_trash),
          area_with_trash = sum(trash_area_percentage_river_18$area_with_trash)
  ) %>% 
  mutate(
    p_area_trash = area_with_trash/total_area,
    p_label = scales::percent(p_area_trash),
    stratum = factor(stratum, levels = c('Agriculture', 'Urban', 'Open',
                                         'Total Land Use'))
  )

percentage_area_2018 %>% 
  ggplot(aes(x = stratum, y = p_area_trash, fill = stratum)) +
  geom_col() +
  geom_text(aes(label = p_label), position = position_dodge(0.9), vjust = -1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none') +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ggsci::scale_fill_jco() +
  labs(
    x = 'Stratum',
    y = 'Relative %',
    title = 'Percentage of Area with Trash 2018',
    caption = 'There are significant increase in abundance of trash in the all areas'
  )

##--------------------------------------------------------------------------------##

## Comibine the years ##

percentage_area_2013 %>% 
  mutate(
    year = 2013,
    stratum = as.character(stratum)
  ) %>% 
  bind_rows(
    percentage_area_2018 %>% 
      mutate(
        year = 2018,
        stratum = as.character(stratum)
      )
  ) %>% 
  mutate(
    year = factor(year, levels = c('2013', '2018')),
    stratum = factor(stratum, levels = c('Agriculture', 'Urban', 'Open', 
                                         'Total Land Use'))
  ) %>% 
  ggplot(aes(x = stratum, y = p_area_trash, fill = year, group = year)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = p_label), position = position_dodge(0.9), vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ggsci::scale_fill_jco() +
  labs(
    x = 'Stratum',
    y = 'Relative Percent Trash Area',
    title = 'Percentage of Area with Trash',
    caption = 'The percentages of area that contain trash in 2018, in general,
    are much higher compared to 2013 especially in the Agriculure area'
  )