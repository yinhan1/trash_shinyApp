library(tidyverse)

tblgis <- readxl::read_excel('data/tblgismetrics_031920.xlsx')

tblgis_f <- tblgis %>% 
  select(
    stationcode, 
    new_lat,
    new_long, 
    area_sqkm,
    Ag_percent = ag_2011_ws,
    Urban_percent = urban_2011_ws
  ) %>% 
  mutate(
    Open_percent = 100 - (Ag_percent + Urban_percent),
    Ag_area = Ag_percent/100 * area_sqkm,
    Urban_area = Urban_percent/100 * area_sqkm,
    Open_area = Open_percent/100 * area_sqkm
  ) 


river_trashtally <- read_csv('data/2018/River/tbl_trashtally.csv') %>%
  mutate(
    sampledate = as.Date(sampledate),
    resulttotal = if_else(resulttotal < 0 , 0, resulttotal)
  ) %>% 
  select(
    objectid, stationid, sampledate, debriscategory, resulttotal
  ) 


debris_count_by_cat <- river_trashtally %>% 
  group_by(stationid, debriscategory) %>% 
  summarise(
    total_count = sum(resulttotal)
  )

debris_count_by_station <- river_trashtally %>% 
  group_by(stationid) %>% 
  summarise(
    total_count = sum(resulttotal)
  ) %>% 
  left_join(
    tblgis_f,
    by = c('stationid' = 'stationcode')
  )


debris_with_area <- debris_count_by_station %>% 
  drop_na() %>% 
  mutate(
    land_use = if_else(Urban_area > Ag_area, 'Urban', 'Agriculture'),
    weight = (if_else(Urban_area > Ag_area, Urban_percent, Ag_percent))/100
  ) %>%
  group_by(land_use) %>% 
  mutate(
    areaweight = sum(weight*area_sqkm)/n()
  ) %>% 
  ungroup()

# inner_join(
#   read_csv('data/2018/River/rivertrash_2018.csv'),
#   debris_with_area %>% select(stationid,areaweight),
#   by ="stationid"
# ) %>% 
#   write.csv(file = 'data/2018/River/river_2018_map.csv')


# debris_with_area %>% 
#   select(
#     stationid,total_count,new_lat,new_long,land_use,areaweight
#   ) %>% 
#   write.csv(file="data/2018/River/river_2018.csv")

area_weighted_count_land <- debris_with_area %>% 
  mutate(total_count = if_else(total_count < 0, 0, total_count)) %>% 
  group_by(land_use) %>% 
  summarize(
    mean = sum(total_count *areaweight, na.rm =T)/
      sum(areaweight, na.rm = T),
    sd = sqrt(sum(((total_count - mean)*areaweight)^2, na.rm = T)/
                (sum(areaweight, na.rm = T))^2),
    CI_95 = 1.96*sd,
    mean_total_count = mean(total_count, na.rm = T),
    sd_total_count = sd(total_count, na.rm = T),
    CI_95_total_count = 1.96*sd_total_count,
    Min = min(na.omit(total_count)),
    Max = max(na.omit(total_count)),
    Median = median(na.omit(total_count)),
    Pcnt_10 = quantile(na.omit(total_count), .1),
    Pcnt_90 = quantile(na.omit(total_count), .9)
  )


count_by_land <- area_weighted_count_land %>% 
  ggplot(aes(x = land_use, y = mean, fill = land_use)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean - CI_95, ymax = mean + CI_95),
                width = 0.2, alpha = 0.6, size = 1.5) +
  
  theme_classic() +
  labs(
    y = 'Area weighted average count (95% CI)',
    x = ''
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')
