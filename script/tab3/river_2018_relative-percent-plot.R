library(tidyverse)

load('data/2018/River/riverine.RData')

river_2018 <- tally_df %>% 
  mutate(resulttotal = if_else(resulttotal < 0, 0, resulttotal)) %>% 
  inner_join(
    siteinfo %>% 
      select(stationid, watershed),
    by = 'stationid'
  )

count_by_watershed_cat <- river_2018 %>% 
  group_by(watershed, debriscategory) %>% 
  summarise(
    total_count = sum(resulttotal)
  ) %>% 
  ungroup()


relative_count <- count_by_watershed_cat %>% 
  # filter(debriscategory != 'None') %>% 
  group_by(watershed) %>% 
  mutate(
    relative_percent = total_count/sum(total_count)
  ) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x = watershed, y = relative_percent, 
               group = watershed, fill = debriscategory), 
           position = 'stack') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_igv() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom') +
  labs(
    x = 'Watershed',
    y = 'Relative Pecentage',
    fill = 'Debris Category'
  )
