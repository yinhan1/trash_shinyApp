library(tidyverse)

stations <- readxl::read_excel('data/2018/Ocean/StationCompletionV8.xlsx')

ocean_debris <- readxl::read_excel('data/2018/Ocean/FISH-INVERT-DEBRIS-BIGHT18-CONNECTOR.xlsx', 
                                   sheet = 5)

debris_count <- ocean_debris %>% 
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>% 
  group_by(stationid) %>% 
  summarise(
    debriscount = sum(debriscount)
  ) %>%
  arrange(desc(debriscount))

Debris <- debris_count %>% 
  inner_join(
    stations %>% 
      select(stationid, lat, lon, stratum, FinalStratum, 
             areaweight = `Area Weight All Sites`, Region),
    by = 'stationid'
  )



test <- Debris %>% 
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>% 
  group_by(stationid) %>% 
  summarise(
    sum_debriscount = sum(debriscount, na.rm =T),
    denominator = max(areaweight),
    numerator = sum_debriscount * denominator,
  # ) %>% 
  # group_by(stationid) %>% 
  # summarise(
    mean = sum(numerator, na.rm = T)/sum(denominator, na.rm = T), 
    sd = sqrt(sum(((sum_debriscount - mean)*denominator)^2, na.rm = T)/
                (sum(denominator, na.rm = T))^2),
    CI_95 = 1.96*sd, 
    mean_debriscount = mean(sum_debriscount, na.rm = T),
    sd_debriscount = sd(sum_debriscount, na.rm = T),
    CI_95_debriscount = 1.96*sd_debriscount,
    Min = min(na.omit(sum_debriscount)),
    Max = max(na.omit(sum_debriscount)),
    Median = median(na.omit(sum_debriscount)),
    Pcnt_10 = quantile(na.omit(sum_debriscount), .1),
    Pcnt_90 = quantile(na.omit(sum_debriscount), .9),
    total = sum(sum_debriscount)
  )



area_weighted_count_stratum <- Debris %>% 
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>% 
  group_by(FinalStratum) %>% 
  summarize(
    mean = sum(debriscount *areaweight, na.rm =T)/
      sum(areaweight, na.rm = T),
    sd = sqrt(sum(((debriscount - mean)*areaweight)^2, na.rm = T)/
                (sum(areaweight, na.rm = T))^2),
    CI_95 = 1.96*sd,
    mean_debriscount = mean(debriscount, na.rm = T),
    sd_debriscount = sd(debriscount, na.rm = T),
    CI_95_debriscount = 1.96*sd_debriscount,
    Min = min(na.omit(debriscount)),
    Max = max(na.omit(debriscount)),
    Median = median(na.omit(debriscount)),
    Pcnt_10 = quantile(na.omit(debriscount), .1),
    Pcnt_90 = quantile(na.omit(debriscount), .9)
  )


count_by_stratum <- area_weighted_count_stratum %>% 
  mutate(
    FinalStratum = factor(FinalStratum, levels = c('Bays', 'Ports', 'Marinas',
                                         'Estuaries', "Brackish Estuaries",
                                         "Inner Shelf", "Mid Shelf", "Outer Shelf",
                                         "Upper Slope", "Lower Slope", "Channel Islands",
                                         'Open', 'Urban', 'Agriculture')),
    error_min = mean - CI_95, 
    error_max = mean + CI_95,
    error_min = if_else(error_min < 0, 0, error_min)
  ) %>% 
  ggplot(aes(x = FinalStratum, y = mean, fill = FinalStratum)) +
  geom_col() +
  geom_errorbar(aes(ymin = error_min, ymax = error_max),
                width = 0.2, alpha = 0.6, size = 1.5) +
  
  theme_classic() +
  labs(
    y = 'Area weighted average count (95% CI)',
    x = ''
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')


# group by Region ---------------------------------------------------------

area_weighted_count_region <- Debris %>% 
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>% 
  group_by(Region) %>% 
  summarize(
    mean = sum(debriscount *areaweight, na.rm =T)/
      sum(areaweight, na.rm = T),
    sd = sqrt(sum(((debriscount - mean)*areaweight)^2, na.rm = T)/
                (sum(areaweight, na.rm = T))^2),
    CI_95 = 1.96*sd,
    mean_debriscount = mean(debriscount, na.rm = T),
    sd_debriscount = sd(debriscount, na.rm = T),
    CI_95_debriscount = 1.96*sd_debriscount,
    Min = min(na.omit(debriscount)),
    Max = max(na.omit(debriscount)),
    Median = median(na.omit(debriscount)),
    Pcnt_10 = quantile(na.omit(debriscount), .1),
    Pcnt_90 = quantile(na.omit(debriscount), .9)
  )


count_by_region <- area_weighted_count_region %>% 
  mutate(
    error_min = mean - CI_95, 
    error_max = mean + CI_95,
    error_min = if_else(error_min < 0, 0, error_min)
  ) %>% 
  ggplot(aes(x = Region, y = mean, fill = Region)) +
  geom_col() +
  geom_errorbar(aes(ymin = error_min, ymax = error_max),
                width = 0.2, alpha = 0.6, size = 1.5) +
  
  theme_classic() +
  labs(
    y = 'Area weighted average count (95% CI)',
    x = ''
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none') +
  ggsci::scale_fill_simpsons()

