library(readxl)
library(tidyverse)
library(ggplot2)

ocean_debris <- readxl::read_excel('data/2018/Ocean/FISH-INVERT-DEBRIS-BIGHT18-CONNECTOR.xlsx', sheet = 5)
View(ocean_debris)

tot_count <- ocean_debris %>% 
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>% 
  group_by(debristype) %>% 
  summarise(
    n_total = sum(debriscount)
  ) %>%
  arrange(desc(n_total))
tot_count

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

tot_count %>%
  ggplot()+
  geom_col(aes(x=debristype,y=n_total, fill = debristype))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_colour_manual(values=cbbPalette)+
  labs(y = "Count", title = "Total Debris Count")


Plastic <- 
  ocean_debris %>%
  group_by(debristype) %>%
  filter(debristype %in% c("Bag","Bottle","Cap/Lid","Cup", "Fishing Line/Net",
                           "Plastic Piece (unid.)","Tire", "Polypropylene Rope",
                           "Other Plastic (comment req.)"))
total_plastic <- sum(Plastic$debriscount)

Recyclable <- 
  ocean_debris %>%
  group_by(debristype) %>%
  filter(debristype %in% c("Beer Bottle","Glass Bottle/Jar -other",
                           "Can - other", "Drink Can"))
total_recyclable <- sum(Recyclable$debriscount)

Trash <- 
  ocean_debris %>%
  group_by(debristype) %>%
  filter(debristype %in% c("Fishing Gear", "Food Bag/Wrapper",
                           "Other Misc. (comment req.)","Clothing",
                           "Rag/Cloth", "Other Metal (comment req.)"))
total_trash <- sum(Trash$debriscount)

Degradable <- 
  ocean_debris %>%
  group_by(debristype) %>%
  filter(debristype %in% c("Lumber","Paper","Stick/Branch/Driftwood",
                           "Leaves/Seed Pod"))
total_degradable <- sum(Degradable$debriscount)

Rock <- 
  ocean_debris %>%
  group_by(debristype) %>%
  filter(debristype %in% c("Other Terrestrial (comment req.)","Rock"))
total_rock <- sum(Rock$debriscount)

Marine_Misc <- 
  ocean_debris %>%
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>%
  group_by(debristype) %>%
  filter(debristype %in% c("Foliose Algae - not kelp","Gorgonian Sea Fan (dead)",
                           "Kelp Holdfast", "Kelp Stipe/Blade","Seagrass",
                           "Other Foliose Algae", "Other Marine (comment req.)"))
total_marine_misc <- sum(Marine_Misc$debriscount)

tab <- data.frame(cbind(total_plastic,total_degradable,total_recyclable,
                        total_trash,total_rock, total_marine_misc))


clean_sample <- length(which(ocean_debris$debristype == "None"))

length(unique(ocean_debris$stationid))

station <- readxl::read_excel('data/2018/Ocean/StationCompletionV8.xlsx')

completed_dat <- left_join(ocean_debris, station, "stationid")

stratum_trash <-
  completed_dat %>%
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>%
  group_by(stratum)%>%
  summarise(
    total_debris = sum(debriscount)
  ) %>%
  arrange(desc(total_debris))


stratum_trash %>%
  drop_na() %>% 
  mutate(
    stratum = factor(stratum, levels = c('Bays', 'Ports', 'Marinas',
                                         'Estuaries', "Brackish Estuaries",
                                         "Inner Shelf", "Mid Shelf", "Outer Shelf",
                                         "Upper Slope", "Lower Slope", "Channel Islands",
                                         'Open', 'Urban', 'Agriculture'))
  ) %>% 
  ggplot()+
  geom_col(aes(x=stratum, y = total_debris, fill = stratum))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Total Debris Count by Stratum", x = 'Stratum', y = "Total Count")


# try this table ----------------------------------------------------------


big_type <- tot_count %>% 
  mutate(
    type = case_when(
      debristype %in% c("Bag","Bottle","Cap/Lid","Cup", "Fishing Line/Net",
                        "Plastic Piece (unid.)","Tire", "Polypropylene Rope",
                        "Other Plastic (comment req.)") ~ 'Plastic',
      debristype %in% c("Beer Bottle","Glass Bottle/Jar -other",
                        "Can - other", "Drink Can") ~ 'Recyclable',
      debristype %in% c("Fishing Gear", "Food Bag/Wrapper",
                        "Other Misc. (comment req.)","Clothing",
                        "Rag/Cloth", "Other Metal (comment req.)") ~ 'Trash',
      debristype %in% c("Lumber","Paper","Stick/Branch/Driftwood",
                        "Leaves/Seed Pod") ~ 'Degradable',
      debristype %in% c("Other Terrestrial (comment req.)","Rock") ~ 'Rock',
      debristype %in% c("Foliose Algae - not kelp","Gorgonian Sea Fan (dead)",
                        "Kelp Holdfast", "Kelp Stipe/Blade","Seagrass",
                        "Other Foliose Algae", "Other Marine (comment req.)") ~ 'Marine Misc',
      TRUE ~ NA_character_
    )
  )

big_type %>% 
  group_by(type) %>% 
  summarise(
    n = sum(n_total)
  ) %>% 
  drop_na() %>% 
  mutate(
    p = scales::percent(n/sum(n)),
    type = factor(type, levels = c('Recyclable', 'Rock', 'Plastic', 'Degradable', 'Trash', 'Marine Misc'))
  ) %>% 
  ggplot(aes(x = type, y = n, fill = type)) +
  geom_col() +
  geom_text(aes(label = p, y = n+10)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = 'Total Count')



