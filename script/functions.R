

#### functions for tab 1: background ------------------ ####




#### functions for tab 2: station mapper --------------- ####




#### functions for tab 3 ---------------------------- ####

call_river_2013 <-
  function() {
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


#### functions for tab 4: distance to nearest road -------- ####



#### functions for tab 5: data ------------------------ ####




#### functions for tab 6: summary -------------------- ####







