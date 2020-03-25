library(tidyverse)
library(shiny)
library(shinydashboard)

library(leaflet)
library(stringr)


source_folder <- function(dir, the_env) { # environment needed to source multiple files locally
  scripts <- list.files(dir, full.names = TRUE)
  for (script in scripts) {
    source(script, local = the_env)
  }
}



today <- Sys.Date()

# ui parts

topbox_height <- 330
left_col_width <- 6
right_col_width <- 6


ui_env <- environment()
helpers <- environment()

