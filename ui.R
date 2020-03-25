#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)




# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  #### logo & header ------------- ####
  tags$a(
    href = "https://www.sccwrp.org",
    tags$img(
      src = "sccwrp_logo.jpg",
      title = "SCCWRP",
      width = "300",
      height = "80"
    )
  ),
  
  #### navigator ----------------- ####
  navbarPage(
    theme = shinytheme("cosmo"),
    #cerulean, cosmo**, flatly, journal, lumen**, paper, readable, sandstone, simplex, spacelab*,yeti**
    collapsible = TRUE,
    "Trash and Marine Debris",
    id = "nav",
    
    
    
    #### tab 1: background ####
    tabPanel("Background",
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "bins",
                   "Number of observations:",
                   min = 1,
                   max = 50,
                   value = 5
                 )
               ),
               mainPanel(plotOutput("distPlot"))
             )),
    
    
    #### tab 2: station mapper ####
    tabPanel("Station mapper",
             sidebarLayout(
               sidebarPanel(
                 pickerInput(
                   inputId = "Type",
                   label = "Type",
                   choices = c("Ocean", "River"),
                   options = list(
                     `actions-box` = TRUE,
                     size = 10,
                     `selected-text-format` = "count > 3"
                   ),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "Year",
                   label = "Year",
                   choices = c(2018, 2013, 2008, 2003, 1998),
                   options = list(
                     `actions-box` = TRUE,
                     size = 10,
                     `selected-text-format` = "count > 4"
                   ),
                   multiple = TRUE
                 )
               ),
               mainPanel(plotOutput("StationMapper_site"))
             )),
    
    
    
    #### tab 3: area weight mean count ####
    tabPanel("Area Weighted Mean Count",
             mainPanel(plotOutput('trash_count'))),
    
    #### tab 4: distance to nearest road ####
    tabPanel("Distance to nearest road"),
    
    #### tab 5: data ####
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "Type",
                   label = "Type",
                   choices = c("Ocean", "River")
                 ),
                 selectInput(
                   inputId = "Year",
                   label = "Year",
                   choices = c(2013, 2018)
                 )
               ),
               mainPanel(dataTableOutput("tb_displayed"))
             )),
    
    
    #### tab 6: summary ####
    tabPanel("Summary")
    
    
  )
))
