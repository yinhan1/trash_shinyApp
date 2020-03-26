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
library(shinycssloaders)




# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # #### logo & header ------------- ####
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
    collapsible = TRUE,
    "Trash and Marine Debris",
    id = "nav",
    
    #### tab 1: background ####
    tabPanel("Background",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
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
             div(class="outer",
                 tags$head(includeCSS("www/style_map.css")),
                 leafletOutput("tab2", width="100%", height="100%"),
                 
                 absolutePanel(id = "controls", 
                               class = "panel panel-default",
                               top = 80, left = 20, width = 250, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               span(h4(textOutput("tab2_ocean_site_count"), align = "right"), style = "color:#cc4c02"),
                               span(h4(textOutput("tab2_river_site_count"), align = "right"), style = "color:#662506"),
                               h5(textOutput("tab2_site_count_year"), align = "right"),
                               plotOutput("tab2_mean_count_bar", height="250px", width="100%"),
                               
                               selectInput(
                                 inputId = "tab2_Year",
                                 label = "Year",
                                 choices = c(2018,2013,2008,2003,1998))
                 )
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
                 width = 2,
                 radioButtons(
                   inputId = "tab5_Type",
                   label = "Type",
                   choices = c("Ocean", "River")
                 ),
                 selectInput(
                   inputId = "tab5_Year",
                   label = "Year",
                   choices = c(2013, 2018)
                 )
               ),
               mainPanel(dataTableOutput("tab5"))
             )),
    
    
    #### tab 6: summary ####
    tabPanel("Summary")
    
    
  )
))
