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


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # set up logo
    tags$a(href = "https://www.sccwrp.org", tags$img(src = "sccwrp_logo.jpg", title = "SCCWRP", width = "300", height = "80")),
    
    navbarPage(
      theme = shinytheme("cosmo"), 
      #cerulean, cosmo**, flatly, journal, lumen**, paper, readable, sandstone, simplex, spacelab*,yeti**
               collapsible = TRUE,
               "Trash and Marine Debris", 
               id="nav",
      
      # tab panel for plot
      tabPanel(
        "Background",
        sidebarLayout(
          sidebarPanel(sliderInput("bins", "Number of observations:", min = 1, max = 50, value = 5)),
          mainPanel(plotOutput("distPlot")))),
      
      # tab panel for maps
      tabPanel(
        "Station mapper"
      ),
      
      # tab panel for trend bar over years
      tabPanel(
        "Trend over years"
      ),
      
      # tab panel for model on distance vs trash abundance
      tabPanel(
        "Distance to nearest road"
      ),
      
      # tab panel for data table
      tabPanel(
        "Data",
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "Year", label = "Year", choices = c(2013, 2018)),
            radioButtons(inputId = "Type", label = "Type", choices = c("Ocean","River"))),
          mainPanel(dataTableOutput("tb_displayed")))),
      
      
      # tabl panel for all write ups
      tabPanel(
        "Summary"
      )
      

    ) # end of navbarPanel
  ) # end of fluidPage
) # end of ui
 







