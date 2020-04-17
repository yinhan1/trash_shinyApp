
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(gridExtra)
library(ggsci)


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
    tabPanel(
      "Background",
      column(1, br()),
      column(10, includeMarkdown('./tab_1_background/background.rmd')),
      column(1, br())
    ),

    #### tab 2: station mapper ####
    tabPanel("Station mapper",
             div(class="outer",
                 tags$head(includeCSS("www/style_map.css")),
                 leafletOutput("tab2_map", width="100%", height="100%"),
                 
                 absolutePanel(id = "controls", 
                               class = "panel panel-default",
                               top = 100, left = 20, width = 250, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               span(h3(textOutput("tab2_ocean_site_count"), align = "right"), style = "color:#0000FF"),
                               span(h3(textOutput("tab2_river_site_count"), align = "right"), style = "color:#EE2C2C"),
                               h4(textOutput("tab2_site_count_year"), align = "right", style = "color:#1A1102"),
                               plotOutput("tab2_site_count_bar", height="250px", width="100%"),
                               
                               selectInput(
                                 inputId = "tab2_Year",
                                 label = "Year",
                                 choices = c(2018,2013,2008,1998,1994))
                 )
             )),
    

    #### tab 3: area weight mean count ####
    tabPanel("Area Weighted Mean Count",
             sidebarLayout(position = "left",
                           sidebarPanel("sidebar panel",
                                        checkboxInput("donum1", "Count by Stratum", value = T),
                                        checkboxInput("donum2", "Count by Region", value = F),
                                        checkboxInput("donum3", "Count by Land Use", value = F),
                                        checkboxInput("donum4", "Relative Count by Watershed", value = F)
                                        # sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                                        # sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                                        # sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                           ),
             mainPanel("main panel",
               column(1, plotOutput(outputId="plotgraph", width="500px",height="400px"))
             )
          )
    ),
    
    #### tab 4: distance to nearest road ####
    # tabPanel("Distance to nearest road"),
    
    #### tab 5: data ####
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 radioButtons(
                   inputId = "tab5_Type",
                   label = "Type",
                   choices = c("Ocean","River")
                 ),
                 selectInput(
                   inputId = "tab5_Year",
                   label = "Year",
                   choices = c(2018, 2013)
                 )
               ),
               mainPanel(dataTableOutput("tab5"))
             )),
    
    
    #### tab 6: summary ####
    tabPanel(
      "Summary",
      column(1, br()),
      column(10, includeMarkdown('./tab_6_summary/summary.rmd')),
      column(1, br())
    )
    
    
  )
))


