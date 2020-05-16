
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(gridExtra)
library(ggsci)
library(DT)




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
    


    
    # #### tab 3.5: magnitude and Trend ####
    tabPanel("Magnitude and Trend",
             sidebarLayout(
               position = "left",
               #### sideBar panel ####
               sidebarPanel(
                 width = 2,
                   selectizeInput(
                     inputId = "tab3_Type",
                     label = "River or Ocean",
                     choices = c("River","Ocean"),
                     options = list(
                       placeholder = 'Choose one',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                    ),
                 conditionalPanel(
                   condition = 'input.tab3_Type!=""',
                   div(
                     checkboxInput("tab3_total", "Total count", value = T),
                     checkboxInput("tab3_by_stratum", "Count by stratum", value = T),
                     checkboxInput("tab3_by_region", "Count by region", value = T),
                     checkboxInput("tab3_relative_abundance", "Relative trash type abundance", value = T),
                     style = "font-size: 12px !important; text-align:left;"
                   )
                 )
               ),
              
               mainPanel( 
                 width = 10,
                 conditionalPanel(
                   condition = 'input.tab3_Type!=""',
                   width = 12,
                   h2(textOutput("tab3_title"), align = "center"),
                   fluidRow(
                     width = 12,
                     conditionalPanel(
                       'input.tab3_total',
                       h3("Total Count", align = "center"),
                       br(),
                       fluidRow(
                         width = 12,
                         div(
                           class = "container-fluid",
                           column(1),
                           column(10, align="center", plotOutput("tab3_test_plot0", height = 300)),
                           column(1))
                       ),
                       br()
                     ),
                     conditionalPanel(
                       'input.tab3_by_stratum',
                       h3("Count by Stratum", align = "center"),
                       br(),
                       fluidRow(
                         width = 12,
                         div(
                           class = "container-fluid",
                           column(1),
                           column(5, align="center", plotOutput("tab3_test_plot1", height = 300)),
                           column(5, align="center", plotOutput("tab3_test_plot2", height = 300)),
                           column(1))
                       ),
                       br()
                     ),
                     conditionalPanel(
                       'input.tab3_by_region',
                       h3("Count by County", align = "center"),
                       fluidRow(
                         width = 12,
                         div(
                           class = "container-fluid",
                           column(1),
                           column(5, align="center", plotOutput("tab3_test_plot3", height = 300)),
                           column(5, align="center", plotOutput("tab3_test_plot4", height = 300)),
                           column(1))
                       )
                     ),
                     conditionalPanel(
                       'input.tab3_relative_abundance',
                       h3("Relative Abundance", align = "center"),
                       fluidRow(
                         width = 12,
                         div(
                           class = "container-fluid",
                           column(6, align="center", plotOutput("tab3_test_plot5", height = 700)),
                           column(6, align="center", plotOutput("tab3_test_plot6", height = 700)))
                       )
                     )
                   )
                 )
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


