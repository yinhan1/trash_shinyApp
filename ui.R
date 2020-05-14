
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
             sidebarLayout(position = "left",
                           
                           #### sideBar panel ####
                           sidebarPanel(
                             width = 2,
                               selectizeInput(
                                 inputId = "tab3_Type",
                                 label = "River or Ocean",
                                 choices = c("River","Ocean"),
                                 options = list(
                                   placeholder = 'Choose river or ocean',
                                   onInitialize = I('function() { this.setValue(""); }')
                                 )
                             ),
                             selectizeInput(
                               inputId = "tab3_Year",
                               label = "Year",
                               choices = c(2018,2013,"Compare"),
                               options = list(
                                 placeholder = 'Choose a year',
                                 onInitialize = I('function() { this.setValue(""); }')
                               )
                             ),
                             conditionalPanel(
                               condition = 'input.tab3_Year!="" & input.tab3_Type!=""',
                               div(
                                 checkboxInput("tab3_count_by_stratum", "Count by Stratum", value = T),
                                 checkboxInput("tab3_count_by_region", "Count by Region", value = T),
                                 checkboxInput("tab3_count_by_trashType", "Count by Trash Type", value = T),
                                 checkboxInput("tab3_relative", "Relative by County", value = T),
                                 checkboxInput("tab3_percent_with_trash", "Percent of Area with Trash", value = T),
                                 style = "font-size: 12px !important; text-align:left;"
                               )
                             )
                           ),
                          
                           mainPanel( 
                             width = 10,
                             box(
                               width = 12,
                                 
                               #### single year ####
                               conditionalPanel(
                                 width = 12,
                                 'input.tab3_Year!="" & input.tab3_Year!="Compare"',
                                 column(
                                   width = 12,
                                   h2(textOutput("tab3_title1"), align = "center"),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       'input.tab3_count_by_stratum',
                                       box(
                                         width = 12,
                                         title = "Count by Stratum",
                                         column(6, plotOutput("tab3_ttl_count_by_stratum_plot", height = 300)),
                                         column(6, plotOutput("tab3_area_count_by_stratum_plot", height = 300))
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       'input.tab3_count_by_region',
                                       box(
                                         width = 12,
                                         title = "Count by County",
                                         column(6, plotOutput("tab3_ttl_count_by_county_plot", height = 300)),
                                         column(6, plotOutput("tab3_area_count_by_county_plot", height = 300))
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       'input.tab3_count_by_trashType',
                                       box(
                                         width = 12,
                                         title = "Count by Trash Type",
                                         column(6,plotOutput("tab3_ttl_count_by_trashType_plot", height = 300)),
                                         column(6,plotOutput("tab3_area_count_by_trashType_plot", height = 300))
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       'input.tab3_relative',
                                       box(
                                         width = 12, 
                                         title = "Relative by County",
                                         plotOutput("tab3_relative_by_stratum_plot", height = 400)
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       'input.tab3_percent_with_trash',
                                       box(
                                         width = 12, 
                                         title = "Percent of Area with Trash",
                                         plotOutput("tab3_percent_with_trash_plot", height = 400)
                                       )
                                     )
                                   )
                                 )
                               ),
                               
                               #### compare years ####
                               
                               conditionalPanel(
                                 width = 12,
                                 'input.tab3_Year == "Compare"',
                                 column(
                                   width = 12,
                                   h2(textOutput("tab3_title2"), align = "center"),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       width = 12,
                                       'input.tab3_count_by_stratum',
                                       box(
                                         width = 12,
                                         title = "Count by Stratum",
                                         plotOutput("tab3_compare_stratum_plot", height = 400)
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       width = 12,
                                       'input.tab3_count_by_trashType',
                                       box(
                                         width = 12,
                                         title = "Count by Trash Type",
                                         plotOutput("tab3_compare_trashType_plot", height = 400)
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     width = 12,
                                     conditionalPanel(
                                       width = 12,
                                       'input.tab3_percent_with_trash',
                                       box(
                                         width = 12,
                                         title = "Percent of Area with Trash",
                                         plotOutput("tab3_percent_with_trash", height = 400)
                                       )
                                     )
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


