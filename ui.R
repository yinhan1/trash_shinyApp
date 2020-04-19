
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
                                        checkboxInput("donum1", "Count by Stratum", value = F),
                                        checkboxInput("donum2", "Count by Region", value = F),
                                        checkboxInput("donum3", "Count by Land Use", value = F),
                                        checkboxInput("donum4", "Relative Count by Watershed", value = T)
                                        # sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                                        # sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                                        # sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                           ),
             mainPanel("main panel",
               column(1, plotOutput(outputId="plotgraph", width="500px",height="400px"))
             )
          )
    ),
    
    # #### tab 3.5: magnitude and Trend ####
    tabPanel("Magnitude and Trend",
             sidebarLayout(position = "left",
                           
                           ### sideBar panel
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
                                 checkboxInput("tab3_count_by_stratum", "Count by Stratum", value = F),
                                 checkboxInput("tab3_count_by_county", "Count by County", value = F),
                                 checkboxInput("tab3_relative_by_stratum", "Relative by County", value = F),
                                 checkboxInput("tab3_relative_by_county", "Count by County", value = F),
                                 style = "font-size: 12px !important; text-align:left;"
                               )
                             )
                           ),
                          
                           mainPanel(
                             box(
                               width = 12,
                               
                               #### single year ####
                               
                               conditionalPanel(
                                 width = 12,
                                 'input.tab3_Year!="Compare"',
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_count_by_stratum',
                                     box(
                                       width = 12, title = "Count by Stratum",
                                       plotOutput("tab3_example1", height = 500)
                                       )
                                   )
                                 ),
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_count_by_county',
                                     box(
                                       width = 12, title = "Count by County",
                                       plotOutput("tab3_example2", height = 500)
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_relative_by_stratum',
                                     box(
                                       width = 12, title = "Relative by Stratum",
                                       plotOutput("tab3_example3", height = 500)
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_relative_by_county',
                                     box(
                                       width = 12, title = "Relative by County",
                                       plotOutput("tab3_example4", height = 500)
                                     )
                                   )
                                 )
                               ),
                               
                               #### compare years ####
                               
                               conditionalPanel(
                                 width = 12,
                                 'input.tab3_Year == "Compare"',
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_count_by_stratum',
                                     box(
                                       width = 12,
                                       title = "County by Stratum",
                                       column(6,
                                         plotOutput("tab3_compare_11", height = 500)
                                       ),
                                       column(6,
                                         plotOutput("tab3_compare_12", height = 500)
                                       )
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_count_by_county',
                                     box(
                                       width = 12,
                                       title = "County by County",
                                       column(6,
                                              plotOutput("tab3_compare_21", height = 500)
                                       ),
                                       column(6,
                                              plotOutput("tab3_compare_22", height = 500)
                                       )
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_relative_by_stratum',
                                     box(
                                       width = 12,
                                       title = "Relative by Stratum",
                                       column(6,
                                              plotOutput("tab3_compare_31", height = 500)
                                       ),
                                       column(6,
                                              plotOutput("tab3_compare_32", height = 500)
                                       )
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   width = 12,
                                   conditionalPanel(
                                     'input.tab3_relative_by_county',
                                     box(
                                       width = 12,
                                       title = "Relative by County",
                                       column(6,
                                              plotOutput("tab3_compare_41", height = 500)
                                       ),
                                       column(6,
                                              plotOutput("tab3_compare_42", height = 500)
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


