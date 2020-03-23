#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readxl)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)

ocean_debris <- readxl::read_excel('../data/2018/Ocean/FISH-INVERT-DEBRIS-BIGHT18-CONNECTOR.xlsx', sheet = 5)

tot_count <- ocean_debris %>% 
  mutate(debriscount = if_else(debriscount < 0, 0, debriscount)) %>% 
  group_by(debristype) %>% 
  summarise(
    n_total = sum(debriscount)
  ) %>%
  arrange(desc(n_total))

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
                        "Other Foliose Algae", "Other Marine (comment req.)") ~ 'Marine Debris',
      TRUE ~ NA_character_
    )
  )


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Trash and Marine Debris - Bight 2018"),

    sidebarPanel(
    selectInput("Variable","Data:",
                       c("Ocean_Debris18", 
                         "River_Debris18")), 
   
    selectInput("Variable", "Plot:",
                c("Total_Ocean_Debris_Count18",
                  'somethingthing'))
    ),
    
    mainPanel(
      tableOutput("ocean18"),
      plotOutput("totalcount_ocean18")
    )
)



server <- function(input, output) {
  output$ocean18 <- renderTable({
    big_type
    
  })
  
  output$totalcount_ocean18 <- renderPlot({
    big_type %>% 
      group_by(type) %>% 
      summarise(
        n = sum(n_total)
      ) %>% 
      drop_na() %>% 
      mutate(
        p = scales::percent(n/sum(n)),
        type = factor(type, levels = c('Recyclable', 'Rock', 'Plastic', 'Degradable', 'Trash', 'Marine Debris'))
      ) %>% 
      ggplot(aes(x = type, y = n, fill = type)) +
      geom_col() +
      geom_text(aes(label = p, y = n+10)) +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = '', y = 'Total Count')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
