#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    get_current <- reactive({
        if (input$Year == 2013 & input$Type == "River"){d=read.csv("./data/2013/River/trash_areaweighted_count_by_county.csv")}
        else if (input$Year == 2013 & input$Type == "Ocean"){d=readxl::read_excel("./data/2013/Ocean/Debris Ocean 2013.xlsx")}
        else if (input$Year == 2018 & input$Type == "River"){d=read.csv("./data/2018/River/river_2018.csv")}
        else if (input$Year == 2018 & input$Type == "Ocean"){d=read.csv("./data/2018/Ocean/ocean_2018.csv")}
        else{d=NULL}
    })
    
   
    output$tb_displayed <- renderDataTable(get_current())
})

