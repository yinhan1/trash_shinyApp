
library(shiny)

source("./script/functions.R")

#### server ----------------- ####
shinyServer(function(input, output) {
    
    #### tab 1 output ####
    # markdown output 
    

    #### tab 2 output: station mapper ####
    
    output$tab2_map <- renderLeaflet(tab2_call_map(input$tab2_Year))
    output$tab2_ocean_site_count <- renderText(tab2_ocean_sites(input$tab2_Year))
    output$tab2_river_site_count <- renderText(tab2_river_sites(input$tab2_Year))
    output$tab2_site_count_year <- renderText(paste("sampled in", input$tab2_Year))
    output$tab2_site_count_bar <- renderPlot(tab2_site_count_plotter())
    
    
    #### tab 3 output: area weight mean count ####
    pt1 <- reactive({
        if (!input$donum1) return(NULL)
      count_by_stratum
    })
    pt2 <- reactive({
        if (!input$donum2) return(NULL)
      count_by_region
        
    })
    pt3 <- reactive({
        if (!input$donum3) return(NULL)
      count_by_land
    })
    pt4 <- reactive({
        if (!input$donum4) return(NULL)
      relative_count
    })
    output$plotgraph = renderPlot({
        ptlist <- list(pt1(),pt2(),pt3(),pt4())
        # wtlist <- c(input$wt1,input$wt2,input$wt3)
        # remove the null plots from ptlist and wtlist
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        # wtlist <- wtlist[to_delete]
        if (length(ptlist)==0) return(NULL)
        
        grid.arrange(grobs=ptlist,nrow=length(ptlist))
    })
    
    #### tab 3.5: magnitude and frequency ####
    
    # single year 
    
    output$tab3_example1 <- renderPlot(tab3_plotter(input$tab3_Year, input$tab3_Type))
    output$tab3_example2 <- renderPlot(tab3_plotter(input$tab3_Year, input$tab3_Type))
    output$tab3_example3 <- renderPlot(tab3_plotter(input$tab3_Year, input$tab3_Type))
    output$tab3_example4 <- renderPlot(tab3_plotter(input$tab3_Year, input$tab3_Type))
    
    # compare 
    
    output$tab3_compare_11 <- renderPlot(tab3_plotter(2013, input$tab3_Type))
    output$tab3_compare_12 <- renderPlot(tab3_plotter(2018, input$tab3_Type))
    output$tab3_compare_21 <- renderPlot(tab3_plotter(2013, input$tab3_Type))
    output$tab3_compare_22 <- renderPlot(tab3_plotter(2018, input$tab3_Type))
    output$tab3_compare_31 <- renderPlot(tab3_plotter(2013, input$tab3_Type))
    output$tab3_compare_32 <- renderPlot(tab3_plotter(2018, input$tab3_Type))
    output$tab3_compare_41 <- renderPlot(tab3_plotter(2013, input$tab3_Type))
    output$tab3_compare_42 <- renderPlot(tab3_plotter(2018, input$tab3_Type))
    
    #### tab 4 output: distance to nearest road ####
    
    
    
    
    
    
    
    #### tab 5 output: data ####
    output$tab5 <- renderDataTable(
        tab5_call_tb(input$tab5_Type, input$tab5_Year)
    )
    
    
    
    #### tab 6 output: summary ####
    # markdown output 
    
})
