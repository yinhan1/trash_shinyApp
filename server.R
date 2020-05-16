
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
    
    
    #### tab 3.5: magnitude and frequency ####
  
    output$tab3_title <- renderText(paste("Analysis Report of ", input$tab3_Type, " Data Over 2013 to 2018"))
    
    output$tab3_test_plot0 <- renderPlot(tab3_total_cnt_plotter(input$tab3_Type,"type"))
    
    output$tab3_test_plot1 <- renderPlot(tab3_total_cnt_plotter(input$tab3_Type, "stratum"))
    output$tab3_test_plot2 <- renderPlot(tab3_pArea_covered_plotter(input$tab3_Type, "stratum"))
    
    output$tab3_test_plot3 <- renderPlot(tab3_total_cnt_plotter(input$tab3_Type, "county"))
    output$tab3_test_plot4 <- renderPlot(tab3_pArea_covered_plotter(input$tab3_Type, "county"))
    
    output$tab3_test_plot5 <- renderPlot(tab3_rel_abun_plotter(2013, input$tab3_Type))
    output$tab3_test_plot6 <- renderPlot(tab3_rel_abun_plotter(2018, input$tab3_Type))
    
    
    #### tab 4 output: distance to nearest road ####
    
  
    
    #### tab 5 output: data ####
    tb5 <- reactive(tab3_pick_data(input$tab5_Year, input$tab5_Type))
    output$tab5 <- renderDataTable(datatable(tb5(), options = list("pageLength" = 15)))
    
    
    #### tab 6 output: summary ####
    # markdown output 
    
})
