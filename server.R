
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
    
    output$tab3_title1 <- renderText(paste(input$tab3_Type, " Data Collected in ", input$tab3_Year))
    output$tab3_title2 <- renderText(paste("Trend of ", input$tab3_Type, " Data Over 2013 to 2018"))
    
    # single year 
    
    ## by stratum
    output$tab3_ttl_count_by_stratum_plot <- renderPlot(tab3_by_year_plotter(input$tab3_Year, input$tab3_Type, "stratum", plot_tt_cnt = T))
    output$tab3_area_count_by_stratum_plot <- renderPlot(tab3_by_year_plotter(input$tab3_Year, input$tab3_Type, "stratum", plot_tt_cnt = F))
    ## by county
    output$tab3_ttl_count_by_county_plot <- renderPlot(tab3_by_year_plotter(input$tab3_Year, input$tab3_Type, "county", plot_tt_cnt = T))
    output$tab3_area_count_by_county_plot <- renderPlot(tab3_by_year_plotter(input$tab3_Year, input$tab3_Type, "county", plot_tt_cnt = F))
    ## by trash types
    output$tab3_ttl_count_by_trashType_plot <- renderPlot(tab3_by_year_plotter(input$tab3_Year, input$tab3_Type, "trashType", plot_tt_cnt = T))
    output$tab3_area_count_by_trashType_plot <- renderPlot(tab3_by_year_plotter(input$tab3_Year, input$tab3_Type, "trashType", plot_tt_cnt = F))
    
    # relative percent 
    output$tab3_relative_by_stratum_plot <- renderPlot(tab3_relative_plotter(input$tab3_Year, input$tab3_Type))
    
    # compare 
    output$tab3_compare_stratum_plot <- renderPlot(tab3_compare_plotter(input$tab3_Type, "stratum"))
    output$tab3_compare_county_plot <- renderPlot(tab3_compare_plotter(input$tab3_Type, "county"))
    output$tab3_compare_trashType_plot <- renderPlot(tab3_compare_plotter(input$tab3_Type, "trashType"))
    
    
    #### tab 4 output: distance to nearest road ####
    
  
    
    #### tab 5 output: data ####
    output$tab5 <- renderDataTable(
      datatable(tab3_pick_data(input$tab5_Year, input$tab5_Type), options = list("pageLength" = 15))
      )
    
    
    
    #### tab 6 output: summary ####
    # markdown output 
    
})
