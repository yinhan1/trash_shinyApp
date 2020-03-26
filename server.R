



library(shiny)

source("./script/functions.R")



#### server ----------------- ####
shinyServer(function(input, output) {
    #### tab 1 output ####
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white')
        
    })
    
    
    #### tab 2 output: station mapper ####
    output$tab2 <- renderLeaflet(
        tab2_call_map(input$tab2_Type, input$tab2_Year)
    )
    output$tab2_ocean_site_count <- renderText(paste("200", "ocean sites"))
    output$tab2_river_site_count <- renderText(paste("250", "river sites"))
    output$tab2_site_count_year <- renderText(paste("sampled in", input$tab2_Year))
    
    output$tab2_mean_count_bar <- renderPlot(tab2_mean_count_plotter()
    )
    
    
    #### tab 3 output: area weight mean count ####
    output$trash_count <- renderPlot({
        tab3_call_river_2013() %>%
            group_by(county, stratum) %>%
            summarise(`Total Area` = sum(areaweight),
                      `Total Count` = sum(totalcount)) %>%
            pivot_longer(cols = c(`Total Count`, `Total Area`)) %>%
            mutate_at('name', factor, levels = c('Total Count', 'Total Area')) %>%
            ggplot() +
            geom_col(aes(
                x = stratum,
                y = value,
                group = county,
                fill = stratum
            )) +
            facet_grid(name ~ county, scales = 'free_y') +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = '',
                 y = '')
    })
    
    
    
    
    #### tab 4 output: distance to nearest road ####
    
    
    
    
    
    
    
    #### tab 5 output: data ####
    output$tab5 <- renderDataTable(
        tab5_call_tb(input$tab5_Type, input$tab5_Year)
    )
    
    
    
    #### tab 6 output: summary ####
    
    
    
    
    
    
    
    
})
