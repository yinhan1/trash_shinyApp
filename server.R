



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
    
    
    
    
    
    
    
    #### tab 3 output: area weight mean count ####
    output$trash_count <- renderPlot({
        call_river_2013() %>%
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
    get_current <- reactive({
        if (input$Year == 2013 & input$Type == "River") {
            d = read.csv("./data/2013/River/trash_areaweighted_count_by_county.csv")
        }
        else if (input$Year == 2013 & input$Type == "Ocean") {
            d = readxl::read_excel("./data/2013/Ocean/Debris Ocean 2013.xlsx")
        }
        else if (input$Year == 2018 & input$Type == "River") {
            d = read.csv("./data/2018/River/river_2018.csv")
        }
        else if (input$Year == 2018 & input$Type == "Ocean") {
            d = read.csv("./data/2018/Ocean/ocean_2018.csv")
        }
        else{
            d = NULL
        }
    })
    output$tb_displayed <- renderDataTable(get_current())
    
    
    
    #### tab 6 output: summary ####
    
    
    
    
    
    
    
    
})
