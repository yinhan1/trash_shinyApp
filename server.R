
library(shiny)

source("./script/functions.R")

#### server ----------------- ####
shinyServer(function(input, output) {
    
    #### tab 1 output ####
    # markdown output 
    

    #### tab 2 output: station mapper ####
    
    output$tab2 <- renderLeaflet(tab2_call_map(input$tab2_Type, input$tab2_Year))
    output$tab2_ocean_site_count <- renderText(paste("40", "Ocean Sites"))
    output$tab2_river_site_count <- renderText(paste("118", "River Sites"))
    output$tab2_site_count_year <- renderText(paste("sampled in", input$tab2_Year))
    output$tab2_site_count_bar <- renderPlot(tab2_site_count_plotter())
    
    
    #### tab 3 output: area weight mean count ####
    pt1 <- reactive({
        if (!input$donum1) return(NULL)
        area_w_stratum
    })
    pt2 <- reactive({
        if (!input$donum2) return(NULL)
        total_count_area
        
    })
    pt3 <- reactive({
        if (!input$donum3) return(NULL)
        trash_w_stratum
    })
    pt4 <- reactive({
        if (!input$donum4) return(NULL)
        trash_w_county
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
    
    
    
    
    #### tab 4 output: distance to nearest road ####
    
    
    
    
    
    
    
    #### tab 5 output: data ####
    output$tab5 <- renderDataTable(
        tab5_call_tb(input$tab5_Type, input$tab5_Year)
    )
    
    
    
    #### tab 6 output: summary ####
    # markdown output 
    
})
