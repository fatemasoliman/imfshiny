library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(reshape2)
library(shiny)
library(googleVis)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  # output$arrtypesnbar <- renderPlotly({
  #   
  #   p = arrangementsbytype %>% filter(arrTypeGroup %in% c(input$arrtype)) %>% ggplot() +
  #     geom_bar(aes(x = Approval.Year, y = tot, fill = arrTypeGroup),
  #              stat = "identity") +
  #     xlab("Approval Year") + ylab("Number of Approved Arrangements") + theme_classic() +
  #     theme(legend.position = "bottom") + scale_fill_brewer(palette = "Paired") +
  #     labs(title = "Approved Arrangements by Year")
  #   
  #   ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  #   
  # })
  # output$arrtypesamountsbar <- renderPlotly({
  #   arrangementsbytype %>% ggplot() +
  #     geom_bar(aes(x = Approval.Year, y = totalaccessamount), fill = 'deepskyblue3', 
  #              stat = "identity") +
  #     xlab("Approval Year") + ylab("Total Access Amount (SDR)") + theme_classic() +
  #     theme(legend.position = "bottom") + labs(title = "Total Access Amount of Approved Arrangements")
  #   
  # })
  
  output$totalbar <- renderPlotly({
   a =  arrangementsbytype %>%  rename("yvar" = input$graphtype) %>% 
      filter(arrTypeGroup %in% c(input$arrtype)) %>% ggplot() +
      geom_bar(aes(x = Approval.Year, 
                   y = yvar, fill = arrTypeGroup, 
                   text = paste('Year: ', Approval.Year,
                                       '<br>', yvar)), 
               stat = "identity") + theme_classic() +
      theme(legend.position = "bottom") + scale_fill_brewer(palette = "Paired") +
      labs(x = "Approval Year", 
           y = if_else(input$graphtype == "tot", "Number of Approved Arrangments", "Total Access Amount (mn SDR)"))
  
   ggplotly(a, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2)) 
    
    
  })
  # 
  # output$typepie <- renderPlotly({
  #   c = arrangementsbytype %>% group_by(arrTypeGroup) %>% summarise(narr = n()) %>% 
  #     ggplot() +
  #     geom_bar(aes(x = factor(1), y = narr, fill = factor(arrTypeGroup)), width = 1, 
  #               stat = "identity") 
  #   c+ coord_polar(theta = "y")
  #      # coord_polar(theta = "y")
  #     # # geom_text(aes(y = arrTypeGroup, label = narr), color = "white")+
  #     # scale_fill_brewer(palette = "Paired") +
  #     # theme_classic()
  # 
  # })

  output$regionmap = renderGvis({
    gvisGeoChart(descriptions %>% group_by(Country.Name, Region.Code) %>% 
                   summarise('Access Amount mnSDR' = sum(Totalaccess), 'Number of Loans' = n()) %>% 
                   filter('Access Amount mnSDR'>0) %>% filter(input$region == Region.Code), 
                 "Country.Name",input$vartype2,
      options = list( region = input$region,displayMode = "regions", 
                      width = 1000,height = 600)
    )
  })
  
  output$worldmap = renderGvis({
    gvisGeoChart(descriptions %>% group_by(Country.Name) %>% 
                   summarise("Access Amount mnSDR" = sum(Totalaccess), 'Number of Loans' = n()) %>% 
                   filter('Access Amount mnSDR'>0), 
                 "Country.Name",input$vartype3,
                 options = list( region = "world",displayMode = "regions", 
                                 width = 1000,height = 600)
    )
  })
  
  output$scat1 = renderPlotly({

    c =  grabycountry %>%
      filter(Totalaccess >= input$totalccesslider[1] & Totalaccess<= input$totalccesslider[2]) %>%
      ggplot() +
      geom_jitter(aes(x = Approval.Date, y= Totalaccess, color = Arrangement.Type)) +
      labs(x = "Approval Date", y = "Total Access Amounts (mn SDR)", color = "Arrangement Type") +
      theme_classic() + 
      scale_x_discrete(breaks=c(paste("01/01/", c(2000:2019), sep ='')),
                                         labels=c(2000:2019)) + theme(legend.position = "none")

    ggplotly(c, tooltip = "text")


  })

  output$scat2 = renderPlotly({

    d =  grabycountry %>% group_by(Country.Name, Arrangement.Type) %>%
      summarise(nloans = n(), totacc = sum(Totalaccess)) %>%
      filter(totacc >= input$totalccesslider[1] & totacc<= input$totalccesslider[2]) %>%
      ggplot() +
      geom_jitter(aes(x = nloans, y= totacc,color = Arrangement.Type,
                     text = paste(Country.Name))) +
      labs(x = "Number of Loans", y = "Total Access Amounts (mn SDR)") +
      theme_classic()
      
    ggplotly(d, tooltip = "text") %>%  layout(legend = list(orientation = "h", x = 0.4, y = -0.2))


  })


  
  
})
