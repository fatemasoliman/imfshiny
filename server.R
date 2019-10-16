library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(reshape2)
library(shiny)
library(googleVis)
library(RColorBrewer)

shinyServer(function(input, output, session) {
  
    observe({
      
    arrtypebop = grabycountry %>% filter(Country.Name == input$countrybop) %>% select(arrTypeGroup)
    
    if(length(arrtypebop)==1){
      arrtypebop = arrtypebop[[1]]
    }
    
    updateCheckboxGroupInput(session = session, inputId = "arrtypebop",
                         choices = unique(arrtypebop), 
                       selected = arrtypebop)
    
    
    # rmaparrtype = grabycountry %>% filter(Region.Code==input$region) %>% select(arrTypeGroup)
    # 
    #   if(length(rmaparrtype)==1){
    #     rmaparrtype = rmaparrtype[[1]]
    #   }
    # 
    #   updateCheckboxGroupInput(session = session, inputId = "rmaparrtype",
    #                            choices = unique(rmaparrtype),
    #                            selected = unique(rmaparrtype))
                      })

  
  
  # observe({
  #   
  #   rmaparrtype = grabycountry %>% filter(Region.Code==input$region) %>% select(arrTypeGroup)
  # 
  #   if(length(rmaparrtype)==1){
  #     rmaparrtype = rmaparrtype[[1]]
  #   }
  #   
  #   updateCheckboxGroupInput(session = session2, inputId = "rmaparrtype",
  #                            choices = unique(rmaparrtype), 
  #                            selected = rmaparrtype)
  # })
  
  
  
  
  
  output$totalbar <- renderPlotly({
   a =  arrangementsbytype %>%  rename("yvar" = input$graphtype) %>% 
      filter(arrTypeGroup %in% c(input$arrtype)) %>% ggplot() +
      geom_bar(aes(x = Approval.Year, 
                   y = yvar, fill = arrTypeGroup, 
                   text = paste('Year: ', Approval.Year,
                                       '<br>', yvar)), 
               stat = "identity", position = (input$stackfill)) + theme_classic() +
      theme(legend.position = "bottom") + scale_fill_brewer(palette = "Paired") +
      labs(x = "Approval Year", 
           y = if_else(input$graphtype == "tot", "Number of Approved Arrangments", "Total Access Amount (mn SDR)"))
  
   ggplotly(a, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.4)) 
    
    
  })
  
  
  output$regionbar <- renderPlotly({
    r =  byregion %>% 
      filter(arrTypeGroup%in%c(input$arrtype)) %>% 
      group_by(Region.Name, Approval.Year) %>% 
      summarise(tot = sum(n), totalaccessamount = sum(Totalaccess)) %>% 
      group_by(Region.Name) %>% rename("yvar" = input$graphtype) %>% 
      ggplot() +geom_bar(aes(x = Approval.Year, y = yvar, fill = Region.Name, 
                             text = paste('Year: ', Approval.Year,
                                          '<br>', yvar)), 
                         stat = "identity", position = (input$stackfill)) + 
      theme_classic() +
      theme(legend.position = "bottom") + scale_fill_brewer(palette = "Paired") +
      labs(x = "Approval Year", 
           y = if_else(input$graphtype == "tot", "Number of Approved Arrangments", "Total Access Amount (mn SDR)"))
    
    ggplotly(r, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.4)) 
    
  })
  
  

  output$regionmap = renderGvis({
    
    rdf = descriptions %>% 
      filter(Region.Code == input$region) %>% 
      group_by(Country.Name, Region.Code) %>%
      summarise('Access Amount mnSDR' = sum(Totalaccess), 'Number of Loans' = n())
    
    gvisGeoChart(rdf,
                 "Country.Name",input$vartype2,
                 options = list( region = input$region,displayMode = "regions",
                                 width = 1000,height = 600)
    )
  
    #   gvisGeoChart(descriptions %>% filter(arrTypeGroup %in% c(input$rmaparrtype)) %>%
    #                group_by(Country.Name, Region.Code) %>%
    #                summarise('Access Amount mnSDR' = sum(Totalaccess), 'Number of Loans' = n()) %>%
    #                filter('Access Amount mnSDR'>0) %>% filter(input$region == Region.Code),
    #              "Country.Name",input$vartype2,
    #              options = list( region = input$region,displayMode = "regions",
    #                              width = 1000,height = 600)
    # )
 
    
  })
  
  output$worldmap = renderGvis({
    wm = descriptions %>%  filter(arrTypeGroup %in% c(input$wmaparrtype)) %>% 
      group_by(Country.Name) %>% 
      summarise("Access Amount mnSDR" = sum(Totalaccess), 'Number of Loans' = n()) %>% 
      filter('Access Amount mnSDR'>0)
             
    gvisGeoChart(wm, 
                 "Country.Name",input$vartype3,
                 options = list( region = "world",displayMode = "regions", 
                                 width = 1000,height = 600)
    )
  })
  
  output$scat1 = renderPlotly({

    c =  grabycountry %>%
      filter(Totalaccess >= input$totalccesslider[1] & Totalaccess<= input$totalccesslider[2], 
             arrTypeGroup %in% c(input$scatarrtype)) %>%
      ggplot() +
      geom_jitter(aes(x = Approval.Year, y= Totalaccess, color = arrTypeGroup, 
                      text = paste(Country.Name, '<br>', "Date: ", Approval.Year, 
                                   '<br>', "Amount: ", Totalaccess))) +
      labs(x = "Approval Year", y = "Total Access Amounts (mn SDR)", color = "Arrangement Type") +
      theme_classic() + scale_color_brewer(palette = "Paired")+
     theme(legend.position = "none")
    
    ggplotly(c, tooltip = "text")


  })

  output$scat2 = renderPlotly({

    d =  grabycountry %>% group_by(Country.Name, arrTypeGroup) %>%
      summarise(nloans = n(), totacc = sum(Totalaccess)) %>%
      filter(totacc >= input$totalccesslider[1] & totacc<= input$totalccesslider[2], 
             arrTypeGroup %in% c(input$scatarrtype)) %>%
      ggplot() +
      geom_jitter(aes(x = nloans, y= totacc,color = arrTypeGroup,
                     text = paste(Country.Name, '<br>', 
                                  '<br>', 'Number of loans: ', nloans,
                                    '<br>', "Amount: ", totacc))) +
      labs(x = "Number of Loans", y = "Total Access Amounts (mn SDR)") +
      theme_classic() + scale_color_brewer(palette = "Paired")
    ggplotly(d, tooltip = "text") %>%  layout(legend = list(orientation = "h", x = 0.4, y = -0.4), 
                                              xaxis = list(
                                                dtick = 1,
                                                tickmode = "linear"))
  })

  output$bop = renderPlotly({
    
    yrs = unlist(grabycountry %>% 
                   filter(Country.Name %in% c(input$countrybop), 
                          arrTypeGroup %in% c(input$arrtypebop)) %>% select(Inipgmyr))
    
    countries = unlist(grabycountry %>%
                         filter(Country.Name %in% c(input$countrybop), 
                                arrTypeGroup %in% c(input$arrtypebop)) %>% 
                         select(Country.Name))
    
    arrtypes = unlist(grabycountry %>%
                        filter(Country.Name %in% c(input$countrybop), 
                               arrTypeGroup %in% c(input$arrtypebop)) %>% 
                        select(arrTypeGroup))
    
    tam = unlist(grabycountry %>%
                   filter(Country.Name %in% c(input$countrybop), 
                          arrTypeGroup %in% c(input$arrtypebop)) %>% 
                   select(Totalaccess))
      
    yrs_end = unlist(grabycountry %>%
                       filter(Country.Name %in% c(input$countrybop), 
                              arrTypeGroup %in% c(input$arrtypebop)) %>% 
                       select(Initial.End.Year))
    
    vlines_start = data.frame(xint = c(yrs), grp = c(countries), at = arrtypes, xintend = c(yrs_end), tam)
    
    e =  cab %>% filter(Country.Name %in% c(input$countrybop)) %>%
      ggplot(aes(
        x = year,
        y = bop)) +
      geom_line(color = "deepskyblue3") +
      labs(x = "Year", y = "BoP (% of GDP)") +
      theme_classic() +
      geom_vline(data = vlines_start,aes(xintercept = xint, color = at,
                                         text = paste(tam, " mn SDR")), linetype = "dashed") +
      scale_colour_brewer(palette = "Set1")

    ggplotly(e, tooltip = "text") %>%  layout(legend = list(orientation = "h", x = 0.4, y = -0.4),
                                              xaxis = list(
                                                dtick = 2,
                                                tickmode = "linear"))

  })
  
  output$successpie = renderGvis({
    
    piedata = success %>% ungroup() %>% filter(Region.Code %in% input$regionpie, 
                                               Arrangement.Type %in% input$gratype) %>% 
      ungroup() %>% 
      summarise("Year 1" = sum(n1), "Year 2"= sum(n2), 
                "Year 3"= sum(n3), "No Improvement" = sum(none)) %>% melt() %>% 
      rename("yearimpr" = variable, "n" = value) 
    
    gvisPieChart(piedata, options = list(legend = "bottom", 
                                         width = "600",
                                         height = "600"))

  })
  
  output$successbar1 = renderPlotly({
    bardata = success %>% filter(Region.Code %in% input$regionpie, 
                                 arrTypeGroup %in% input$gratype) %>% 
      group_by(arrTypeGroup) %>% 
      summarise("Year 1" = sum(n1), "Year 2"= sum(n2), 
                "Year 3"= sum(n3), "No Improvement" = sum(none)) %>% melt()
    
   succbar= bardata %>% ggplot() + 
     geom_bar(aes(x = arrTypeGroup, y = value, fill = variable ), 
              stat = "identity", position = input$stackfillbop) + theme_classic() +
     scale_fill_brewer(palette = "Paired") + 
     labs(x = "Arrangement Type", y = "Number or Proportion of Loans")
   
    
    ggplotly(succbar) %>%  layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
    
  })
  
  output$successbar2 = renderPlotly({
    bardata = success %>% filter(Region.Code %in% input$regionpie, 
                                 arrTypeGroup %in% input$gratype) %>%
      group_by(Region.Name) %>% 
      summarise("Year 1" = sum(n1), "Year 2"= sum(n2), 
                "Year 3"= sum(n3), "No Improvement" = sum(none)) %>% melt() 
    
    succbar= bardata %>% ggplot() + geom_bar(aes(x = Region.Name, y = value, fill = variable ), 
                                             stat = "identity", position = input$stackfillbop) + theme_classic() +
      scale_fill_brewer(palette = "Paired") + labs(x = "Region", 
                                                   y = "Number or Proportion of Loans")
    
    
    ggplotly(succbar) %>%  layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
    
  })
  
  

})
  
