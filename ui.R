


library(shiny)
library(shinythemes)


navbarPage(theme = shinytheme("sandstone"), "IMF LOAN DATA",
           tabPanel("Total Loans 2000-2019",
             fluidPage(
                 sidebarPanel(
                   checkboxGroupInput(
                     inputId = "arrtype",
                     label = h4("Arrangement Type"),
                     choices = unique(totarrangements$arrTypeGroup),
                     selected = c(
                       "Poverty Reduction and Growth Trust (PRGT)",
                       "General Resources Account (GRA)",
                       "Policy Support Instrument (PSI)",
                       "Policy Coordination Instrument (PCI)"
                     )
                   ),
                   br(),
                   br(),
                   radioButtons(
                     inputId = "graphtype",
                     label = h4("Variable"),
                     choices = c(
                       "Number of arrangment approvals" = "tot",
                       "Total Access Amounts" = "totalaccessamount"
                     )
                   )
                 ),
                 mainPanel(plotlyOutput("totalbar"))
               )),
               tabPanel("Region Maps", 
                 "Loans by Region",
                 sidebarPanel(
                   radioButtons(
                     inputId = "region",
                     label = h3("Region"),
                     choices = c(
                       "South America" = '005',
                       "Central America" = '013',
                       "Asia" = '142',
                       "Europe" = '150',
                       "Africa" = '002',
                       "Oceania" = '009'
                     ),
                     selected = c('005')
                   ),
                   radioButtons(
                     inputId = 'vartype2',
                     label = h4("Variable"),
                     choices = c("Number of Loans",
                                 "Total Access Amount" = "Access Amount mnSDR")
                   )
                 ),
                 mainPanel(htmlOutput("regionmap"))
               ),
               
               tabPanel("World Map", fluidPage(
                 sidebarPanel(
                   radioButtons(
                     inputId = 'vartype3',
                     label = h3("Variable"),
                     choices = c("Number of Loans",
                                 "Total Access Amount" = "Access Amount mnSDR")
                   )
                 ),
                 mainPanel(htmlOutput("worldmap")))),
           
           tabPanel("By Type", fluidPage(
             sidebarPanel(
               sliderInput(
                 inputId = 'totalccesslider',
                 label = "Total Access Amounts (mn SDR)",
                 min = 0, max = 63000, value = c(0,2000)
               )
             ),
             mainPanel(plotlyOutput("scat1"), br(), br(), br(), br(), plotlyOutput("scat2")))),
           
           tabPanel("BoP", fluidPage(
             sidebarPanel(
               selectInput(
                 inputId = 'countrybop',
                 label = "Select Countries",
                choices = cabimfcountries$Country.Name,
                selected = c("AFGHANISTAN", "MEXICO", "COLOMBIA", "EGYPT"),
                multiple = TRUE
               )
             ),
             mainPanel(plotlyOutput("bop"))))
           
           )