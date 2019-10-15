


library(shiny)
library(shinythemes)


navbarPage(theme = shinytheme("sandstone"), "IMF LOAN DATA",
           tabPanel("Total Loans 2000-2019",
             fluidPage(
                 sidebarPanel(
                   checkboxGroupInput(
                     inputId = "arrtype",
                     label = h4("Arrangement Type"),
                     choices = c("Poverty Reduction and Growth Trust (PRGT)", "General Resources Account (GRA)",
                     "Policy Support Instrument (PSI) (non-monetary)" = "Policy Support Instrument (PSI)",
                     "Policy Coordination Instrument (PCI) (non-monetary)" = 
                       "Policy Coordination Instrument (PCI)"),
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
                   ),
                   radioButtons(
                     inputId = "stackfill",
                     label = h4("Stack or Fill?"),
                     choices = c("stack", "fill"),
                     selected = "stack"
                     )
                 ),
                 mainPanel(plotlyOutput("totalbar"), br(), br(), br(), plotlyOutput("regionbar"))
               )),
           
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
              
           
           tabPanel("GRA Loans", fluidPage(
             sidebarPanel(
               sliderInput(
                 inputId = 'totalccesslider',
                 label = "Total Access Amounts (mn SDR)",
                 min = 0, max = 63000, value = c(0,2000)
               ),
               
               checkboxGroupInput(
                 inputId = "scatarrtype",
                 label = h4("Arrangement Type"),
                 choices = unique(grabycountry$Arrangement.Type),
                 selected = unique(grabycountry$Arrangement.Type)
                 )
                 ),
             mainPanel(plotlyOutput("scat1"), br(), br(), br(), plotlyOutput("scat2")))),
           
           tabPanel("Balance of Payments and GRA Loans", fluidPage(
             sidebarPanel(
               selectInput(
                 inputId = 'countrybop',
                 label = "Select Countries",
                choices = c(unique(grabycountry$Country.Name)),
               selected = "COLOMBIA"
                # multiple = TRUE
               ),
               checkboxGroupInput(
                 inputId = "arrtypebop",
                 label = h4("Arrangement Type"),
                choices = unique(grabycountry %>% select(Arrangement.Type))
                 # multiple = TRUE
             )),
             mainPanel(plotlyOutput("bop") ))),
           
           tabPanel("BoP - YoY Improvement", fluidPage(
             sidebarPanel(
               checkboxGroupInput(
               inputId = "regionpie",
               label = h3("Region"),
               choices = c(
                 "South America" = '005',
                 "Central America" = '013',
                 "Asia" = '142',
                 "Europe" = '150',
                 "Africa" = '002',
                 "Oceania" = '009'
               ),
               selected = c("002", "005", "013", "142", "009", "150")
             ),
             checkboxGroupInput(
               inputId = "gratype",
               label = h3("GRA Type"),
               choices = c("EFF","SBA", "FCL", "ESF", "SBA-SCF","SBA-ESF","PLL", "PCL"),
               selected = c("EFF","SBA", "FCL", "ESF", "SBA-SCF","SBA-ESF","PLL", "PCL"))
             ),
             mainPanel(plotlyOutput("successbar1"), br(), br(), br(), plotlyOutput("successbar2")) 
             ))

           
)