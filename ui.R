


library(shiny)
library(shinythemes)


navbarPage(theme = shinytheme("sandstone"), "",
           tabPanel("Home", fluidPage( fluidRow( 
                    br(), br(), br(), br(),
                    column(1,""),
                    column(2,
                           br(), br(), 
                    img(src='logo.png', height = "200%", width = "200%",
                        align = "left")), 
                    column(3, ""),
                    column(4,
                          br(), br(), br() ,
                          h4("Established by the Bretton Woods Agreement in 1944, the International Monetary Fund seeks 
                          to ensure the stability of the international monetary system by promoting 
                          international monetary co-operation, international trade, high employment, 
                          exchange-rate stability, sustainable economic growth, and making resources 
                          available to member countries in financial difficulty.") ,
                          br(),
                          h4("The fund currently has SDR 204 billion, equivalent to $283 billion."),
                          br(),
                          h4("The IMF has a number of lending and policy instruments, including: ",
                             br(), br(), "- General Resources Account (GRA): available to all member states",
                             br(), br(), "- Poverty Reduction and Growth Trust (PRGT): concessional financial support to LICs",
                             br(), br(), "- Policy Coordination Instrument (PCI): non-financing tool for all member states",
                             br(),  br(), "- Policy Support Instrument (PSI): non-financing tool for LICs"
                             )
                    )))),
           
           tabPanel("Total Loans 2000-2019",
             fluidPage(
                 sidebarPanel(
                   checkboxGroupInput(
                     inputId = "arrtype",
                     label = h4("Arrangement type"),
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
                       "Total access amounts" = "totalaccessamount"
                     )
                   ),
                   radioButtons(
                     inputId = "stackfill",
                     label = h4("Bar type"),
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
                 label = h4("Variable"),
                 choices = c("Number of loans" = "Number of Loans",
                             "Total access amount" = "Access Amount mnSDR")
               ),
               checkboxGroupInput(
                 inputId = 'wmaparrtype',
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
               )
             ),
             mainPanel(htmlOutput("worldmap")))),
           
           
               tabPanel("Region Maps", 
                 "Loans by Region",
                 sidebarPanel(
                   selectInput(
                     inputId = "region",
                     label = h4("Region"),
                     choices = c(
                       "South America" = '005',
                       "Central America" = '013',
                       "Asia" = '142',
                       "Europe" = '150',
                       "Africa" = '002',
                       "Oceania" = '009'
                     ),
                      selected = "002"
                   ),
                   radioButtons(
                     inputId = 'vartype2',
                     label = h4("Variable"),
                     choices = c("Number of loans" = "Number of Loans",
                                 "Total access amount" = "Access Amount mnSDR")
                   )
                   # checkboxGroupInput(
                   #   inputId = 'rmaparrtype',
                   #   label = h4("Arrangement Type"),
                   #   choices = unique(grabycountry$arrTypeGroup),
                   #   # selected = c(
                   #   #   "Poverty Reduction and Growth Trust (PRGT)",
                   #   #   "General Resources Account (GRA)",
                   #   #   "Policy Support Instrument (PSI)",
                   #   #   "Policy Coordination Instrument (PCI)"
                   #   # )
                   # )
                 ),
                 mainPanel(htmlOutput("regionmap"))
               ),
              
           
           tabPanel("PRGF & GRA Loans", fluidPage(
             sidebarPanel(
               sliderInput(
                 inputId = 'totalccesslider',
                 label = h4("Total access amounts (mn SDR)"),
                 min = 0, max = 400000, value = c(0,2000)
               ),
               
               checkboxGroupInput(
                 inputId = "scatarrtype",
                 label = h4("Arrangement Type"),
                 choices = c("Poverty Reduction and Growth Trust (PRGT)",
                              "General Resources Account (GRA)",
                              "Policy Support Instrument (PSI)",
                              "Policy Coordination Instrument (PCI)"),
                 selected = unique(grabycountry$arrTypeGroup)
                 )
                 ),
             mainPanel(plotlyOutput("scat1"), br(), br(), br(), plotlyOutput("scat2")))),
           
           tabPanel("Balance of Payments and Loans", fluidPage(
             sidebarPanel(
               selectInput(
                 inputId = 'countrybop',
                 label = h4("Select countries"),
                choices = c(unique(cabgra$Country.Name)),
               selected = "COLOMBIA"
                # multiple = TRUE
               ),
               checkboxGroupInput(
                 inputId = "arrtypebop",
                 label = h4("Arrangement type"),
                choices = unique(grabycountry %>% select(arrTypeGroup))
                 # multiple = TRUE
             )),
             mainPanel(plotlyOutput("bop"),
                       br(), br(), 
                       h4("The Balance of Payments (BoP) is an account of a country's international transcations
                          over a period of time. Countries seeking IMF aid are often experiencing large and deteriorting 
                          BoP deficits. An improvement in the BoP is the IMF's primary indicator of success.")))),
           
           tabPanel("BoP - YoY Improvement", fluidPage(
             sidebarPanel(
               checkboxGroupInput(
               inputId = "regionpie",
               label = h4("Region"),
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
               label = h4("Arrangement type"),
               choices = unique(grabycountry$arrTypeGroup),
               selected = unique(grabycountry$arrTypeGroup)),
             radioButtons(
               inputId = "stackfillbop",
               label = h4("Bar type"),
               choices = c("dodge", 'stack', "fill"),
               selected = "stack"
             )
             ),
             mainPanel(plotlyOutput("successbar1"), br(), br(), br(), plotlyOutput("successbar2")) 
             ))

           
)