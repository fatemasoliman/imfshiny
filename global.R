library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(stringr)
library(shiny)



descriptions = read.csv('./descriptions_clean.csv', stringsAsFactors = F)

descriptions = descriptions %>% mutate(Region.Code = case_when(Region.Code == 2 ~ '002',
                               Region.Code== 19 ~ '019', 
                               Region.Code == 9~ '009',
                               Region.Code == 13~ '013',
                               Region.Code == 3~ '003',
                               Region.Code == 005~ '005',
                               TRUE ~ as.character(Region.Code))) 


arrangementsbytype = descriptions %>%
  group_by(Approval.Year, arrTypeGroup) %>% summarise(tot = n(), totalaccessamount = sum(Totalaccess)) 


totarrangements = arrangementsbytype %>%
  group_by(Approval.Year, arrTypeGroup) %>% summarise(tot = sum(tot),
                                                      totalaccessamount = sum(totalaccessamount))
grabycountry = descriptions %>% 
  select(Country.Name, Approval.Date, Approval.Year, arrTypeGroup, Totalaccess, Arrangement.Type) %>% 
  filter(arrTypeGroup == "General Resources Account (GRA)")



