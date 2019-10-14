library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(stringr)
library(shiny)



descriptions = read.csv('./descriptions_clean.csv', stringsAsFactors = F)
cab = read.csv('./cab_clean.csv', stringsAsFactors = F)

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
  select(Country.Name, Approval.Date, Approval.Year, Inipgmyr, arrTypeGroup, Totalaccess, Arrangement.Type, Initial.End.Year) %>% 
  filter(arrTypeGroup == "General Resources Account (GRA)")


arrbycountry = descriptions %>% 
  select(Country.Name, Approval.Date, Approval.Year, arrTypeGroup, Totalaccess, Arrangement.Type)

cabimfcountries = inner_join(grabycountry, cab, by = "Country.Name")
cabimfcountries = unique(cabimfcountries%>% select(Country.Name))

arrbycountry2 = descriptions %>% 
  select(Country.Name, Inipgmyr, arrTypeGroup, Totalaccess)
head(arrbycountry2)

