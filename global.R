library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(stringr)
library(shiny)


#READ IN (CLEAN) IMF DATA AND WORLD BANK BOP DATA AS CSV 
descriptions = read.csv('./descriptions_clean.csv', stringsAsFactors = F)
cab = read.csv('./cab_clean.csv', stringsAsFactors = F)

#READING AS CSV REMOVES NECESSARY LEADING ZEROS - PUT THEM BACK
descriptions = descriptions %>% mutate(Region.Code = case_when(Region.Code == 2 ~ '002',
                               Region.Code== 19 ~ '019', 
                               Region.Code == 9~ '009',
                               Region.Code == 13~ '013',
                               Region.Code == 3~ '003',
                               Region.Code == 005~ '005',
 
                                                             TRUE ~ as.character(Region.Code))) 

#DEFINE DATAFRAMES THAT WILL BE CALLED IN SERVER 

#IMF DATA
arrangementsbytype = descriptions %>%
  group_by(Approval.Year, arrTypeGroup) %>% summarise(tot = n(), totalaccessamount = sum(Totalaccess)) 


totarrangements = arrangementsbytype %>%
  group_by(Approval.Year, arrTypeGroup) %>% summarise(tot = sum(tot),
                                                      totalaccessamount = sum(totalaccessamount))
grabycountry = descriptions %>% 
  select(Country.Name, Approval.Date, Approval.Year, Inipgmyr, arrTypeGroup, Totalaccess, 
         Arrangement.Type, Initial.End.Year, Region.Code, Region.Name) %>% 
  filter(arrTypeGroup == "General Resources Account (GRA)" |
           arrTypeGroup == "Poverty Reduction and Growth Trust (PRGT)" )


arrbycountry = descriptions %>% 
  select(Country.Name, Approval.Date, Approval.Year, arrTypeGroup, Totalaccess, Arrangement.Type)

arrbycountry2 = descriptions %>% 
  select(Country.Name, Inipgmyr, arrTypeGroup, Totalaccess)

#BOP DATA

cabimfcountries = inner_join(grabycountry, cab, by = "Country.Name")
cabimfcountries = unique(cabimfcountries%>% select(Country.Name))



#IMF DATA BY REGION
byregion = descriptions %>% 
  group_by(Region.Name, Region.Code, Country.Name, arrTypeGroup, Arrangement.Type, Totalaccess, Approval.Year) %>% 
  summarise(n = n())




#BOP DATA WITH YOY CALCULATIONS
cabyoy = cab %>% arrange(Country.Name, year) %>% 
  mutate(yoy1 = -(bop - lag(bop))/lag(bop), 
         yoy2 = -(bop - lag(bop,2))/lag(bop,2),
         yoy3 = -(bop - lag(bop,3))/lag(bop,3)) %>% 
  rename("Approval.Year" = year)

cabgra = inner_join(grabycountry, cabyoy, by = c("Country.Name", "Approval.Year"))

success = cabgra %>% group_by(Region.Code, Region.Name, Country.Name, Approval.Year, 
                              arrTypeGroup, Arrangement.Type, Totalaccess, yoy1) %>% 
  mutate(yoy1B = if_else(yoy1>0, 1, 0), 
         yoy2B = if_else(yoy2>0, 1, 0), 
         yoy3B = if_else(yoy3>0, 1, 0)) %>% 
  summarise(n1 = sum(yoy1B), n2 = sum(yoy2B), n3 = sum(yoy3B)) 

success = success %>% mutate(none = if_else(n1 == 0 & n2 ==0 & n3==0, 1, 0))

successtot = success  %>% 
  ungroup() %>% 
  summarise("Year 1" = sum(n1), "Year 2"= sum(n2), 
            "Year 3"= sum(n3), "No Improvement" = sum(none)) %>% melt() %>% 
  rename("yearimpr" = variable, "n" = value) 

