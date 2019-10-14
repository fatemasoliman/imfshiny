library(dplyr)
library(lubridate)
library(ggplot2)


meconraw = read.csv('./Mecon.csv', stringsAsFactors = F) 
descriptionraw = read.csv('./description.csv', stringsAsFactors = F)

head(descriptionraw)

descriptions = descriptionraw %>%
  select(Arrangement.Number, Country.Name, Country.Code, Arrangement.Type, 
         Approval.Date, Approval.Year, Initial.End.date, Initial.End.Year, Board.Action.Date,
         Program.Type, Inipgmyr, Totalaccess, Review.Type) %>% distinct() 


arrangements = descriptions %>% 
  filter(Review.Type == 'R0') %>% 
  group_by(Inipgmyr, Arrangement.Type) %>% 
  summarise(tot = n(), totalaccessamount = sum(Totalaccess))


head(arrangements)
 
ggplot(data = arrangements) + 
  geom_bar(aes(x = Inipgmyr, y = tot), 
           stat = "identity", position = 'dodge')

ggplot(data = arrangements) + 
  geom_bar(aes(x = Inipgmyr, y = totalaccessamount), 
           stat = "identity", position = 'dodge')




head(arrangements)




countries = unique(descriptions$Country.Name)

countries2 = gsub(",.*",'', countries)






descriptionraw = read.csv('./description.csv', stringsAsFactors = F)


descriptions = descriptionraw %>% select(
  Arrangement.Number,
  Country.Name,
  Country.Code,
  Arrangement.Type,
  Approval.Date,
  Approval.Year,
  Initial.End.date,
  Initial.End.Year,
  Board.Action.Date,
  Program.Type,
  Inipgmyr,
  Totalaccess,
  Review.Type
) %>%   filter(Review.Type == 'R0') %>%
  mutate(
    arrTypeGroup = case_when(
      Arrangement.Type %in%
        c("EFF", "PCL", "SBA", "SBA-ESF", "SBA-SCF", "ESF", "PLL", 'FCL') ~ 'General Resources Account (GRA)',
      Arrangement.Type %in% c("ECF", "ECF-EFF", "PRGF", "PRGF-EFF", "SCF") ~
        'Poverty Reduction and Growth Trust (PRGT)',
      Arrangement.Type == 'PSI' ~ "Policy Support Instrument (PSI)",
      Arrangement.Type == 'PCI' ~ "Policy Coordination Instrument (PCI)"
    )
  ) %>%
  mutate(Country.Name = gsub(",.*", '', Country.Name), Country.Name = str_to_title(Country.Name))


descriptions = left_join(descriptions, regionlookup, by="Country.Name")

regionlookup = read.csv('./UNSD — Methodology.txt', stringsAsFactors = FALSE)

regionlookup = regionlookup %>% select(Region.Code, Region.Name, Country.or.Area) %>% 
  mutate(Region.Code = case_when(Region.Code == 2 ~ '002',
                                 Region.Code== 19 ~ '019', 
                                 Region.Code == 9~ '009',
                                 TRUE ~ as.character(Region.Code))) %>% 
  rename('Country.Name' = Country.or.Area)


nas = descriptions %>% filter(is.na(Region.Code)) %>% select(Country.Name)
 

list(unique(nas$Country.Name))


descriptions %>% group_by(Country.Name) %>%  summarise(nloans = n())
