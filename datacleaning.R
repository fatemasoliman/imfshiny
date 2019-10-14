descriptionraw = read.csv('./description.csv', stringsAsFactors = F)

regionlookup = read.csv('./UNSD — Methodology.csv', stringsAsFactors = FALSE)




regionlookup = regionlookup %>% select(Region.Code, Region.Name, Country.or.Area) %>% 
  mutate(Region.Code = case_when(Region.Code == 2 ~ '002',
                                 Region.Code== 19 ~ '019', 
                                 Region.Code == 9~ '009',
                                 Region.Code == 13~ '013',
                                 Region.Code == 3~ '003',
                                 Region.Code == 005~ '005',
                                 TRUE ~ as.character(Region.Code))) %>% 
  rename('Country.Name' = Country.or.Area) %>% mutate(Country.Name = toupper(Country.Name))



descriptions = descriptionraw %>%
  select(Arrangement.Number,Country.Name,Country.Code, Arrangement.Type,Approval.Date, 
         Approval.Year,  Initial.End.date,Initial.End.Year, Board.Action.Date,Program.Type,
         Inipgmyr, Totalaccess, Review.Type) %>%
  mutate(
    arrTypeGroup = case_when(
      Arrangement.Type %in%
        c("EFF", "PCL", "SBA", "SBA-ESF", "SBA-SCF", "ESF", "PLL", 'FCL') ~
        'General Resources Account (GRA)',
      Arrangement.Type %in%
        c("ECF", "ECF-EFF", "PRGF", "PRGF-EFF", "SCF") ~
        'Poverty Reduction and Growth Trust (PRGT)',
      Arrangement.Type == 'PSI' ~ "Policy Support Instrument (PSI)",
      Arrangement.Type == 'PCI' ~ "Policy Coordination Instrument (PCI)")) %>% 
  filter(Review.Type == 'R0') %>% 
  mutate(Country.Name = gsub(",.*",'', Country.Name), 
         Country.Name = trimws(str_to_title(Country.Name)), 
         Country.Name = toupper(Country.Name))

descriptions = left_join(descriptions, regionlookup, by="Country.Name")

write.csv(descriptions, "./descriptions_clean.csv")



##############BALANCE OF PAYMENTS DATA FROM WORLD BANK#####################
cab = read.csv('./cab.csv', stringsAsFactors = FALSE)
cab = cab %>% select(Country.Name, Country.Code, contains('X'))
colnames(cab) = gsub(pattern = 'X', replacement = "", colnames(cab))
cab = melt(cab) %>% rename('year' = variable, 'bop' = value) 
cab = cab %>%  filter(!is.na(bop))
cab$year = as.numeric(as.character(cab$year))
cab = cab %>%  
  mutate(Country.Name = gsub(",.*",'', Country.Name), 
         Country.Name = gsub("Cabo Verde", "CAPE VERDE", Country.Name),
         Country.Name = toupper(Country.Name)) %>% 
  filter(year > 1999)


write.csv(cab, "./cab_clean.csv")

#########


cab %>% 
  filter(Country.Name %in% c("Algeria", "Egypt")) %>%  
  ggplot(aes(x = year, y = bop, group = Country.Name, color = Country.Name)) + 
  geom_line()



