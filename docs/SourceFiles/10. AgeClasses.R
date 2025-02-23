
## Confirm data is loaded 
ifelse(exists('data_all') && is.data.frame(get('data_all')), 
       " ", 
       source("SourceFiles/1. Load and Prep Data.R"))

dev.new() 

## 1. Prep Data ----
simms.dat <- data_all %>%
                filter(Species %in% c("CT", "CO")) %>%
                mutate(age.class = ifelse(Length > 130, "adult","juvenile")) %>%
                select(Year, Period, Date, date.std, Species, Length, Weight) %>%
                mutate(AgeClass = case_when(Species == "CO" & Period == "Spring" & Length <= 75 ~ "Age-0",
                                            Species == "CO" & Period == "Spring" & Length > 75 ~ "Age-1",
                                            Species == "CT" & Period == "Spring" & Length <= 75 ~ "Age-0",
                                            Species == "CT" & Period == "Spring" & Length < 120 ~ "Age-1",
                                            Species == "CT" & Period == "Spring" & Length < 200 ~ "Age-2",
                                            Species == "CT" & Period == "Spring" & Length >= 200 ~ "Age-2+",
                                            Species == "CO" & Period == "Fall" & Length >=500 ~ "Adult",
                                            Species == "CO" & Period == "Fall" & Length <500 ~ "Jack",
                                            Species == "CT" & Period == "Fall" ~ "Adult"),
                       ## Year that fish emerged  
                       spawn.year = case_when(AgeClass == "Age-0"  ~ Year - 1, 
                                              AgeClass == "Age-1"  ~ Year - 2,
                                              AgeClass == "Age-1+" ~ Year - 3,
                                              AgeClass == "Age-2+" ~ Year - 4,
                                              AgeClass == "Adult"  ~  Year),
                       ## Year that juvenile CO return to Simms Creek as Adults
                       est.return.year = case_when(AgeClass == "Age-0" ~ Year + 1,
                                                   AgeClass == "Age-1" ~ Year + 2,
                                                   AgeClass == "Jack" ~ Year)) 
CT.AgeClass.Plot <- simms.dat %>%
                    group_by(Year,Period,Species, AgeClass, Length) %>%
                    summarize(n = n()) %>%
                    filter(Period == "Spring",
                           Species == "CT") %>%
                    
                    ggplot(., aes(x = AgeClass, y = n)) +
                      geom_col() +
                      facet_grid(Year~.)

CO.AgeClass.Plot <- simms.dat %>% 
                    group_by(Year,Period,Species, AgeClass, Length) %>%
                    summarize(n = n()) %>%
                    filter(Period == "Spring",
                           Species == "CO",
                           !is.na(AgeClass)) %>%
                    
                    
                    ggplot(., aes(x = AgeClass, y = n)) +
                    geom_col() +
                    facet_grid(Year~.)

juv.AgeClass.plot <- simms.dat %>% 
                      group_by(Year,Period,Species, AgeClass, Length) %>%
                      summarize(n = n()) %>%
                      filter(Period == "Spring",
                             !is.na(AgeClass)) %>%
                      mutate(Species = case_when(Species == "CT" ~ "Cutthroat Trout",
                                                 Species == "CO" ~ "Coho Salmon")) %>%
                      
                      
                      ggplot(., aes(x = AgeClass, y = n)) +
                      geom_col() +
                      facet_grid(Year~Species) +
                      labs(x = "", y = "# of Fish") +
                      theme_bw()

juv.AgeClass.plot