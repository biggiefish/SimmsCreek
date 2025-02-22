
## Confirm data is loaded 
ifelse(exists('data_all') && is.data.frame(get('data_all')), 
       " ", 
       source("SourceFiles/1. Load and Prep Data.R"))

## 1. Prep Data ----
simms.dat <- data_all %>%
             filter(Species %in% c("CT", "CO")) %>%
             mutate(age.class = ifelse(Length > 130, "adult","juvenile")) %>%
             select(Year, Period, Date, date.std, Species, Length, Weight) %>%
             mutate(AgeClass = case_when(Species == "CO" & Period == "Spring" & Length <= 75 ~ "Age-0",
                                         Species == "CO" & Period == "Spring" & Length > 75 ~ "Age-1",
                                         Species == "CT" & Period == "Spring" & Length <= 85 ~ "Age-0",
                                         Species == "CT" & Period == "Spring" & Length < 250 ~ "Age-1+",
                                         Species == "CT" & Period == "Spring" & Length >= 250 ~ "Age-2+",
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

simms.dat %>% group_by(Species,AgeClass) %>% summarize(n = n(),
                                                       min = min(Length),
                                                       max = max(Length),
                                                       mean = mean(Length))

regional.dat <- data_regional %>% 
                filter(species %in% c("Coho")) %>%
                mutate(spawn.year = year) %>%
                select(spawn.year, spawners) 
                
## Spring Fish data - Simms + Regional
spring.fish.dat <- simms.dat %>%
                      filter(Period == "Spring") %>%
                      group_by(spawn.year, Species) %>%
                      # group_by(Year, Species) %>%
                      summarize(juveniles = n()) %>%
                      select(spawn.year,
                             # out.year = Year,
                             species = Species,
                             juveniles) %>%
                      left_join(regional.dat, by = c("spawn.year"))
                      # left_join(regional.dat, by = c("out.year"))

## Fall Fish Data - Simms + Regional
fall.fish.dat <- simms.dat %>%
                  filter(Period == "Fall") %>%
                  group_by(spawn.year, Species) %>%
                  # group_by(Year, Species) %>%
                  summarize(adults = n()) %>%
                  select(spawn.year,
                         # out.year = Year,
                         species = Species,
                         adults) %>%
                  left_join(regional.dat, by = c("spawn.year"))
                  # left_join(regional.dat, by = c("out.year"))

## 2. Regional vs Local ----
## * 2.1 Regional Adult vs Juveniles Counts----
## * * 2.1.1 Time Series ----
      ## * * * Regional CO Spawners vs Juvenile CT in Simms Creek over time ----
      spring.fish.dat %>% 
                   filter(species == "CT") %>%
                   mutate(spawners = spawners/1000) %>%
                   pivot_longer(!c(spawn.year, species), names_to = "age.class", values_to = "n.fish") %>%
                   mutate(age.class = case_when(age.class == "juveniles" ~ "Juvenile CT (Simms Ck.)",
                                               age.class == "spawners" ~ "Spawning CO (Regionally)")) %>%
                   as.data.frame(.) %>%
                   
                   ggplot(.,aes(x = spawn.year, y = n.fish, color = age.class))+
                   geom_point() +
                   geom_smooth() +
                   scale_x_continuous(limits= c(2008, 2024),
                                      breaks = seq(2008,2024,2))+    
                   labs(x = "Year", y = "# Spawning CO (Regionally) (1,000's of Fish)", color = "Dataset") +
                   theme_bw() +
                   theme(legend.position = "bottom")

      ## * * * Regional CO Spawners vs Juvenile CO in Simms Creek over time ----
      spring.fish.dat %>% 
                  filter(species == "CO") %>%
                  mutate(spawners = spawners/1000) %>%
                  pivot_longer(!c(spawn.year, species), names_to = "age.class", values_to = "n.fish") %>%
                  mutate(age.class = case_when(age.class == "juveniles" ~ "Juvenile CO (Simms Ck.)",
                                               age.class == "spawners" ~ "Spawning CO (Regionally)")) %>%
                  as.data.frame(.) %>%
                  
                  ggplot(.,aes(x = spawn.year, y = n.fish, color = age.class))+
                  geom_point() +
                  geom_smooth() +
                  scale_x_continuous(limits= c(2008, 2024),
                                     breaks = seq(2008,2024,2))+
                  labs(x = "Year", y = "# Spawning CO (Regionally) (1,000's of Fish)", color = "Dataset") +
                  theme_bw() +
                  theme(legend.position = "bottom")


                   
## * * * Adult CO vs Adult CO ----                   
fall.fish.dat %>% filter(species == "CO") %>%
                  select(species, 
                         "Simms Creek" = adults,
                         "Regional" = spawners) %>%
                  mutate(Regional = Regional/1000) %>%
                  pivot_longer(!c(spawn.year, species), names_to = "age.class", values_to = "n.fish") %>%
                  mutate(age.class = case_when(age.class == "Simms Creek" ~ "Spawning CO (Simms Ck.)",
                                               age.class == "Regional" ~ "Spawning CO (Regionally)")) %>%
                 
                  ggplot(.,aes(x = spawn.year, y = n.fish, colour = age.class))+
                  geom_point() +
                  geom_smooth()+
                  scale_x_continuous(limits= c(2008, 2024),
                                     breaks = seq(2008,2024,2))+
                  labs(x = "",
                       y = "# Spawning CO (Regionally) (1,000's of Fish)",
                       title = "Relative number of juvenile Coho Salmon and Cutthroat Trout emigrants",
                       color = "Dataset")  +
                  theme_bw()+
                  theme(legend.position = "bottom")
                 
## * * 2.1.2 Relative Abundance ----
## * * *  Juv. CT vs adult CO ----              
spring.fish.dat %>%
                   filter(species == "CT") %>%   
                   ggplot(.,aes(x = spawners/1000, y = juveniles))+
                   geom_point()+
                   geom_smooth(method = 'lm', formula = y~x)+
                   labs(x = "# Spawning CO (Regionally) (1,000's of Fish)", 
                        y = "# Juvenile CCT (Simms Ck.)",
                        title = "Regional Coho Abundance relative to Juvenile CCT Abundance in Simms Creek") +
                   theme_bw()+
                   theme(legend.position = "bottom")
                   
## * * * Juv CO vs adult CO ----
spring.fish.dat %>%
                   filter(species == "CO") %>%   
                   ggplot(., aes(x = spawners/1000, y = juveniles))+
                   geom_point()+
                   geom_smooth(method = 'lm', formula = y~x) +
                   labs(x = "# Spawning CO (Regionally) (1,000's of Fish)", 
                        y = "# Juvenile CO (Simms Ck.)",
                        title = "Regional Coho Abundance relative to Juvenile Coho Abundance in Simms Creek") +
                   theme_bw()

spring.fish.dat %>% ggplot(., aes(x = spawners/1000, y = juveniles, color = species, group = species)) +
                    geom_point() +
                    geom_smooth(method = "lm", formula = y~x, alpha = 0.2) +
                    # geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,
                    #             method.args = list(start = list(a = 0.1, b = 0.1))) +
                    labs(x = "# Spawning CO (Regionally) (1,000's of Fish)", 
                         y = "# Juveniles (Simms Ck.)",
                         title = "Regional Coho Abundance relative to Juvenile Coho Abundance in Simms Creek") +
                    theme_bw() +
                    theme(legend.position = "bottom")
                    
                    

# ||--------------------------------------|| ----
# 3 Local (Simms Creek) Data----

## * 3.1 Time Series ----
## * * * * Juvenile CT and CO over time ----
#     - Initial read suggests low CT during years with high CO. 
#     - Adding geom_smooth(span = 0.3) shows that there is not a relationship. 
#     - CO abundance was higher from 2013 to 2017, but was comparable from 2008 to 2012 and from 2018 onwards.
spring.fish.dat %>% ggplot(., aes(x = spawn.year, 
                                  y = juveniles, 
                                  color = species)) +
                      geom_point()+
                      # geom_smooth() +
                      geom_smooth(spawn = 0.5) +
                      scale_x_continuous(limits= c(2008, 2024),
                                         breaks = seq(2008,2024,2))+
                      labs(x = "",
                           y = "# of Juveniles (Simms Ck.)",
                           title = "Number of juvenile CO and CT smolts per year",
                           color = "Species")

## * * * * Adult CO and juvenile CO over time ----
#     - Change in juvenile abundance is not driven by the number of spawners. 
simms.CO <- rbind(fall.fish.dat %>%   select(-spawners, n.fish = adults) %>% 
                                     filter(species =="CO") %>% 
                                     mutate(age.class = "adult"),
                 spring.fish.dat %>% select(-spawners, n.fish = juveniles) %>% 
                                     filter(species =="CO") %>% 
                                     mutate(age.class = "juvenile",
                                            n.fish    = n.fish/10))
  
      ggplot(simms.CO, 
             aes(x = spawn.year, y = n.fish, color = age.class))+
      geom_point() +
      geom_smooth() + 
      scale_x_continuous(limits= c(2008, 2024),
                         breaks = seq(2008,2024,2))+
      scale_y_continuous(breaks = seq(-60,300,20))+
      geom_hline(yintercept = 0) +
      labs(x = "",
           y = "# of Fish (10's of juveniles)",
           title = "Juvenile CO relative to adult CO spawners in Simms Creek",
           color = "Age Class") +
      theme_bw()
      
## * * * * Adult CO and juvenile CT over time ----
#     - Change in juvenile CT abundance may be driven by the number of CO spawners from 2008 to 2019.
#     - BUT data post 2019 suggests other factors are at play.       
      
simms.COCT <- rbind(simms.CO %>% filter(age.class != "juvenile"),
                   spring.fish.dat %>% select(-spawners, n.fish = juveniles) %>% 
                                      filter(species =="CT") %>% 
                                      mutate(age.class = "juvenile",
                                             n.fish    = n.fish/10)) %>%
                  mutate(age.class.spp = paste0(species,"_",age.class))
      
      ggplot(simms.COCT, 
             aes(x = spawn.year, y = n.fish, color = age.class.spp))+
        geom_point() +
        geom_smooth() + 
        scale_x_continuous(limits= c(2008, 2024),
                           breaks = seq(2008,2024,2))+
        scale_y_continuous(breaks = seq(-40,300,20))+
        geom_hline(yintercept = 0) +
        labs(x = "",
             y = "# of Fish (10's of juveniles)",
             title = "Juvenile CO relative to adult CO spawners in Simms Creek",
             color = "Age Class") +
        theme_bw()      


## * 3.2 Relative Abundance ----
## * * * * Juvenile CO vs Juvenile CT ----
spring.fish.dat %>% select(species, juveniles) %>%
                    pivot_wider(names_from = species, values_from = juveniles) %>%
            
            ggplot(.,aes(x = CT, y = CO))+
            geom_point() +
            geom_smooth(method = 'lm', formula = y~x)+
            labs(x = "# of Juvenile Cutthroat (Simms Ck.)",
                 y = "# of Juvenile Coho  (Simms Ck.)") +
            theme_bw()

## * * * * Juvenile CO vs Adult CO ----
simms.CO %>% pivot_wider(names_from = age.class, values_from = n.fish) %>% 
             mutate(juvenile = juvenile *10) %>%
             
             ggplot(., aes(x = adult, y = juvenile)) +
             geom_point() +
             geom_smooth(method = 'lm', formula = y~x)+
             labs(x = "# of Adult Coho (Simms Ck.)",
                  y = "# of Juvenile Coho (Simms Ck.)") +
             scale_x_continuous(limits = c(0, 130),
                                breaks = seq(0, 130,10))+
             scale_y_continuous(limits = c(200, 1000),
                                breaks = seq(200, 1000,100))+
             theme_bw()
      
## * * * * Juvenile CT vs Adult CO ----
simms.COCT %>% select(spawn.year, n.fish, age.class.spp) %>%
               pivot_wider(names_from = age.class.spp, 
                           values_from = n.fish) %>% 
               mutate(CT_juvenile = 10*CT_juvenile) %>%
             
             ggplot(., aes(x = CO_adult, y = CT_juvenile)) +
             geom_point() +
             geom_smooth(method = 'lm', formula = y~x)+
             labs(x = "# of Adult Coho (Simms Ck.)",
                  y = "# of Juvenile Cutthroat (Simms Ck.)") +
             scale_x_continuous(limits = c(0, 130),
                                breaks = seq(0, 130,10))+
             scale_y_continuous(limits = c(0, 800),
                                breaks = seq(0, 800,100))+
             theme_bw()

##||-----------------------------------------||----
## 4. Juvenile Survival ----
## Calculate number of CO that smolted from each spawning event
juv.surv <- simms.dat %>% filter(Species =="CO") %>%
                       group_by(spawn.year, AgeClass) %>%
                       summarize(n = n()) %>%
                       pivot_wider(names_from = AgeClass, values_from = n) %>%
                       select(!`NA`)%>%
                       mutate(across(where(is.numeric), ~replace(.,is.na(.),0)),
                              Adult         = Adult - Jack,
                              Juv.Total     = sum(`Age-0`,`Age-1`,na.rm = TRUE),
                              Fecundity     = 2600,
                              Eggs          = (0.5*Adult) * Fecundity,
                              Egg_to_Smolt  = Juv.Total/Eggs,
                              Species = "CO") %>%
                      # Remove years with incomplete data. 
                       filter(spawn.year >=2008,
                              !spawn.year %in% c(2018, 2019, 2020, 2023)) %>%
                       ungroup() %>%
                      # Lag juveniles by 1 year to account for abundance upon emergence (i.e., 2-3 months when YOY and past years fish are present) 
                       mutate(Juv.Total.Lagged = lag(Juv.Total)) %>%
                       select(Species, spawn.year, "Adult","Jack","Fecundity","Age-0","Age-1","Juv.Total","Juv.Total.Lagged","Eggs","Egg_to_Smolt")

## Prepare Plots

      ## Egg-to-Fry Survival Relative to Juvenile Abundance
      ggplot(juv.surv, 
             aes(x = Juv.Total, y = 100*Egg_to_Smolt)) +
        geom_point() +
        geom_smooth(method = 'lm', formula = y~x) +
        labs(y = "Egg-to-Smolt Survival (%)",
             x = "# Juvenile Coho Salmon (Simms Ck.)",
             title = "Relationship between Egg-to-Smolt survival and number of Juvenile Coho Salmon produced each year.") +
        theme_bw()
      
      ## Egg-to-Fry Survival Relative to Juvenile Abundance
      ggplot(juv.surv, 
             aes(x = Juv.Total, y = Juv.Total.Lagged)) +
        geom_point() +
        geom_smooth(method = 'lm', formula = y~x) +
        labs(y = "# Juvenile Coho Salmon (in previous year)",
             x = "# Juvenile Coho Salmon (in current year)",
             title = "Relationship between Egg-to-Smolt survival and number of Juvenile Coho Salmon produced each year.") +
        theme_bw()
      

## 5. Calculate Adult Survival ----
Adult.Surv <- left_join(simms.dat %>% filter(Species == "CO") %>%
                                      group_by(est.return.year, AgeClass) %>%
                                      summarize(n = n()) %>%
                                      pivot_wider(names_from = "AgeClass", values_from = "n") %>%
                                      mutate(across(where(is.numeric), ~replace(.,is.na(.),0))) %>%
                                      select(!`NA`) %>%
                                      group_by(est.return.year) %>%
                                      mutate(Total.Est.Adults = sum(`Jack` + `Age-0` + `Age-1`)),
                        simms.dat %>% filter(Species == "CO",
                                      AgeClass %in% c("Adult", "Jack")) %>%
                                      group_by(spawn.year, AgeClass) %>%
                                      summarize(n = n()) %>%
                                      pivot_wider(names_from = "AgeClass", values_from = "n") %>%
                                      mutate(across(where(is.numeric), ~replace(.,is.na(.),0)),
                                             Total.Adult = sum(Adult+Jack)),
                      by = c("est.return.year"= "spawn.year"),
                      suffix = c(".est", ".actual")) %>%
              mutate(Adult.Surv = Total.Adult/Total.Est.Adults) %>%
              filter(Adult.Surv<1)
      
      
      ggplot(Adult.Surv, 
             aes(x = Adult.actual, y = Total.Est.Adults)) +
        geom_point() +
        geom_smooth(method = 'lm', formula = y~x) +
        labs(y = "Estimated Max. Possible Returning Adults (Juveniles + Jacks)",
             x = "# Returning Adult Coho Salmon (Enumerated at Fence)",
             title = "") +
        theme_bw()                 
      
      
## 5. Plot showing local and regional abundance trends 
dat <- rbind(simms.dat %>% mutate(AgeClass = ifelse(AgeClass == "Adult", "Adult","Juvenile")) %>%
                           group_by(Year,Period,Species, AgeClass) %>%
                           summarize(n = n()) %>%
                           mutate(Dataset = "Local"),
             regional.dat %>% mutate(Year = spawn.year,
                                     Period = "Fall",
                                     Species = "CO",
                                     AgeClass = "Adult",
                                     n        = spawners/1000,
                                     Dataset  = "Regional (1,000s of fish") %>%
                              select(Year, Period, Species, AgeClass,n, Dataset)) %>%
              filter(!(Species == "CT" & AgeClass == "Adult"),
                     !is.na(AgeClass),
                     Year >= 2008) %>%
              mutate(Species = paste0(Species," (",Dataset,")"))


ggplot(dat, aes(x = Year, y = n, color = Species)) +
    geom_point() +
    geom_smooth(aes(color = Species, linetype = AgeClass)) + 
    labs(x = "",
         y = "# of Fish") +
    theme_bw() +
    theme(legend.position = "bottom")
