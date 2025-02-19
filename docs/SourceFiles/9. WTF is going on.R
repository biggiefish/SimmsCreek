
## Confirm data is loaded 
ifelse(exists('data_all') && is.data.frame(get('data_all')), 
       " ", 
       source("SourceFiles/1. Load and Prep Data.R"))

## 1. Prep Data ----
simms.dat <- data_all %>%
             filter(Species %in% c("CT", "CO")) %>%
             mutate(age.class = ifelse(Length > 130, "adult","juvenile")) %>%
             select(Year, Period, Date, date.std, Species, Length, Weight)

regional.dat <- data_regional %>% 
                filter(species %in% c("Coho")) %>%
                mutate(out.year = year+1) %>%
                select(out.year, spawners) 
                
## Spring Fish data - Simms + Regional
spring.fish.dat <- simms.dat %>%
            filter(Period == "Spring") %>%
            group_by(Year, Species) %>%
            summarize(juveniles = n()) %>%
            select(out.year = Year,
                   species = Species,
                   juveniles) %>%
            left_join(regional.dat, by = c("out.year"))

## Fall Fish Data - Simms + Regional
fall.fish.dat <- simms.dat %>%
                  filter(Period == "Fall") %>%
                  group_by(Year, Species) %>%
                  summarize(adults = n()) %>%
                  select(out.year = Year,
                         species = Species,
                         adults) %>%
                  left_join(regional.dat, by = c("out.year"))

## 2. Regional vs Local ----
## * 2.1 Regional Adult vs Juveniles Counts----
## * * 2.1.1 Time Series ----
      ## * * * Adult CO vs Juv. CT over time ----
      spring.fish.dat %>% 
                   filter(species == "CT") %>%
                   mutate(spawners = spawners/1000) %>%
                   pivot_longer(!c(out.year, species), names_to = "age.class", values_to = "n.fish") %>%
                   as.data.frame(.) %>%
                   
                   ggplot(.,aes(x = out.year, y = n.fish, color = age.class))+
                   geom_point() +
                   geom_smooth() +
                   scale_x_continuous(limits= c(2008, 2024),
                                      breaks = seq(2008,2024,2))+    
                   labs(x = "Year", y = "# Fish (1,000's of adults)", color = "Age Class") +
                   theme_bw()

      ## * * * Adult CO vs Juv. CO over time ----
      spring.fish.dat %>% 
                  filter(species == "CO") %>%
                  mutate(spawners = spawners/1000) %>%
                  pivot_longer(!c(out.year, species), names_to = "age.class", values_to = "n.fish") %>%
                  as.data.frame(.) %>%
                  
                  ggplot(.,aes(x = out.year, y = n.fish, color = age.class))+
                  geom_point() +
                  geom_smooth() +
                  scale_x_continuous(limits= c(2008, 2024),
                                     breaks = seq(2008,2024,2))+
                  labs(x = "Year", y = "# Fish (1,000's of adults)", color = "Age Class") +
                  theme_bw()
                   
## * * * Adult CO vs Adult CO ----                   
fall.fish.dat %>% filter(species == "CO") %>%
                  select(species, 
                         "Simms Creek" = adults,
                         "Regional" = spawners) %>%
                  mutate(Regional = Regional/1000) %>%
                  pivot_longer(!c(out.year, species), names_to = "age.class", values_to = "n.fish") %>%
                 
                  ggplot(.,aes(x = out.year, y = n.fish, colour = age.class))+
                  geom_point() +
                  geom_smooth()+
                  scale_x_continuous(limits= c(2008, 2024),
                                     breaks = seq(2008,2024,2))+
                  labs(x = "",
                       y = "# of CO Spawners (1,000's for Regional)",
                       title = "Relative number of juvenile Coho Salmon and Cutthroat Trout emigrants",
                       color = "Dataset")  +
                  theme_bw()
                 
## * * 2.1.2 Relative Abundance ----
## * * *  Juv. CT vs adult CO ----              
spring.fish.dat %>%
                   filter(species == "CT") %>%   
                   ggplot(.,aes(x = spawners/1000, y = juveniles))+
                   geom_point()+
                   geom_smooth(method = 'lm', formula = y~x)+
                   labs(x = "# of Adult CO Spawners (1,000's fish)", 
                        y = "# of Juvenile CCT",
                        title = "Adult CO relative to juvenile CCT") +
                   theme_bw()
                   
## * * * Juv CO vs adult CO ----
spring.fish.dat %>%
                   filter(species == "CO") %>%   
                   ggplot(., aes(x = spawners/1000, y = juveniles))+
                   geom_point()+
                   geom_smooth(method = 'lm', formula = y~x) +
                   labs(x = "# of Adult CO Spawners (1,000's fish)", 
                        y = "# of Juvenile CO",
                        title = "Adult CO relative to juvenile CO") +
                   theme_bw()

# ||--------------------------------------|| ----
# 3 Local (Simms Creek) Data----

## * 3.1 Time Series ----
## * * * * Juvenile CT and CO over time ----
#     - Initial read suggests low CT during years with high CO. 
#     - Adding geom_smooth(span = 0.3) shows that there is not a relationship. 
#     - CO abundance was higher from 2013 to 2017, but was comparable from 2008 to 2012 and from 2018 onwards.
  spring.fish.dat %>% ggplot(., aes(x = out.year, 
                             y = juveniles, 
                             color = species)) +
                      geom_point()+
                      geom_smooth() +
                      # geom_smooth(spawn = 0.3) +
                      scale_x_continuous(limits= c(2008, 2024),
                                         breaks = seq(2008,2024,2))+
                      labs(x = "",
                           y = "# of Fish",
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
             aes(x = out.year, y = n.fish, color = age.class))+
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
      
simms.COCT <- rbind(simms.COCT %>% filter(age.class != "juvenile"),
                  spring.fish.dat %>% select(-spawners, n.fish = juveniles) %>% 
                                      filter(species =="CT") %>% 
                                      mutate(age.class = "juvenile",
                                             n.fish    = n.fish/10)) %>%
                  mutate(age.class.spp = paste0(species,"_",age.class))
      
      ggplot(simms.COCT, 
             aes(x = out.year, y = n.fish, color = age.class.spp))+
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
            labs(x = "# of Juvenile Cutthroat",
                 y = "# of Juvenile Coho",
                 title = "Relative number of juvenile Coho Salmon and Cutthroat Trout emigrants")

## * * * * Juvenile CO vs Adult CO ----
simms.CO %>% pivot_wider(names_from = age.class, 
                         values_from = n.fish) %>% 
             mutate(juvenile = juvenile *10) %>%
             
             ggplot(., aes(x = adult, y = juvenile)) +
             geom_point() +
             geom_smooth(method = 'lm', formula = y~x)+
             labs(x = "# of Adult Coho",
                  y = "# of Juvenile Coho",
                  title = "Relationship between Adult spawning Coho Salmon and outmigrating \njuvenile Coho Salmon in Simms Creek") +
             scale_x_continuous(limits = c(0, 130),
                                breaks = seq(0, 130,10))+
             scale_y_continuous(limits = c(200, 1000),
                                breaks = seq(200, 1000,100))+
             theme_bw()
      
## * * * * Juvenile CT vs Adult CO ----
simms.COCT %>% select(out.year, n.fish, age.class.spp) %>%
               pivot_wider(names_from = age.class.spp, 
                           values_from = n.fish) %>% 
               mutate(CT_juvenile = 10*CT_juvenile) %>%
             
             ggplot(., aes(x = CO_adult, y = CT_juvenile)) +
             geom_point() +
             geom_smooth(method = 'lm', formula = y~x)+
             labs(x = "# of Adult Coho",
                  y = "# of Juvenile Cutthroat",
                  title = "Relationship between Adult spawning Coho Salmon and outmigrating \njuvenile Coho Salmon in Simms Creek") +
             scale_x_continuous(limits = c(0, 130),
                                breaks = seq(0, 130,10))+
             scale_y_continuous(limits = c(0, 800),
                                breaks = seq(0, 800,100))+
             theme_bw()
      
      