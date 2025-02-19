##____________________________________________________________________________________________________________________________----
## 6. Length Frequency
# 
#  How big were fish that passed the fence?
#
#  - Figure 1 - 2. CO (Spring and Fall) - Directional length frequency plot (# fish by FL, coloured by direction and faceted by year)
#  - Figure 3 - 4. CT (Spring and Fall) - Directional length frequency plot (# fish by FL, coloured by direction and faceted by year)
#  - Figure 5 to 19. CO Spring - Annual Directional Length Frequency Plots
#  - Figure 20 to 34. CT Spring - Annual Directional Length Frequency Plots
#  - Figure 35 to 49. CO Fall - Annual Directional Length Frequency Plots
#  - Figure 50 to 57. CT Fall - Annual Directional Length Frequency Plots (CT only capturedi n 8 years)
#
##____________________________________________________________________________________________________________________________----


## Confirm data is loaded 
    ifelse(exists('data_all') && is.data.frame(get('data_all')), 
           " ", 
           source("SourceFiles/1. Load and Prep Data.R"))
    
    ## Load Trap Effort Data 
    ifelse(exists('trap_effort') && is.data.frame(get('trap_effort')), 
           " ", 
           source("SourceFiles/2. Trapping Effort.R"))

## Prepare Tables
    max.fish  <- data_all %>% 
                      filter(Species %in% c("CT","CO")) %>%
                      group_by(Year,Period,Species) %>%
                      summarise(MaxFL = max(Length)) %>%
                      pivot_wider(names_from = c(Species, Period),values_from = MaxFL) %>%
                      select(Year, CT_Spring, CT_Fall, CO_Spring,CO_Fall)
    
        
# Prepare Figures ----
  ## Length Frequency of all fish

  # Faceted Directional Length Frequency Plots----
    ## Create empty list to save plots to 
    length.freq.all <- list()
    length.freq.plot <- list()
    
    ## Run for loop
    for(i in spp){
      for(j in sample_period){
        
        length.freq <- data_all %>%
                          filter(Species == i,
                                 Period == j) 
    
        fl.plot <- ggplot(length.freq) +
                      geom_histogram(aes(x = Length, fill = Direction)) + 
                      labs(x = "Fork Length (mm)", y = "Frequency (# of Fish)") +
                      # facet_grid(Year~.)+
                      theme_bw()

        length.freq.all[[i]][[j]] <- fl.plot
        
        length.freq.plot[[i]][[j]] <- fl.plot +
                                        facet_grid(Year~.)
    
        }
    
    }
    
    ## Save Figures
        ## * * Figure 1. Spring CO Directional Length.Freq. ----
        LF.plot_spring.all.CO <- length.freq.all[[1]][[1]]
        
        ## * * Figure 2. Fall CO Directional Length.Freq. ----
        LF.plot_fall.all.CO <- length.freq.all[[1]][[2]]
        
        ## * * Figure 3. Spring CT Directional Length.Freq. ----
        LF.plot_spring.all.CT <- length.freq.all[[2]][[1]]
        
        ## * * Figure 4. Fall CT Directional Length.Freq. ----
        LF.plot_fall.all.CT <- length.freq.all[[2]][[2]]
    
        ## * * Figure 1. Spring CO Directional Length.Freq. ----
        LF.plot_spring.CO <- length.freq.plot[[1]][[1]]
        
        ## * * Figure 2. Fall CO Directional Length.Freq. ----
        LF.plot_fall.CO <- length.freq.plot[[1]][[2]]
        
        ## * * Figure 3. Spring CT Directional Length.Freq. ----
        LF.plot_spring.CT <- length.freq.plot[[2]][[1]]
        
        ## * * Figure 4. Fall CT Directional Length.Freq. ----
        LF.plot_fall.CT <- length.freq.plot[[2]][[2]]
        

# Spring - Annual Direction Length Frequency Plot ----    
    ## Create Empty List to save plots to
    spring.lf.plot <- list()    

    ## Run For Loop
    for(i in spp){
        for(j in spring_years){

                
                ## Calculate Years Sampled during each period
                plot.dat <- data_all %>% 
                              filter(Species == i,
                                     Period == "Spring",
                                     Year == j)

                fl.plot <- ggplot(plot.dat) +
                              geom_histogram(aes(x = Length, fill = Direction)) + 
                              labs(x = "Fork Length (mm)", y = "Frequency (# of Fish)") +
                              facet_grid(Year~Species)+
                              theme_bw()
          
                spring.lf.plot[[i]][[j]] <- fl.plot
        
        # ggsave(plot, filename = paste0("Outputs/Length_vs_Date_", j,"_",i,".png"))
      }
    }
    
     ## Save Spring Plots 
         #  - Created annual plots for all 14 years of sampling. [[spp(1,2 = CO, RB)]][[year(1 to 14 = 2008 to 2024)]]
         #  - No sampling completed in spring of 2020, 2021, or 2023
            
        # Figure 5 to 19. Spring Coho by year        
             ## * * Figure 5. Spring 2008 Coho - Directional Length Freq. ----
             CO_2008_LF <- spring.lf.plot[[1]][[1]]
          
             ## * * Figure 11. Spring 2008 Coho - Directional Length Freq. ----
             CO_2014_LF <- spring.lf.plot[[1]][[7]]
        
             ## * * Figure 19. Spring 2024 Coho - Directional Length Freq. ----
             CO_2024_LF <- spring.lf.plot[[1]][[14]]
          
        # Figure 20 to 34. Spring CT by Year
            ## * * Figure 20. Spring 2008 CT - Directional Length Freq. ---- 
              CT_2008_LF <- spring.lf.plot[[2]][[1]]
      
            ## * * Figure 27. Spring 2008 CT - Directional Length Freq. ---- 
              CT_2014_LF <- spring.lf.plot[[2]][[7]]
      
            ## * * Figure 34. Spring 2008 CT - Directional Length Freq. ---- 
              CT_2014_LF <- spring.lf.plot[[2]][[14]]
    
    
# Fall - Annual Direction Length Frequency Plot ----    
    ## Create Empty List to save plots to
    fall.lf.plot <- list()    
    
    ## Run For Loop
    for(i in spp){
      for(j in fall_years){
        
        
        ## Calculate Years Sampled during each period
        plot.dat <- data_all %>% 
                        filter(Species == i,
                               Period == "Fall",
                               Year == j)
                      # as.Date("2024-04-27")
        
        fl.plot <- ggplot(plot.dat) +
                      geom_histogram(aes(x = Length, fill = Direction)) + 
                      labs(x = "Fork Length (mm)", y = "Frequency (# of Fish)") +
                      facet_grid(Year~Species)+
                      theme_bw()
                    
      fall.lf.plot[[i]][[j]] <- fl.plot
        
      }
    }
    
    ## Save Fall Plots 
    #  - Created annual plots for all 14 years of sampling. [[spp(1,2 = CO, RB)]][[year(1 to 14 = 2008 to 2023)]]
    #  - No sampling completed in fall of 2020 or 2021. 
    #  - CT only captured in 8 years (2008, 2010, 2012, 2014, 2017-2019, 2023)
    
    # Figure 35 to 19. Fall Coho by year        
        ## * * Figure 35. Fall 2008 Coho - Directional Length Freq. ----
        CO_2008_LF <- fall.lf.plot[[1]][[1]]
        
        ## * * Figure 41. Fall 2014 Coho - Directional Length Freq. ----
        CO_2014_LF <- fall.lf.plot[[1]][[7]]
        
        ## * * Figure 49. Fall 2023 Coho - Directional Length Freq. ----
        CO_2023_LF <- fall.lf.plot[[1]][[14]]
        
    # Figure 50 to 57. Fall CT by Year
        ## * * Figure 50. Fall 2008 CT - Directional Length Freq. ----
        CT_2008_LF <- fall.lf.plot[[2]][[1]]
        
        ## * * Figure 51. Fall 2010 CT - Directional Length Freq. ----
        CT_2010_LF <- fall.lf.plot[[2]][[3]]
    
        ## * * Figure 52. Fall 2012 CT - Directional Length Freq. ---- 
        CT_2012_LF <- fall.lf.plot[[2]][[5]]
    
        ## * * Figure 53. Fall 2014 CT - Directional Length Freq. ---- 
        CT_2014_LF <- fall.lf.plot[[2]][[7]]
    
        ## * * Figure 54. Fall 2017 CT - Directional Length Freq. ---- 
        CT_2017_LF <- fall.lf.plot[[2]][[10]]
        
        ## * * Figure 55. Fall 2018 CT - Directional Length Freq. ---- 
        CT_2018_LF <- fall.lf.plot[[2]][[11]]
        
        ## * * Figure 56. Fall 2019 CT - Directional Length Freq. ---- 
        CT_2019_LF <- fall.lf.plot[[2]][[12]]
        
        ## * * Figure 57. Fall 2023 CT - Directional Length Freq. ---- 
        CT_2023_LF <- fall.lf.plot[[2]][[14]]
        
## Calculate proportion of adult CCT relative to total CCT   
CT_age <- data_all %>%
              filter(Species == "CT") %>%
              mutate(age.class = ifelse(Length>230, "Adult","Juvenile")) %>%
              group_by(Year, age.class) %>%
              summarize(n = n()) %>%
              pivot_wider(names_from = age.class, values_from = n) %>%
              ungroup() %>%
              mutate(Total = Adult + Juvenile,
                     Adult.Prop = Adult/Total,
                     Adult.Total = sum(Adult),
                     Juv.Prop   = Juvenile/Total,
                     Juv.Total = sum(Juvenile))

CT.AgeClass.Prop <- head(CT_age,1) %>%
                    summarize(Adult = Adult.Total/(Juv.Total + Adult.Total),
                              Juvenile = 1-Adult)
