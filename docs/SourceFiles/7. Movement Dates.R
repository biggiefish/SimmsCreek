## Spawning occuring before fence is installed?? Largest fish are observed during during fall

##____________________________________________________________________________________________________________________________----
## 7. Movement Timing
# 
#  How big were fish that passed the fence?
#
#  - Figure 1 - 2. CO (Spring and Fall) - Directional length frequency plot (# fish by FL, coloured by direction and faceted by year)
#  - Figure 3 - 4. CT (Spring and Fall) - Directional length frequency plot (# fish by FL, coloured by direction and faceted by year)
#  - Figure 5 to 19. CO Spring - Annual Directional Length Frequency Plots
#  - Figure 20 to 34. CT Spring - Annual Directional Length Frequency Plots
#  - Figure 35 to 49. CO Fall - Annual Directional Length Frequency Plots
#  - Figure 50 to 57. CT Fall - Annual Directional Length Frequency Plots (CT only captured in 8 years)
#
##____________________________________________________________________________________________________________________________----

## Confirm data is loaded 
ifelse(exists('data_all') && is.data.frame(get('data_all')), 
       " ", 
       source("SourceFiles/1. Load and Prep Data.R"))


## Create Empty List to save plots to
    spring.move.plot <- list()    
    
    ## Run For Loop
    for(i in spp){
      for(j in spring_years){
        
        ## Calculate Years Sampled during each period
        plot.dat <- data_all %>% 
          filter(Species == i,
                 Period == "Spring",
                 Year == j,
                 Length >125)
       summary(data_all$Length[data_all$Period =="Spring"])
        
            ## Establish Plot Boundaries
            FL_min <- rounder(min(plot.dat$Length))-10
            FL_max <- rounder(max(plot.dat$Length))+10
            seqby <- rounder((FL_max - FL_min)/10)
        
        move.plot <- ggplot(plot.dat) + 
                        geom_point(aes(x = date.std, y = Length, color = Direction), 
                                   shape = 1,
                                   alpha = 0.7) + 
                        scale_y_continuous(breaks = seq(100, FL_max, 50),
                                           limits = c(100,FL_max))+
                        scale_x_date(breaks = "5 days",
                                     limits = c(as.Date("2024-04-05"),as.Date("2024-06-15")),
                                     date_labels = "%b-%d")+
                        labs(x = "", 
                             y = "Fork Length (mm)", 
                             title = paste0("Spring ", j,"-",i)) +
                        # annotate("text", 
                        #          label = paste0("Spring ",j," - ",i), 
                        #          # x = 0.1,
                        #          x = min(plot.dat$date.std), 
                        #          y= 0.9,
                        #          # y= 350,
                        #          hjust = "left", 
                        #          vjust = 0,) +
                        theme_bw()+
                        theme(axis.text.x = element_text(angle = 45, 
                                                         vjust = 0.9, 
                                                         hjust = 1)) 

        spring.move.plot[[i]][[j]] <- move.plot
      }
    }
        ## Save Spring Plots 
        #  - Created annual plots for all 14 years of sampling. [[spp(1,2 = CO, RB)]][[year(1 to 14 = 2008 to 2024)]]
        #  - No sampling completed in spring of 2020, 2021, or 2023
        
        # Figure 1 to 14. Spring Coho by year        
                ## * Figure 1. Spring 2008 Coho - Directional Length Freq. ----
                      CO_2008_springLF <- spring.move.plot[[1]][[1]]
                ## * Figure 7. Spring 2008 Coho - Directional Length Freq. ----
                      CO_2014_springLF <- spring.move.plot[[1]][[7]]
                ## * Figure 14. Spring 2024 Coho - Directional Length Freq. ----
                      CO_2024_springLF <- spring.move.plot[[1]][[14]]
                
        # Figure 15 to 29. Spring CT by Year
                ## * Figure 15. Spring 2008 CT - Directional Length Freq. ---- 
                      CT_2008_springLF <- spring.move.plot[[2]][[1]]
                ## * Figure 22. Spring 2008 CT - Directional Length Freq. ---- 
                      CT_2014_springLF <- spring.move.plot[[2]][[7]]
                ## * Figure 29. Spring 2008 CT - Directional Length Freq. ---- 
                      CT_2014_springLF <- spring.move.plot[[2]][[14]]
                
    
    
## Fall - Annual Direction Length Frequency Plot ----    
   ## Create Empty List to save plots to
   fall.move.plot <- list()    
        
   ## Run For Loop
   for(i in fall_years){
            
    ## Prep data
    plot.dat <- data_all %>% 
                   filter(Period == "Fall",
                          Species == "CO",
                          Year == i)
    
   
            ## Establish Plot Boundaries
            FL_min <- rounder(min(plot.dat$Length))-10
            FL_max <- rounder(max(plot.dat$Length))+10
            seqby <- rounder((FL_max - FL_min)/10)
            
     ## Create Plot       
     temp.plot <- ggplot(plot.dat) + 
                      geom_point(aes(x = date.std, y = Length),
                                 shape = 1,
                                 alpha = 0.7) + 
                      labs(x = "Passage Date", y = "Fork Length (mm)") +
                      scale_x_date(breaks = "5 days",
                                   limits = c(as.Date("2024-10-01"),as.Date("2024-11-20")),
                                   date_labels = "%b-%d")+
                      scale_y_continuous(limits = c(200,800))+
                      theme_bw()+ 
                      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
                      facet_grid(.~Year)
                    
            fall.move.plot[[i]] <- temp.plot

          }
      
        ## Save Fall Plots 
        #  - Created annual plots for all 14 years of sampling (2008 to 2023).
        #  - No sampling completed in fall of 2020 or 2021.
        #  - Coho only
        
        # Figure 30 to 44. Fall Coho by year        
              ## * Figure 30. Fall 2008 Coho - Directional Length Freq. ----
                    CO_2008_LF <- fall.move.plot[[1]]
              ## * Figure 37. Fall 2008 Coho - Directional Length Freq. ----
                    CO_2014_LF <- fall.move.plot[[7]]
              ## * Figure 44. Fall 2023 Coho - Directional Length Freq. ----
                    CO_2023_LF <- fall.move.plot[[14]]
              
        
              
       