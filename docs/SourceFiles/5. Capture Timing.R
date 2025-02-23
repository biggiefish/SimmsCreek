##____________________________________________________________________________________________________________________________----
# 5. Capture Timing  ----
#
# When did fish pass the fence? 
#
#   - Table 1. Spring Passage Dates (Date of first and last pass, 
#                                    difference from overall mean first/last pass dates, 
#                                    days after trap start that fish was first caught)
#   - Table 2. Fall Passage Dates (see above, but for fall)
#   - Table 3. Spring Peak Passage Dates (date of peak passage, difference from mean peak passage date (per spp))
#   - Table 4. Fall Peak Passage Dates (date of peak passage, difference from mean peak passage date (per spp))
#
#   - Figure 1. Spring CT Timing Plot (Total daily catch, faceted by year)
#   - Figure 2. Spring CO Timing Plot (Total daily catch, faceted by year)
#   - Figure 3. Fall CT Timing Plot (Total daily catch, faceted by year)
#   - Figure 4. Fall CO Timing Plot (Total daily catch, faceted by year)
#   - Figure 5. Date of 50% Passage - Spring (Frequency of 50% passage date, faceted by spp)
#   - Figure 6. Date of 50% Passage - Fall (Frequency of 50% passage date, faceted by spp)
#   - Figure 7. Date of Peak Passage - Spring (Frequency of 50% passage date, faceted by spp)
#   - Figure 8. Date of Peak Passage - Fall (Frequency of 50% passage date, faceted by spp)
#   - Figure 9. Stacked daily catch plot - Spring CT - (Stacked total daily catch, colored by year) 
#   - Figure 10. Stacked daily catch plot - Spring CO - (Stacked total daily catch, colored by year) 
#   - Figure 11. Stacked daily catch plot - Fall CT - (Stacked total daily catch, colored by year) 
#   - Figure 12. Stacked daily catch plot - Fall CO - (Stacked total daily catch, colored by year) 
##____________________________________________________________________________________________________________________________----

## Confirm data is loaded 
    ifelse(exists('data_all') && is.data.frame(get('data_all')), 
           " ", 
           source("SourceFiles/1. Load and Prep Data.R"))
    
    ## Load Trap Effort Data 
    ifelse(exists('trap_effort') && is.data.frame(get('trap_effort')), 
           " ", 
           source("SourceFiles/2. Trapping Effort.R"))
    
    ## Load Annual Capture Data
    ifelse(exists('catch_annual') && is.data.frame(get('catch_annual')), 
           " ", 
           source("SourceFiles/4. Annual Captures.R"))
    
# Summary Tables ----

## *  First and Last Pass Dates ----    
    pass.period <- catch_annual_table %>%
                        mutate(Date_min.std = case_when(year(Date_min) >= 0 ~ 'year<-'(Date_min, 2024)),
                               Date_med.std = case_when(year(Date_med) >= 0 ~ 'year<-'(Date_med, 2024)),
                               Date_max.std = case_when(year(Date_max) >= 0 ~ 'year<-'(Date_max, 2024))) %>%
                        group_by(Period, Year, Species) %>%
                        summarize(minPass.min  = min(Date_min.std, na.rm = T),
                                  maxPass.max  = max(Date_max.std, na.rm =T)) %>%
                        ungroup() %>%
                        group_by(Period,Species) %>%
                        mutate(minMean  = round_date(mean(minPass.min, na.rm = TRUE),"day"),
                               maxMean  = round_date(mean(maxPass.max, na.rm = TRUE),"day"),
                               minMeanDiff = difftime(minMean, minPass.min, "units" = c("days")),
                               maxMeanDiff = difftime(maxMean, maxPass.max, "units" = c("days"))) %>%
                        left_join(trap_effort, by = c("Period", "Year")) %>%
                        mutate(days_after_start = as.Date(minPass.min)-as.Date(start, "%b-%d", "%Y"=2024)) %>%
                        group_by(Period,Species) %>%
                        mutate(Mean_daysAfterStart = mean(days_after_start, na.rm = TRUE),
                               SD_daysAfterStart   = sd(days_after_start, na.rm = TRUE),
                               ## first_day = # seasons where fish were caught on first day of trapping
                               first_day           = sum(days_after_start == 0))   
    
    ### * * Table 1. Spring Passage Summary ----
    pass.period.spring <- pass.period %>% filter(Period == "Spring")
    
    ### * * Table 2. Fall Passage Summary ----
    pass.period.fall <- pass.period %>% filter(Period == "Fall")
   
## Date of 50% Passage ----
    ## Daily proportion of total catch during each year and season
    catch_prop <- data_all %>% 
                      filter(Species %in% c("CO","CT"),
                             complete.cases(Species)) %>%
                      mutate(catch =1) %>%
                      group_by(Period, Year, Date, Species) %>%
                      summarize(catch       = sum(catch),
                                cum_catch   = cumsum(catch)) %>%
                      ungroup() %>%
                      group_by(Period, Year) %>%
                      mutate(total_catch = max(cum_catch),
                             prop        = cum_catch/total_catch,
                             date.std    = case_when(year(Date) >= 0 ~ 'year<-'(Date, 2024)),
                             week.std    = week(date.std)) %>%
                      mutate(Decade = floor_decade(as.numeric(Year))) %>%
                              select(Species, Period, Year, date.std, everything())
    
            ## Spring - Daily Proportion of Total Catch 
            catch_prop.spring <- catch_prop %>% filter(Period == "Spring")
            
            ## Fall - Daily Proportion of Total Catch 
            catch_prop.fall <- catch_prop %>% filter(Period == "Fall")
     
    ## Calculate Date of 50% Passage              
    catch50prop <- catch_prop %>% 
                      mutate(dif = abs(prop-0.5)) %>%
                      group_by(Species, Period, Year) %>%
                      filter(dif == min(dif)) %>%
                      summarise(date.50p = round_date(mean(date.std, na.rm = TRUE), "days")) %>%
                      ungroup() %>%
                      group_by(Species, Period) %>%
                      mutate(meanDate50p = round_date(mean(date.50p, na.rm =TRUE), "days"),
                             Date50pDiff = difftime(meanDate50p, date.50p,"units" = c("days")))
    

    ## * Table 1. Date when 50% of run had passed fence each spring ---- 
    catch50prop.spring <- catch50prop %>% filter(Period == "Spring")
    
    ## * Table 2. Date when 50% of run had passed fence each fall ---- 
    catch50prop.fall <- catch50prop %>% filter(Period == "Fall")
    
## Date of Peak Migration ---- 
    catch_peak <- catch_prop <- data_all %>% 
                          filter(Species %in% c("CO","CT"),
                                 complete.cases(Species)) %>%
                          mutate(catch =1) %>%
                          group_by(Period, Year, Date, Species) %>%
                          summarize(catch = sum(catch)) %>% 
                          ungroup() %>%
                          group_by(Period, Year, Species) %>%
                          filter(catch == max(catch)) %>%
                          mutate(date.std = case_when(year(Date) >= 0 ~ 'year<-'(Date, 2024))) %>%
                          rename(PeakCatch = "catch") %>%
                          ungroup() %>%
                          group_by(Period,Species) %>%
                          mutate(MeanPeak  = round_date(mean(date.std, na.rm = TRUE),"day"),
                                 MeanDiff = difftime(MeanPeak, date.std, "units" = c("days"))) %>%
                          filter(Species %in% c("CO","CT")) %>%
                          ungroup()
    
    unique(catch_peak$Species)
    str(catch_peak)
    
    ## * Table 3. Spring Peak Migration Dates ----                    
    catch_peak.spring <- catch_peak %>% 
                            filter(Period == "Spring") %>% 
                            pivot_wider(names_from = "Species", 
                                        values_from = c("PeakCatch","date.std", "MeanPeak", "MeanDiff")) %>%
                            select(1:3, PeakCatch_CT, date.std_CT, MeanPeak_CT, MeanDiff_CT,
                                        PeakCatch_CO, date.std_CO, MeanPeak_CO, MeanDiff_CO)
    
    ## * Table 4. Spring Peak Migration Dates ----                      
    catch_peak.fall <- catch_peak %>% 
                            filter(Period == "Fall") %>%
                            pivot_wider(names_from = "Species", 
                                        values_from = c("PeakCatch","date.std", "MeanPeak",MeanDiff)) %>%
                            select(1:3, PeakCatch_CT, date.std_CT, MeanPeak_CT, MeanDiff_CT,
                                   PeakCatch_CO, date.std_CO, MeanPeak_CO, MeanDiff_CO)
                    
    
## Plots ----
## Daily Captures ----
    ## Plot labels 
        ## Spring Labels
        labels <- data.frame(Year = unique(data_all$Year), YYYEAR = unique(data_all$Year)) %>%
          # data.frame(Year = unique(CCT_catch$Year), YYYEAR = unique(CCT_catch$Year)) %>%
          mutate(YYYEAR = ifelse(Year == 2020, "2020 - COVID",
                                 ifelse(Year == 2021,"2021 - COVID",
                                        ifelse(Year == 2023, "2023 - Strike",Year))),
                 labX = as.Date(ifelse(Year == 2020, as.Date("2024-05-25"),
                                       ifelse(Year == 2021,as.Date("2024-05-25"),
                                              ifelse(Year == 2023, as.Date("2024-05-25"),as.Date("2024-04-16"))))))
        
        ## Fall Labels
        fall.labels <- data.frame(Year = unique(data_all$Year), YYYEAR = unique(data_all$Year)) %>%
          # data.frame(Year = unique(CCT_catch$Year), YYYEAR = unique(CCT_catch$Year)) %>%
          mutate(YYYEAR = ifelse(Year == 2020, "2020 - COVID",
                                 ifelse(Year == 2021,"2021 - COVID",Year)),
                 labX = as.Date(ifelse(Year == 2020, as.Date("2024-10-01"),
                                       ifelse(Year == 2021,as.Date("2024-10-25"),as.Date("2024-10-01")))))      

    ## Create empty list to save plots to
       daily_catch_plot <- list()  
       daily_catch_summary_plot <- list()
       
    ## Prep For Loop Variables

             ## Run for loops      
      for(i in sample_period){
        for(j in spp){
          
          xx <- data_all %>% 
            filter(Period == i,
                   Species == j)
          # Species %in% c("CO","CT"))
          
          ## Prepare Data
          yy <- left_join(date_seq, xx, 
                          by = c("Date", "Month")) %>%
            mutate(Year = strftime(Date, format = "%Y"),
                   Species = j,
                   Period = i,
                   Catch = ifelse(Length >0,1,0),) %>%
            group_by(Period, Date, Species) %>%
            summarize(catch = sum(Catch)) %>%
            mutate(catch = coalesce(catch,0),
                   Year = strftime(Date, format = "%Y")) %>%
            ungroup() %>%
            group_by(Year, Period, Species) %>%
            mutate(cum_catch   = cumsum(catch),
                   total_catch = max(cum_catch),
                   prop        = cum_catch/total_catch,
                   prop.scaled = ifelse(Species == "CT" & Period == "Fall", prop*10,prop*300),
                   # prop.scaled = prop*ifelse(i == "Spring",300,100),
                   date.std    = case_when(year(Date) >= 0 ~ 'year<-'(Date, 2024)),
                   week.std    = week(date.std),
                   Period.Filter = ifelse(date.std>=as.Date("2024-04-15") & 
                                            date.std <=as.Date("2024-06-15"),
                                          "Spring",
                                          ifelse(date.std>=as.Date("2024-09-15") & 
                                                   date.std <=as.Date("2024-12-31"),
                                                 "Fall",NA))) %>%
            filter(Period.Filter == i) %>%
            mutate(Decade = floor_decade(as.numeric(Year)),
                   Species = j,
                   Period  = i) %>%
            select(Species, Period, everything()) %>%
            filter(!is.na(Period.Filter))
          
          ## Save dataframes for each season
          assign(paste("catch_timing.plot",i, j, sep="_"), yy)
          
          ## Daily Capture Plot 
          y.plot <- ggplot(yy) + 
                  geom_col(aes(x= date.std, y =catch))+
                  geom_line(aes(x= date.std, y = prop.scaled)) +
                  scale_x_date(date_breaks = "2 week", date_labels = "%b-%d") +
                  scale_y_continuous("Total Captures (# of Fish)",
                                     sec.axis = sec_axis(~./300, 
                                                         name = "Proportion of Total Catch (%)"))+
                  facet_grid(Year~Species) +
                  labs(x="", y = "Total Captures (# of Fish)") +
                  theme_bw() +
                  theme(strip.text.y = element_blank())  
                
          # assign name to plot 
          daily_catch_plot[[i]][[j]] <- y.plot 
          
          
          ## Daily Capture - Summary Plots
          yy.plot <- ggplot(yy) + 
                        geom_col(aes(x = date.std, y = catch, fill = Year)) +
                        labs(x="", y = "Total Captures (# of Fish)") +
                        theme_bw()
          
          daily_catch_summary_plot[[i]][[j]] <- yy.plot
 

                }
      }

    ## * * Figure 1. Spring CT Timing Plot ----
    daily_catch_plot.Spring.CT <- daily_catch_plot[[1]][[2]] +
                                      geom_label(data = labels, 
                                                 aes(label = YYYEAR),
                                                 x = as.Date("2024-04-15"), 
                                                 y = 250,
                                                 hjust = "left", vjust = 0,
                                                 inherit.aes = FALSE)
    
    ## * * Figure 2. Spring CO Timing Plot ----
    daily_catch_plot.Spring.CO <- daily_catch_plot[[1]][[1]] +
                                      geom_label(data = labels, 
                                                 aes(label = YYYEAR),
                                                 x = as.Date("2024-04-15"), 
                                                 y = 250,
                                                 hjust = "left", vjust = 0,
                                                 inherit.aes = FALSE) 
    
    ## * * Figure 3. Fall CT Timing Plot ----
    daily_catch_plot.Fall.CT <- daily_catch_plot[[2]][[2]] +
                                    geom_label(data = fall.labels, 
                                               aes(label = YYYEAR),
                                               x = as.Date("2024-09-15"), 
                                               y = 2,
                                               hjust = "left", 
                                               vjust = 0,
                                               inherit.aes = FALSE) +
                                    scale_y_continuous("Total Captures (# of Fish)",
                                                       # breaks = seq(0,10,5),
                                                       sec.axis = sec_axis(~./10, 
                                                       name = "Proportion of Total Catch (%)"))   
    
    ## * * Figure 4. Fall CO Timing Plot ----
    daily_catch_plot.Fall.CO <- daily_catch_plot[[2]][[1]] +
                                    geom_label(data = labels, 
                                               aes(label = YYYEAR),
                                               x = as.Date("2024-09-15"), 
                                               y = 100,
                                               hjust = "left", 
                                               vjust = 0,
                                               inherit.aes = FALSE)
        
## Date of 50% Passage Plot ----
  ## * * Figure 5. Date of 50% Passage - Spring ----
  DateHalfPassed.plot.spring <- ggplot(catch50prop.spring) +
                                    geom_histogram(aes(x =date.50p)) + 
                                    labs(x = "", y = "Frequency") +
                                    scale_y_continuous(breaks = seq(0,3,1)) +
                                    theme_bw() +
                                    facet_grid(Species ~.)
        
  ## * * Figure 6. Date of 50% Passage - Fall ----
  DateHalfPassed.plot.fall <- ggplot(catch50prop.fall) +
                              geom_histogram(aes(x =date.50p)) + 
                              labs(x = "", y = "Frequency") +
                              scale_y_continuous(breaks = seq(0,3,1)) +
                              theme_bw() +
                              facet_grid(Species ~.)
  
## Peak Catch Timing Plots ----
  ## * * Figure 7. Date of Peak Passage - Spring ----
       period <- c("Spring","Fall")
       peak.plot <- list()
       
       for(i in spp){
         for(j in period){
         
            xx <-  catch_peak %>%
                        filter(Species == i,
                               Period == j)
             
                        t.plot  <- ggplot(xx) + 
                                   geom_histogram(aes(x =date.std)) + 
                                                labs(x = "", y = "Frequency") +
                                                scale_y_continuous(breaks = seq(0,2,1)) +
                                                theme_bw()
                        
                        peak.plot[[i]][[j]] <- t.plot
         }
       }
        ## Frequency of Peak Spring Coho Outmigration            
        CO.peak.spring.plot <- peak.plot[[1]][[1]]
        
        ## Frequency of Peak Fall Coho In migration            
        CO.peak.fall.plot <- peak.plot[[1]][[2]]
        
        ## Frequency of Peak Spring CT Outmigration 
        CT.peak.spring.plot <- peak.plot[[2]][[1]]
        
        ## Frequency of Peak Fall CT Outmigration 
        CT.peak.fall.plot <- peak.plot[[2]][[2]]
      
        
  ## * * Figure 8. Date of Peak Passage - Fall ----
  Catch_Peak_Plot.Fall <- catch_peak %>%
                             filter(Period == "Fall") %>%
                             
                             ggplot(., aes(x = Year, y = PeakCatch)) + 
                                  
                                  labs(x = "", y = "Frequency") +
                                  # scale_y_continuous(breaks = seq(0,2,1)) +
                                  theme_bw() +
                                  facet_grid(Species ~., scales = "free_y") 
  
  
## Daily Summary Plot ----
  ## * * Figure 9 - Daily Catch Plot - Spring CT
  daily_catch_summary_plot.Spring.CT <- daily_catch_summary_plot[[1]][[2]]
        
  ## * * Figure 10 - Daily Catch Plot - Spring CO        
  daily_catch_summary_plot.Spring.CO <- daily_catch_summary_plot[[1]][[1]] 
  
  ## * * Figure 11 - Daily Catch Plot - Fall CT
  daily_catch_summary_plot.Fall.CT <- daily_catch_summary_plot[[2]][[2]]
  
  ## * * Figure 12 - Daily Catch Plot - Fall CO
  daily_catch_summary_plot.Fall.CO <- daily_catch_summary_plot[[2]][[1]]
  
  
## Capture Timing Boxplot ----
  bx.dat <- data_all %>%
                filter(Species %in% c("CO","CT"))
  
boxplots.summary <- list()

      for(i in sample_period){
        
            ## Prep Data
            bx <- bx.dat %>%
                    filter(Period == i)
            
            bxplot.sm <- ggplot(bx, aes(x = Species,
                                        y = date.std)) +
                            geom_boxplot(alpha = 0.8, show.legend = FALSE) +
                            geom_point(aes(color = as.factor(Year)),
                                       shape = 1,
                                       alpha = 0.2,
                                       position = position_jitter()) +
                            labs(x = "", y = "") +
                            coord_flip() +
                            theme_bw() +
                            theme(legend.title = element_blank(),
                                  legend.position = "bottom",
                                  legend.key.width = unit(2, "cm"),
                                  legend.key.height = unit(0.1,"cm")) +
                            guides(color = guide_legend(override.aes = list(alpha = 1), 
                                                        nrow = 2))
            
            boxplots.summary[[i]] <- bxplot.sm
      }

  Spring.Boxplot.Summary <- boxplots.summary[[1]]
  Fall.Boxplot.Summary <- boxplots.summary[[2]]
      
  ## Plot of Median Date Across Years
  boxplots.years <-list()
  for(j in spp){
    for(i in sample_period){
              
    bx <- bx.dat %>%
            filter(Period == i,
                   Species == j)

      bxplot <- ggplot(bx, aes(x = as.factor(Year), 
                                                y = date.std, 
                                                color = Species)) +
      geom_boxplot(alpha = 0.8) +
      geom_point(aes(color = Species),
                 shape = 1,
                 alpha = 0.2,
                 position = position_jitterdodge()) +
      geom_hline(aes(linetype = "Mean", yintercept = ifelse(Period == "Spring",ifelse(Species == "CO",as.Date("2024-05-15"),as.Date("2024-05-11")),
                                                  ifelse(Species =="CO",as.Date("2024-10-31"),as.Date("2024-11-02")))))+  
      scale_linetype_manual(values = "dashed") +
      labs(x = "", y = "Date") +
      # coord_flip() +
      guides(linetype = guide_legend(title = element_blank()))+  
      theme_bw()
    
    boxplots.years[[i]][[j]] <- bxplot
    }
  }
  

  Spring.Boxplot.Years.CO <-  boxplots.years[[1]][[1]]
  Fall.Boxplot.Years.CO   <-  boxplots.years[[2]][[1]]
  Spring.Boxplot.Years.CT <-  boxplots.years[[1]][[2]]
  Fall.Boxplot.Years.CT   <-  boxplots.years[[2]][[2]]
            
  ## Faceted Median Date Across Years
  boxplots.years <-list()
  
    for(i in sample_period){
      
      bx <- bx.dat %>%
        filter(Period == i)
      
      bxplot <- ggplot(bx, aes(y = as.factor(Year), 
                               x = date.std, 
                               color = Species)) +
        geom_boxplot(alpha = 0.8) +
        geom_point(aes(color = Species),
                   shape = 1,
                   alpha = 0.2,
                   position = position_jitterdodge()) +
        geom_hline(aes(linetype = "Mean CO", yintercept = ifelse(Period == "Spring", as.Date("2024-05-15"), as.Date("2024-10-31"))))+
        geom_hline(aes(linetype = "Mean CT", yintercept = ifelse(Period == "Spring", as.Date("2024-05-11"), as.Date("2024-11-02"))))+  
        scale_linetype_manual(values = c("dashed", "dotted")) +
        labs(x = "", y = "") +
        scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
        guides(linetype = guide_legend(title = element_blank()))+  
        theme_bw() +
        theme(legend.position = "bottom")
      
      boxplots.years[[i]] <- bxplot
    }
  

  Spring.Boxplot.Facet <-  boxplots.years[[1]]
  Fall.Boxplot.Facet   <-  boxplots.years[[2]]
            
            
## Mean Capture Timing Trends ----
bx.spring <- bx.dat %>% 
                filter(Period =="Spring")       

      ## Plot of Mean Passage Date
      mean.spring.trend <- ggplot(bx.spring,
                                  aes(x = Year, 
                                      y = date.std, 
                                      color = Species)) + 
                            geom_point(shape = 1,
                                       alpha = 0.2,
                                       position = position_jitterdodge()) +
                            geom_smooth(method = 'loess') +
                            scale_x_continuous(breaks = seq(2008,2024,2)) +
                            labs(x = "", y = "") +
                            theme_bw()
                            



  

      
  
  
        
        