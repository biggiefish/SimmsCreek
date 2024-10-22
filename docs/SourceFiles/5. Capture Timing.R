# 5. Capture Timing  ----

## Load Data
    ## All Data
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
    
    
    pass.period <- catch_annual_table %>%
                        mutate(Date_min.std = case_when(year(Date_min) >= 0 ~ 'year<-'(Date_min, 2024)),
                               Date_med.std = case_when(year(Date_med) >= 0 ~ 'year<-'(Date_med, 2024)),
                               Date_max.std = case_when(year(Date_max) >= 0 ~ 'year<-'(Date_max, 2024))) %>%
                        group_by(Period, Species) %>%
                        summarize(minPass.min  = min(Date_min.std, na.rm = T),
                                  minPass.sd   = sd(Date_min.std, na.rm = T),
                                  minPass.Avg  = mean(Date_min.std, na.rm = T),
                                  maxPass.max  = max(Date_max.std, na.rm =T),
                                  maxPass.sd   = sd(Date_max.std, na.rm =T),
                                  maxPass.Avg  = mean(Date_max.std, na.rm =T))
    pass.period.spring <- pass.period %>% filter(Period == "Spring")
    pass.period.fall <- pass.period %>% filter(Period == "Fall")
   
    
    

# Prepare Plot Data 
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
       catch_timing_plots <- list()  
    
    ## Prep For Loop Variables
        sample_period <- c("Spring","Fall")
        spp <- c("CO","CT")

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
          
          ## Prepare Plots
          z <- ggplot(yy) + 
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
          catch_timing_plots[[i]][[j]] <- z 
          
        }
      }

    ## * * Spring CT Timing Plot ----
    catch_timing_plot.Spring.CT <- catch_timing_plots[[1]][[2]] +
                                      geom_label(data = labels, 
                                                 aes(label = YYYEAR),
                                                 x = as.Date("2024-04-15"), 
                                                 y = 125,
                                                 hjust = "left", vjust = 0,
                                                 inherit.aes = FALSE)
    
    ## * * Spring CO Timing Plot ----
    catch_timing_plot.Spring.CO <- catch_timing_plots[[1]][[1]] +
                                      geom_label(data = labels, 
                                                 aes(label = YYYEAR),
                                                 x = as.Date("2024-04-15"), 
                                                 y = 125,
                                                 hjust = "left", vjust = 0,
                                                 inherit.aes = FALSE) 
    
    ## * * Fall CT Timing Plot ----
    catch_timing_plot.Fall.CT <- catch_timing_plots[[2]][[2]] +
                                    geom_label(data = fall.labels, 
                                               aes(label = YYYEAR),
                                               x = as.Date("2024-09-15"), 
                                               y = 2,
                                               hjust = "left", 
                                               vjust = 0,
                                               inherit.aes = FALSE) +
                                    scale_y_continuous("Total Captures (# of Fish)",
                                                       breaks = seq(0,10,5),
                                                       sec.axis = sec_axis(~./10, 
                                                                           name = "Proportion of Total Catch (%)"))   
    
    ## * * Fall CO Timing Plot ----
    catch_timing_plot.Fall.CO <- catch_timing_plots[[2]][[1]] +
                                    geom_label(data = labels, 
                                               aes(label = YYYEAR),
                                               x = as.Date("2024-09-15"), 
                                               y = 100,
                                               hjust = "left", 
                                               vjust = 0,
                                               inherit.aes = FALSE)
        