# 4. Annual Captures  ----

## Load Data
    ## All Data
    ifelse(exists('data_all') && is.data.frame(get('data_all')), 
           " ", 
           source("SourceFiles/1. Load and Prep Data.R"))

    ## Load Trap Effort Data 
    ifelse(exists('trap_effort') && is.data.frame(get('trap_effort')), 
           " ", 
           source("SourceFiles/2. Trapping Effort.R"))

# Annual Catches ----
# * Annual Catches (All Species) ---- 
catch_annual_all_species <- data_all %>%  
                        # filter(Species %in% c("CT","CO")) %>%
                        group_by(Year, Period, Species) %>%
                        summarize(n = n(),
                                  n = coalesce(n,0),
                                  Date_min = min(Date, na.rm=T),
                                  Date_med = median(Date, na.rm=T),
                                  Date_max = max(Date, na.rm=T),
                                  FL_mean  = mean(Length, na.rm = T),
                                  FL_SD    = sd(Length, na.rm = T)) %>%
                        filter(!is.na(Species))
    
# * Annual Catches (CO CT) -----    
catch_annual_table <- catch_annual_all_species %>%
                      filter(Species %in% c("CT","CO"))

      ## * * Annual Catches (CO CT) - Wide ----
      catch_annual_table_wide <- catch_annual_table %>%
                            pivot_wider(names_from = Period,
                                        values_from = c(n, Date_min,Date_med, Date_max, FL_mean,FL_SD)) %>%
                            select(Year, Species, 
                                   n_Spring, Date_min_Spring, Date_med_Spring, 
                                   Date_max_Spring, FL_mean_Spring, FL_SD_Spring,
                                   n_Fall, Date_min_Fall, Date_med_Fall, 
                                   Date_max_Fall, FL_mean_Fall, FL_SD_Fall)

        
# Seasonal Catches ----
    
    ## Prep Data
    spp <- c("CO", "CT")
    # for loop to summarize CO and CT catch data 
    for(i in spp){
      
      x <- data_all %>%  
        filter(Species == i) %>%
        group_by(Year, Period, Species) %>%
        summarize(n = n(),
                  n = coalesce(n,0),
                  Date_min = min(Date, na.rm=T),
                  Date_med = median(Date, na.rm=T),
                  Date_max = max(Date, na.rm=T),
                  FL_mean = mean(Length, na.rm = T),
                  FL_min  = min(Length, na.rm = T),
                  FL_max  = max(Length, na.rm = T),
                  FL_SD   = sd(Length, na.rm = T))
      
      ## Create DF's named "catch_CT" and "catch_CO"
      assign(paste0("catch_", i, sep = ""), x)
    }
    
     trap_effort2 <- trap_effort %>% 
                     mutate(start = as.Date(start, "%b-%d"),
                            end = as.Date(end,"%b-%d"))
## * * Total Seasonal Catches All Species 
     catch_seasonal_all_species <- catch_annual_all_species %>%
                                   group_by(Period,Species) %>%
                                   summarise(Catch.Total = sum(n),
                                             Catch.Avg   = mean(n),
                                             Catch.SD    = sd(n),
                                             Years       = n()) %>%
                                   mutate(Years.Perc  = ifelse(Period == "Spring", 
                                                               Years/max(Years[Period=="Spring"]),
                                                               Years/max(Years[Period=="Fall"])))
           
    
## * * All Seasons Catch Table ----
catch_annual.Table.all.seasons <- left_join(trap_effort2, catch_CT, 
                                            by = c("Year", "Period"), 
                                            suffix = c("","_CT")) %>%
                                  left_join(.,catch_CO, 
                                            by = c("Year","Period"), 
                                            suffix = c("CT","CO")) %>%
                                  select("Year", "Period", "start","end", "trap_days", 
                                         "Date_medCT","Date_minCT", "Date_maxCT", 
                                         "nCT", "FL_meanCT", "FL_SDCT", "FL_minCT", "FL_maxCT",
                                         "Date_medCO","Date_minCO", "Date_maxCO", 
                                         "nCO", "FL_meanCO", "FL_SDCO", "FL_minCO", "FL_maxCO") %>%
                                  mutate(start = format(as.Date(start), "%b-%d"),
                                         end   = format(as.Date(end), "%b-%d"),
                                         Date_medCT = format(Date_medCT, "%b-%d"),
                                         Date_minCT = format(Date_minCT, "%b-%d"),
                                         Date_maxCT = format(Date_maxCT, "%b-%d"),
                                         Date_medCO = format(Date_medCO, "%b-%d"),
                                         Date_minCO = format(Date_minCO, "%b-%d"),
                                         Date_maxCO = format(Date_maxCO, "%b-%d")) %>%
                                  mutate_at(vars(nCT,nCO), ~replace_na(.,0))

## * * Spring Catch Table ----
catch_annual.Table.Spring <- catch_annual.Table.all.seasons %>% 
                              filter(Period == "Spring") %>%
                              select(!Period)

## * * Fall Catch Table ----
catch_annual.Table.Fall <- catch_annual.Table.all.seasons %>% 
                            filter(Period == "Fall") %>%
                            select(!Period)

# Catch Plots ----
## * Annual Catch Line Plot  (CO CT) ----
     catch_annual.lineplot <-   ggplot(catch_annual_table, 
                                       aes(x =Year, y = n,color = Species)) +
                                       geom_point()+
                                       geom_line(aes(linetype = Period)) +
                                       labs(y = "Total Captured (# of Fish)", x = NULL) +
                                       scale_x_continuous(breaks = seq(2004,2026,2)) +
                                       theme_bw() +
                                       theme(legend.position = "bottom")     
     
# * Total Catch Bar Plot ----
    ## Prepare data
        ## Define Capture Periods
        sample_period <- c("Spring", "Fall")
    
        ## Create empty list to save plots to
        catch_plots <- list()

    ## Run for loop      
    for(i in sample_period){
      
      ## Prepare data
          xx <- data_all %>%
            filter(Period == i) %>%
            ungroup() %>%
            group_by(Year, Period, Species) %>%
            summarize(n = n()) %>%
            filter(!is.na(Period),
                   !is.na(Species), 
                   Species %in% c("CO","CT")) 
      ## Create Plot
      g <- ggplot(xx) +
              geom_col(aes(x = Year, y = n)) + 
              facet_grid(Species~.) +
              labs(x = "", 
                   y= "Total Captures (# of Fish)") +
              scale_x_continuous(breaks = seq(2008,2024,2)) +
              theme_bw()
      
      # assign name to plot 
      catch_plots[[i]] <- g        
      
    }

## * * Annual Spring Catch Plot ----
       catch_annual_barPlot.Spring <- catch_plots[[1]]

## * * Annual Fall Catch Plot ----
        catch_annual_barPlot.Fall   <- catch_plots[[2]]


        