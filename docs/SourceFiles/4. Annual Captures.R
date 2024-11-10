##____________________________________________________________________________________________________________________________----
# 4. Fish Captures  ----

# Number and size of fish caught each year and season, in different formats

## Annual Capture Summaries
#   - Table 1. Annual Catches - number and size of all species captured per year/season.  
#   - Table 2. Annual Catches (CO and CT only)
#   - Table 3. Annual Catches (CO CT, wide format)
#
#
## Seasonal Capture Summaries
#   - Table 4. Annual Spring Catches of CO and CT (total number, date range and size range)
#   - Table 5  Annual Fall Catches of CO CT(total number, date range and size range)
#   - Table 6. Seasonal Coho Catches (total number, date range and size range)
#   - Table 7. Seasonal CT Catches (total number, date range and size range)
#   - Table 8. All Seasons Catch (Kable Format: Start and End date of each season, date and size range of CO and CT)
#   - Table 9. Spring Catch (Kable Format: Start and End date of each season, date and size range of CO and CT)
#   - Table 10. Spring - days to first/last catch
#   - Table 11. Fall Catch (Kable Format: Start and End date of each season, date and size range of CO and CT)
#
## Plots
#   - Figure 1. Annual Catch Line Plot  (CO CT) - Line Plot - # CT and CO caught during each season.
#   - Figure 2. Spring Catch Plot - Bar Plot - Total captures per year, faceted by spp.
#   - Figure 3. Fall Catch Plot - Bar Plot - Total captures per year, faceted by spp.
##____________________________________________________________________________________________________________________________----


## Confirm data is loaded 
    ifelse(exists('data_all') && is.data.frame(get('data_all')), 
           " ", 
           source("SourceFiles/1. Load and Prep Data.R"))

    ## Load Trap Effort Data 
    ifelse(exists('trap_effort') && is.data.frame(get('trap_effort')), 
           " ", 
           source("SourceFiles/2. Trapping Effort.R"))

# Annual Catches ----
    # * Table 1. Annual Catches (All Species) ---- 
    catch_annual_all_species <- data_all %>%  
                            # filter(Species %in% c("CT","CO")) %>%
                            group_by(Year, Period, Species) %>%
                            summarize(n = n(),
                                      n = coalesce(n,0),
                                      Date_min = min(Date, na.rm=T),
                                      Date_med = median(Date, na.rm=T),
                                      Date_max = max(Date, na.rm=T),
                                      FL_mean  = mean(Length, na.rm = T),
                                      FL_SD    = sd(Length, na.rm = T),
                                      FL_max   = max(Length, na.rm = T)) %>%
                            filter(!is.na(Species))
        
        
    # * Table 2. Annual Catches (CO CT) -----    
    catch_annual_table <- catch_annual_all_species %>%
                          filter(Species %in% c("CT","CO"))
    
      ## * * Table 3. Annual Catches (CO CT) - Wide ----
          ## Prep Data
          catch_annual_table_wide <- catch_annual_table %>%
                                pivot_wider(names_from = Period,
                                            values_from = c(n, Date_min,Date_med, Date_max, FL_mean,FL_SD, FL_max)) %>%
                                select(Year, Species, 
                                       n_Spring, Date_min_Spring, Date_med_Spring, 
                                       Date_max_Spring, FL_mean_Spring, FL_SD_Spring, FL_max_Spring,
                                       n_Fall, Date_min_Fall, Date_med_Fall, 
                                       Date_max_Fall, FL_mean_Fall, FL_SD_Fall, FL_max_Fall) 

# Seasonal Catches ----
    ## * Seasonal Summary (CO and CT) ----
            ## * * Table 4. Spring Catch Table (CO CT) ----
            catch_annual_table_wide.spring <- catch_annual_table_wide %>% select(1:9)
        
            ## * * Table 5. Fall Catch Table (CO CT) ----
            catch_annual_table_wide.fall <- catch_annual_table_wide %>% select(1:2, 10:16)
            
        
    ## Seasonal Summary by Spp ----

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
                          FL_min  = min(Length, na.rm = T),
                          FL_max  = max(Length, na.rm = T),
                          FL_mean = mean(Length, na.rm = T),
                          FL_SD   = sd(Length, na.rm = T))
              
              ## Create DF's named "catch_CT" and "catch_CO"
              assign(paste0("catch_", i, sep = ""), x)
            }
            
            ## View Outputs
                ## * * Table 6. Summary of Coho ----
                catch_CO
                
                ## * * Table 7. Summary of CT ----
                catch_CT
            
      ## Prep Trap Effort Data    
      trap_effort2 <- trap_effort %>% 
                     mutate(start = as.Date(start, "%b-%d"),
                            end = as.Date(end,"%b-%d"))
        

           
    
## * Table 8. All Seasons Catch Kable ----
catch_annual.kable.all.seasons <- left_join(trap_effort2, catch_CT, 
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
      

## * Table 9. Spring Catch Kable ----
catch_annual.Table.Spring <- catch_annual.kable.all.seasons %>% 
                              filter(Period == "Spring") %>%
                              select(!Period)
     
     ## * Table 10. Spring - days to first/last catch -----
     catch_annual.Table.Spring.calcs  <- catch_annual.Table.Spring %>%
                                              summarise(CT_days.after.start = as.numeric(as.Date(Date_minCT, format = "%b-%d") - as.Date(start, format = "%b-%d")),
                                                     CT_days.before.end  = as.numeric(as.Date(Date_maxCT, format = "%b-%d") - as.Date(end, format = "%b-%d")),
                                                     CO_days.after.start = as.numeric(as.Date(Date_minCO, format = "%b-%d") - as.Date(start, format = "%b-%d")),
                                                     CO_days.before.end  = as.numeric(as.Date(Date_maxCO, format = "%b-%d") - as.Date(end, format = "%b-%d"))) %>%
                                               mutate(Year = as.character(Year)) %>%
                                               # adorn_totals("row") %>%
                                               bind_rows(summarize(.,Year = "Overal Avg",
                                                                     CT_days.after.start = mean(CT_days.after.start, na.rm = T),
                                                                     CT_days.before.end= mean(CT_days.before.end, na.rm = T),
                                                                     CO_days.after.start= mean(CO_days.after.start, na.rm = T),
                                                                     CO_days.before.end= mean(CO_days.before.end, na.rm = T))) %>%
                                               bind_rows(summarize(.,Year = "St. Dev.",
                                                                   CT_days.after.start = sd(CT_days.after.start),
                                                                   CT_days.before.end= sd(CT_days.before.end),
                                                                   CO_days.after.start= sd(CO_days.after.start),
                                                                   CO_days.before.end= sd(CO_days.before.end)))
     
     length(catch_annual.Table.Spring.calcs$CT_days.before.end[catch_annual.Table.Spring.calcs$CT_days.before.end==0])

## * Table 11. Fall Catch Kable  ----
catch_annual.Table.Fall <- catch_annual.kable.all.seasons %>% 
                            filter(Period == "Fall") %>%
                            select(!Period)

# Catch Plots ----
## * Figure 1. Annual Catch Line Plot  (CO CT) ----
     catch_annual.lineplot <-   ggplot(catch_annual_table, 
                                       aes(x =Year, y = n,color = Species)) +
                                       geom_point()+
                                       geom_line(aes(linetype = Period)) +
                                       labs(y = "Total Captured (# of Fish)", x = NULL) +
                                       scale_x_continuous(breaks = seq(2004,2026,2)) +
                                       theme_bw() +
                                       theme(legend.position = "bottom")     
     
# Total Catch Bar Plots ----
  ## Prepare data
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

## * * Figure 2. Annual Spring Catch Bar Plot ----
       catch_annual_barPlot.Spring <- catch_plots[[1]]

## * * Figure 3. Annual Fall Catch Bar Plot ----
        catch_annual_barPlot.Fall   <- catch_plots[[2]]


        