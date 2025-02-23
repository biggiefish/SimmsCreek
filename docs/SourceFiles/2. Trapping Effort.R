##____________________________________________________________________________________________________________________________----
# 2. Trapping Effort ----
## 

## - Effort Table - shows start and end dates of spring and fall sampling, plus total number of sampling days per season.
## - Effort Plot - shows proportion of years each day was sampled.

##___________________________________________________________________________________________________________________________----

## Confirm data is loaded 
ifelse(exists('data_all') && is.data.frame(get('data_all')), 
       " ", 
       source("SourceFiles/1. Load and Prep Data.R"))


## Define periods when trapping did not occur   
covid <- data.frame(Year = c(2020, 2021, 2023), 
                    Period = c("COVID","COVID", "DFO Strike"),
                    Date   = c(NA,NA,NA),
                    binary = c(0,0,0))

## Start and End of Each Trapping Period  
    trap_effort1 <- data_all %>% 
                      group_by(Year, Period, Date) %>%
                      summarize(binary = 1) %>%
                      ungroup() %>%
                      rbind(covid) %>%
                      group_by(Year, Period) %>%
                      summarize(trap_days = sum(binary),
                                start = min(Date),
                                end = max(Date)) %>%
                                # start = format(min(Date), "%b-%d"),
                                # end   = format(max(Date),"%b-%d"))
                      filter(complete.cases(Period))
    
    trap_effort <- data_all %>% 
                        group_by(Year, Period, Date) %>%
                        summarize(binary = 1) %>%
                        ungroup() %>%
                        rbind(covid) %>%
                        group_by(Year, Period) %>%
                        summarize(trap_days = sum(binary),
                                  start = format(min(Date), "%b-%d"),
                                  end   = format(max(Date),"%b-%d")) %>%
                        filter(complete.cases(Period))

## Effort Table ----
    trap_effort_summary_table <- trap_effort1 %>%
                                    filter(Period %in% c("Spring","Fall")) %>%
                                    group_by(Period) %>%
                                    mutate(start.std = case_when(year(start) >= 0 ~ 'year<-'(start, 2024)),
                                           end.std   = case_when(year(end) >= 0 ~ 'year<-'(end, 2024))) %>%
                                    summarise(Days_min = min(trap_days, na.rm = TRUE),
                                              Days_max = max(trap_days, na.rm = TRUE),
                                              Days_avg = mean(trap_days, na.rm = TRUE),
                                              Days_sd  = sd(trap_days, na.rm = TRUE),
                                              Start_min = format(min(start.std, na.rm = TRUE), "%d-%b"),
                                              Start_max = format(max(start.std, na.rm = TRUE), "%d-%b"),
                                              Start_avg = format(mean(start.std, na.rm = TRUE), "%d-%b"),
                                              Start_sd  = sd(start.std, na.rm = TRUE),
                                              End_min = format(min(end.std, na.rm = TRUE), "%d-%b"),
                                              End_max = format(max(end.std, na.rm = TRUE), "%d-%b"),
                                              End_avg = format(mean(end.std, na.rm = TRUE), "%d-%b"),
                                              End_sd  = sd(end.std, na.rm = TRUE))
    
    
    trap_effort_table <- trap_effort %>%
                          pivot_wider(names_from = Period, 
                                      values_from = c(trap_days, start, end)) %>%
                          select(Year, trap_days_Spring, start_Spring, 
                                 end_Spring,trap_days_Fall, start_Fall, end_Fall) %>%
                          # Add rationales for not trapping
                          mutate(start_Spring = ifelse(c(Year == 2020 | Year == 2021), "COVID",
                                                       ifelse(Year == 2023, "DFO Strike",start_Spring)),
                                 trap_days_Spring = coalesce(trap_days_Spring,0),
                                 trap_days_Fall   = coalesce(trap_days_Fall,0)) 

## Effort Plot ---- 
# Prepare Plot Data
trap_plot <- data_all %>%
              mutate(date.std = case_when(year(Date) >= 0 ~ 'year<-'(Date, 2024))) %>%
              drop_na(Period) %>%
              group_by(Year, Period, date.std) %>%
              summarize(binary = 1) %>%
              ungroup() %>%
              group_by(Period,date.std) %>%
              summarize(n = sum(binary),         # number years where date was sampled.
                        nyears = 14,             # number years sampling has occurred.
                        prop.sampled  = n/nyears)        # calculate proportion of years that trapped on each date

## * * Spring Effort Plot ----
trap_effort_plot_spring <- ggplot(trap_plot[trap_plot$Period=="Spring",]) + 
                              geom_col(aes(x= date.std, y = prop.sampled))+
                              labs(x = "", y = "Proportion of Years Sampled (%)") +
                              scale_y_continuous(breaks = seq(0,1,0.2)) +
                              scale_x_date(date_breaks = "1 week",
                                           date_minor_breaks = "1 day",
                                           date_labels = "%b-%d") +
                              # facet_grid(.~Period, scale = "free_x") +
                              theme_bw()

## * * Fall Effort Plot ----
trap_effort_plot_fall <- ggplot(trap_plot[trap_plot$Period=="Fall",]) + 
                            geom_col(aes(x= date.std, y = prop.sampled))+
                            labs(x = "", y = "Proportion of Years Sampled (%)") +
                            scale_y_continuous(breaks = seq(0,1,0.2)) +
                            scale_x_date(date_breaks = "1 week",
                                         date_minor_breaks = "1 day",
                                         date_labels = "%b-%d") +
                            # facet_grid(.~Period, scale = "free_x") +
                            theme_bw()
    