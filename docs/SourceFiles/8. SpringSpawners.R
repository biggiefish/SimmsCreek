##____________________________________________________________________________________________________________________________----
## 8. Spring Spawners
# 
#  Are spawning CCT detected during spring surveys? 
#
##____________________________________________________________________________________________________________________________----

## Confirm data is loaded 
    ifelse(exists('data_all') && is.data.frame(get('data_all')), 
           " ", 
           source("SourceFiles/1. Load and Prep Data.R"))


## Prepare Data
    move.dat   <- data_all %>%
      filter(Period == "Spring",
             Species == "CT",
             Length >125) 

    ## For every unique fork length, calculate first and last observation date of upstream and downstream movements 
    movement.dates <- move.dat %>%
                        group_by(Year,Length,Direction) %>%
                        summarise(n.fish = n(),
                                  Date_min = min(date.std),
                                  Date_max = min(date.std)) %>%
                        pivot_wider(names_from = "Direction", values_from = c("Date_min", "Date_max")) %>%
                        select(Year, 
                               Length, 
                               n.fish,
                               "US_minDate" = Date_min_Upstream, 
                               "US_maxDate" = Date_max_Upstream,
                               "DS_minDate" = Date_min_Downstream ,
                               "DS_maxDate" = Date_max_Downstream) %>%
                        ungroup() %>%
                        group_by(Year,Length) %>%
                        summarise_all(coalesce_by_column)
    
    ## Join Movement Dates to All Spring Data (has one row per fish) and test whether fish could be a possible spawner (detected moving downstream after a fish of equal size moved upstream).
    plot.move <- move.dat %>%  
                  left_join(movement.dates, by = c("Year", "Length")) %>%
                  select(Year, Period, date.std, Direction, Length, n.fish, US_minDate, US_maxDate, DS_minDate, DS_maxDate) %>%
                  filter(complete.cases(US_minDate)) %>%
                  mutate(FilterOut = ifelse(Direction == "Upstream", 
                                            "Keep | Fish moving upstream",
                                            ifelse(Direction == "Downstream" & date.std >= US_minDate,
                                                   "Keep | Fish moved DS after fish of equal size moved upstream",
                                                   ifelse(Direction == "Downstream" & date.std < US_minDate, 
                                                          "Remove | Fish moved DS before any fish of equal size moved upstream",
                                                          "Remove | No fish of this size moved upstream")))) %>%
                  separate_wider_delim(FilterOut, delim = " | ", names = c("FilterAction", "Rationale")) %>%
                  filter(FilterAction == "Keep") 
              
# Potential Spawner Plot ----

    # Create Empty List
    spawn.plot <- list()
    
    # Run for loop
    for(i in spring_years){
      
      ## Prep data
      plot.dat <- plot.move %>% 
                      filter(Year    == i,
                             Period  == "Spring",
                             Length  >  125)
      
          ## Establish Plot Boundaries
              FL_min <- rounder(min(plot.move$Length))-10
              FL_max <- rounder(max(plot.move$Length))+10
              seqby <- rounder((FL_max - FL_min)/10)
      
      ## Create Plot
      temp.plot <- ggplot(plot.dat) + 
                      geom_point(aes(x = date.std, 
                                     y = Length, 
                                     color = Direction),
                                 shape = 1,
                                 alpha = 0.7) + 
                      scale_y_continuous(breaks = seq(FL_min, FL_max,seqby)) +
                      scale_x_date(breaks = "5 days", 
                                   limits = c(as.Date("2024-04-01"),as.Date("2024-06-15")),
                                   date_labels = "%b-%d")+
                      scale_shape(solid = FALSE)+
                      labs(x = "Passage Date", y = "Fork Length (mm)") +
                      facet_grid(.~Year)+
                      theme_bw() +
                      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))
      ## Save plots  
      spawn.plot[[i]] <- temp.plot
      
    }
    
    spawn_2008 <- spawn.plot[[1]]
    spawn_2009 <- spawn.plot[[2]]
    spawn_2010 <- spawn.plot[[3]]
    spawn_2011 <- spawn.plot[[4]]
    spawn_2012 <- spawn.plot[[5]]

