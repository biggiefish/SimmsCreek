## Spawning occuring before fence is installed?? Largest fish are observed during during fall
max.fish  <- data_all %>% 
                filter(Species == "CT") %>%
                group_by(Year,Period) %>%
                summarise(MaxFL = max(Length)) %>%
                pivot_wider(names_from = Period,values_from = MaxFL)





## Prepare Function
coalesce_by_column <- function(df) {
                          return(coalesce(df[1], df[2]))
                          }
## Prepare Data
move.dat   <- data_all %>%
                filter(Period == "Spring",
                       Species == "CT",
                       Length >125) 

## Calculate Date Range fish of each length moved upstream and downstream
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

## Join Movmenet Dates to All Spring Data
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
                     
              ggplot(plot.move)+
                  geom_point(aes(x=date.std, y = Length))  
                
                  
                
####________________________________________________________________________________________________-----



  
  
  