## Simms Creek Data Sources

## To do list
# Plot total annual CT and CO as line plot, colored by season.   


## Prepare Working Environment ----
    ## Clear workspace ##
    rm(list = ls())  
    
    ## This code is to fix a bug in R 2023.06.0+421 <https://github.com/rstudio/rstudio/issues/13188>
    options(rstudio.help.showDataPreview = FALSE)
    # getOption("rstudio.help.showDataPreview")
    
    ## Load Packages and dependencies. 
    list.of.packages <- c("tidyverse",
                          "purrr",
                          "tidyr",
                          "dplyr",
                          "scales",
                          "lubridate",
                          "reshape2",
                          "ggplot2",
                          "readxl",
                          "openxlsx",
                          "downloadthis",
                          "knitr",
                          "captioner",
                          "zoo",
                          "gridExtra",
                          "plotly",
                          "htmlwidgets",
                          "shiny")
    
    ## Load any missing packages.
        new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
        if(length(new.packages)) install.packages(new.packages)
        
        ## Load Packages
        library(tidyverse)
        library(purrr)
        library(tidyr)
        library(dplyr)
        library(lubridate)
        library(reshape2)
        library(ggplot2)
        library(scales)
        library(readxl)
        library(openxlsx)
        library(downloadthis)
        library(knitr)
        library(captioner)
        library(zoo)
        library(gridExtra)
        library(plotly)
        library(htmlwidgets)
        library(formattable)
        library(xfun)
        library(kableExtra)
        library(shiny)
    
        ##  Set up captioner package calls. 
            fig_nums   <- captioner(prefix = "Figure")  
            table_nums <- captioner(prefix = "Table")
        
        ## Flood Decade
        floor_decade    = function(value){ return(value - value %% 10) }
        
        ## Define Working Directory ---
        setwd("C:/Users/evogt/R Analysis/EAV/GitHubMarkdown/SimmsCreek/docs")
# x- - - - - - - - - - - - - - - - - - - - - - -  ----
# xxx Load Data xxx ----
        ## * Load Bio Data (Juvenile and Adult) ----
        data_sets <- c("Juvenile","Adult")
        
        for(i in data_sets){
          #1.1 List of all files in Directory 
          files_list <- list.files(paste0("Data/",i ,"/"), pattern = "*.xlsx", full.names = TRUE)
          
          # Use lapply to read in all files and store them as list elements in one list
          list_of_dfs <- lapply(as.list(files_list), function(x) readWorkbook(x, sheet = "SimmsBioData"))
          
          # Create a vector of names based on the year of data collection. 
          df_names2 <- paste0(i,"_SimmsBio_", as.numeric(str_extract(files_list, "(\\d)+")))
          
          # Assign the names to our list of dfs
          names(list_of_dfs) <- df_names2
          
          #1.2 Dunno what this does...
          dfs_list <-lapply(list_of_dfs, "[")
          # my_cols <- c("Date", "Species", "Stage", "US/DS", "Length", "Weight", "Comments")
          
          #1.3 Prepare final dataframe
          df <-   bind_rows(dfs_list, .id = "Dataset")
          
          ## Rename dataframe based on looping variable (e.g., Juvenile or Adult)
          assign(paste("simms_bio_",i, sep=""), df) %>%
            mutate(MonitorPeriod = i)
        }

        ### * * Format Juvenile Data ----
        juv <- simms_bio_Juvenile %>%
          mutate(Period = "Spring",
                 Sex = "",
                 DatasetYear = as.numeric(str_extract(Dataset, "(\\d)+")),
                 Date = as.Date(Date, origin = "1899-12-30"),
                 Year = as.numeric(format(Date, format = "%Y"))) %>%
          filter(DatasetYear == Year) %>%                                          # Filter to exclude relic data from past years
          rename(Direction = "US/DS") %>%
          select("Dataset","Period", "Date", "Species", "Stage","Sex", "Direction", "Length", "Weight", "Comments")

      ### * * Format Adult Data ----
      adult <- simms_bio_Adult %>%
        mutate(Period = "Fall",
               Stage = "",
               DatasetYear = as.numeric(str_extract(Dataset, "(\\d)+")),
               Date = as.Date(Date, origin = "1899-12-30"),
               Year = as.numeric(format(Date, format = "%Y"))) %>%
        filter(DatasetYear == Year) %>%                                          # Filter to exclude relic data from past years
        rename(Direction = "US/DS") %>%
        select("Dataset","Period","Date", "Species", "Stage","Sex", "Direction", "Length", "Weight", "Comments")

      ### * * Join and Format Juvenile and Adult Datasets ----
      simms_bio <- rbind(juv,adult)
      ## See unique directions
      # unique(simms_bio$Direction)
      ## See unique species codes
      # unique(simms_bio$Species)

      simms_bio_all <- simms_bio %>%     
        ## Correct entry errors in direction data. Assume that US and UST are equal (and that UST is not fish observed US of trap).    
        mutate(Direction = ifelse(grepl("US", Direction), "Upstream", 
                                  ifelse(grepl("DS", Direction), "Downstream",
                                         ifelse(Period == "Fall",
                                                ifelse(is.na(Direction),"Upstream*",Direction),  # assume all fall fish are moving US
                                                ifelse(is.na(Direction),"Not Collected",Direction)))),
               ## Correct species entry errors 
               Species = recode(as.factor(Species),
                                "CO"  = "CO",
                                "CO " = "CO",
                                " CO" = "CO",
                                "CO-J"= "CO-J",
                                "JK"  = "CO-J",
                                "CT"  = "CT",
                                "CT " = "CT",
                                "CN"  = "CN",
                                "CN " = "CN",
                                "CH"  = "CN",
                                "PK" = "PK"),
               Species = factor(Species, 
                                levels = c("CT","CO", "CO-J","CN","CM","PK")))



# * Load Environmental Data ----
    for(i in data_sets){
      #1.1 List of all files in Directory 
      files_list <- list.files(paste0("Data/",i ,"/"), pattern = "*.xlsx", full.names = TRUE)
      
      # files_list <- list.files(paste0("Data/Adult/"), pattern = "*.xlsx", full.names = TRUE)
      
      # Use lapply to read in all files and store them as list elements in one list
      list_of_dfs <- lapply(as.list(files_list), function(x) readWorkbook(x, sheet = "SimmsCreek", cols = 1:10))
      
      # Create a vector of names based on the year of data collection. 
      df_names2 <- paste0(i,"_SimmsEnv_", as.numeric(str_extract(files_list, "(\\d)+")))
      
      # Assign the names to our list of dfs
      names(list_of_dfs) <- df_names2
      
      #1.2 Dunno what this does...
      # dfs_list <-lapply(list_of_dfs, "[")
      dfs_list <- lapply(list_of_dfs, "[")
      
      
      #1.3 Prepare final dataframe
      df <-   bind_rows(dfs_list, .id = "Dataset") %>%
        mutate(MonitorPeriod = i)
      
      ## Rename dataframe based on looping variable (e.g., Juvenile or Adult)
      assign(paste("simms_env_",i, sep=""), df) 
    }    

    ## * * Rename simms env. variables
    simms_env_Juvenile <- simms_env_Juvenile %>%
      rename(pH = "PH",
             Gauge = "Staff.Gauge")
    
    #### * * Join Env. Datasets ----
    simms_env <- rbind(simms_env_Juvenile,simms_env_Adult) %>%
      mutate(Period = ifelse(MonitorPeriod == "Adult","Fall","Spring"),
             DatasetYear = as.numeric(str_extract(Dataset, "(\\d)+")),
             Date = as.Date(Date, origin = "1899-12-30"),
             Time = format(as.POSIXct(Time), "%I:%M %p"),
             Year = as.numeric(format(Date, format = "%Y")),
             binary = 1) %>%
      filter(DatasetYear == Year)


# * Join Bio and Env Data ----
## * * Prepare Date Sequence  ----
    date_seq <- data.frame(Date = seq(ymd('2008-04-15'), ymd('2024-06-15'), by = 'days')) %>%
      mutate(Month = strftime(Date, format = "%m"),
             Month = as.numeric(Month))
    
    ## * * Join Bio and Env Data to Date Sequence ----
    data_all <- left_join(date_seq,simms_env, by = c("Date"), suffix = c("","_Env")) %>%
      left_join(., simms_bio_all, by = c("Date","Period"), suffix = c("","_Bio")) %>%
      mutate(Year = as.numeric(format(Date, format = "%Y")),
             Period = factor(Period, levels = c("Spring","Fall"))) %>%
      select(Year, Period, Date, Month, Time,    Air.Temp, Water.Temp, 
             pH,   DO,     TDS,  Gauge, Weather, Species, Length, Weight)
    
    xx <- data_all %>% filter(complete.cases(Period))
    
    # write.csv(xx, "SimmsCreekData.csv")
    
# * Add Regional Abundance Data ----  
    reg.dat <- read.csv("Data/RegionalAbundance.csv")  %>%
      filter(region == "Vancouver Island & Mainland Inlets",
             year >= 2005)  
    # write.csv(reg.dat, "RegionalAbundance.csv")
# x- - - - - - - - - - - - - - - - - - - - - - -  ----
# xxx Trapping Effort xxx ----
    
    ## Define periods when trapping did not occur   
    covid <- data.frame(Year = c(2020, 2021, 2023), 
                        Period = c("COVID","COVID", "DFO Strike"),
                        Date   = c(NA,NA,NA),
                        binary = c(0,0,0))
    
## Start and End of Each Trapping Period  
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
trap_effort_table <- trap_effort %>%
                        pivot_wider(names_from = Period, values_from = c(trap_days, start, end)) %>%
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
    spring.trap.plot <- ggplot(trap_plot[trap_plot$Period=="Spring",]) + 
                          geom_col(aes(x= date.std, y = prop.sampled))+
                          labs(x = "", y = "Proportion of Years Sampled (%)") +
                          scale_y_continuous(breaks = seq(0,1,0.2)) +
                          scale_x_date(date_breaks = "1 week",
                                       date_minor_breaks = "1 day",
                                       date_labels = "%b-%d") +
                          # facet_grid(.~Period, scale = "free_x") +
                          theme_bw()

    ## * * Fall Effort Plot ----
    fall.trap.plot <- ggplot(trap_plot[trap_plot$Period=="Fall",]) + 
                          geom_col(aes(x= date.std, y = prop.sampled))+
                          labs(x = "", y = "Proportion of Years Sampled (%)") +
                          scale_y_continuous(breaks = seq(0,1,0.2)) +
                          scale_x_date(date_breaks = "1 week",
                                       date_minor_breaks = "1 day",
                                       date_labels = "%b-%d") +
                          # facet_grid(.~Period, scale = "free_x") +
                          theme_bw()


# x- - - - - - - - - - - - - - - - - - - - - - -  ----
# xxx    Annual Stream Conditions xxx ----
## * Conditions Table ----
cond_table <- data_all %>% 
                group_by(Year,Period, Date)%>%
                summarise(Water.Temp = mean(Water.Temp, na.rm = TRUE),
                          pH         = mean(pH, na.rm = TRUE),
                          Gauge      = mean(Gauge, na.rm = TRUE)) %>%
                group_by(Year,Period) %>%
                summarise(Water.Temp_Avg = mean(Water.Temp, na.rm = TRUE),
                          Water.Temp_SD  = sd(Water.Temp, na.rm = TRUE),
                          pH_Avg         = mean(pH, na.rm = TRUE),
                          pH_SD          = sd(pH, na.rm = TRUE),
                          Gauge_Avg      = mean(Gauge, na.rm = TRUE),
                          Gauge_SD       = sd(Gauge, na.rm = TRUE)) %>%
                filter(!is.na(Period))

      # * * Wide Conditions Table ----
      cond_table_wide <- cond_table %>%
                pivot_wider(names_from = Period, 
                            values_from = c(Water.Temp_Avg, Water.Temp_SD, 
                                                                 pH_Avg, pH_SD, Gauge_Avg, Gauge_SD)) %>%
                select(Year, 
                       Water.Temp_Avg_Spring, Water.Temp_SD_Spring,pH_Avg_Spring, pH_SD_Spring, 
                       Gauge_Avg_Spring, Gauge_SD_Spring,
                       Water.Temp_Avg_Fall, Water.Temp_SD_Fall, pH_Avg_Fall, pH_SD_Fall, Gauge_Avg_Fall, 
                       Gauge_SD_Fall)

      ## * * Long Conditions Table ----
      cond_long <- cond_table %>% 
                   select(Year, Period, Water.Temp_Avg,pH_Avg,Gauge_Avg) %>%
                   pivot_longer(c(Water.Temp_Avg,pH_Avg,Gauge_Avg), 
                                 names_to = "Parameter", 
                                 values_to = "value") %>%
                   mutate(ParameterName = ifelse(Parameter == "Water.Temp_Avg", 
                                                  "Water Temp. (\u00B0C)",
                                                  ifelse(Parameter == "Gauge_Avg", "Gauge (m)","pH")))

      ## * Conditions Plot ----
      cond_plot <- ggplot(cond_long, 
                        aes(x = Year, y = value, 
                            color = Period)) +
                   geom_line() +
                   scale_x_continuous(breaks = seq(2004,2026,2)) +
                   facet_wrap(ParameterName~., 
                              scales = "free", 
                              strip.position = "left",
                              ncol = 1) +
                   labs(y = NULL, x = NULL) +
                   theme_bw() +
                   theme(strip.background = element_blank(),
                         strip.placement = "outside",
                         legend.position = "bottom") 
                       

      
      

# x- - - - - - - - - - - - - - - - - - - - - - -  ----
# xxx    Annual Captures xxx ----
# Catch Tables ----
## * Basic Table (#, FL, date by spp and season) ---- 
catch_table <- data_all %>%  
                  filter(Species %in% c("CT","CO")) %>%
                  group_by(Year, Period, Species) %>%
                  summarize(n = n(),
                            n = coalesce(n,0),
                            Date_min = min(Date, na.rm=T),
                            Date_med = median(Date, na.rm=T),
                            Date_max = max(Date, na.rm=T),
                            FL_mean  = mean(Length, na.rm = T),
                            FL_SD    = sd(Length, na.rm = T)) 

        # * * Wide Basic Catch Table ----
        catch_table_wide <- catch_table %>%
                  pivot_wider(names_from =Period,
                              values_from = c(n, Date_min,Date_med, Date_max, FL_mean,FL_SD)) %>%
                  select(Year, Species, 
                         n_Spring, Date_min_Spring, Date_med_Spring, Date_max_Spring, FL_mean_Spring, FL_SD_Spring,
                         n_Fall, Date_min_Fall, Date_med_Fall, Date_max_Fall, FL_mean_Fall, FL_SD_Fall)


## * Basic Catch Plot
catch_plot.basic <- ggplot(catch_table, aes(x =Year, y = n,color = Species, shape = Period)) +
                        geom_point()+
                        geom_line(aes(linetype = Period))


## * Detailed Tables ----

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
      
      ## Create DFs named "catch_CT" and "catch_CO"
      assign(paste0("catch_", i, sep = ""), x)
      }

trap_effort2 <- trap_effort %>% mutate(start = as.Date(start, "%b-%d"),
                                       end = as.Date(end,"%b-%d"))
## * * All Seasons Catch Table ----
TABLE_catch_summary.All <- left_join(trap_effort2, catch_CT, 
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
        TABLE_catch_summary.Spring <- TABLE_catch_summary.All %>% filter(Period == "Spring") %>%
          select(!Period)
      
      ## * * Fall Catch Table ----
      TABLE_catch_summary.Fall <- TABLE_catch_summary.All %>% filter(Period == "Fall") %>%
        select(!Period)

# Catch Plots ----
# * Total Annual Catch Plot ----
## Prepare data
   ## Define sampling periods
      catch_period <- c("Spring", "Fall")
   ## Create empty list to save plots to
      catch_plots <- list()

## Run for loop      
    for(i in catch_period){
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
                   labs(x = "", y= "Total Captures (# of Fish)") +
                   scale_x_continuous(breaks = seq(2008,2024,2)) +
                   theme_bw()
          
          # assign name to plot 
          catch_plots[[i]] <- g        
                
    }
      
    ## * * Annual Catch Plot - Spring ----
     catch_plot.Total.Spring <- catch_plots[[1]]
    
    ## * * Annual Catch Plot - Fall ----
      catch_plot.Total.Fall   <- catch_plots[[2]]

# * Annual Catch Timing Plots ----
## Prep Labels for Plot
labels <- data.frame(Year = unique(data_all$Year), YYYEAR = unique(data_all$Year)) %>%
                  # data.frame(Year = unique(CCT_catch$Year), YYYEAR = unique(CCT_catch$Year)) %>%
                  mutate(YYYEAR = ifelse(Year == 2020, "2020 - COVID",
                                      ifelse(Year == 2021,"2021 - COVID",
                                         ifelse(Year == 2023, "2023 - Strike",Year))),
                  labX = as.Date(ifelse(Year == 2020, as.Date("2024-05-25"),
                                    ifelse(Year == 2021,as.Date("2024-05-25"),
                                       ifelse(Year == 2023, as.Date("2024-05-25"),as.Date("2024-04-16"))))))
      
fall.labels <- data.frame(Year = unique(data_all$Year), YYYEAR = unique(data_all$Year)) %>%
        # data.frame(Year = unique(CCT_catch$Year), YYYEAR = unique(CCT_catch$Year)) %>%
        mutate(YYYEAR = ifelse(Year == 2020, "2020 - COVID",
                               ifelse(Year == 2021,"2021 - COVID",Year)),
               labX = as.Date(ifelse(Year == 2020, as.Date("2024-10-01"),
                                     ifelse(Year == 2021,as.Date("2024-10-25"),as.Date("2024-10-01")))))      
      
## Prep Data for plots
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
                                                 y = 4,
                                                 hjust = "left", vjust = 0,
                                                 inherit.aes = FALSE) +
                                      scale_y_continuous("Total Captures (# of Fish)",
                                                         breaks = seq(0,10,5),
                                                         sec.axis = sec_axis(~./10, 
                                                         name = "Proportion of Total Catch (%)"))   

        ## * * Fall CO Timing Plot ----
        catch_timing_plot.Fall.CO <- catch_timing_plots[[2]][[1]] +
                                        geom_label(data = labels, 
                                                   aes(label = YYYEAR),
                                                   x = as.Date("2024-10-15"), 
                                                   y = 125,
                                                   hjust = "left", vjust = 0,
                                                   inherit.aes = FALSE)

