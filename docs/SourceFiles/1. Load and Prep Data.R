## Load All Simms Creek Data

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
    data_bio_spring <- simms_bio_Juvenile %>%
                    mutate(Period = "Spring",
                           Sex = "",
                           DatasetYear = as.numeric(str_extract(Dataset, "(\\d)+")),
                           Date = as.Date(Date, origin = "1899-12-30"),
                           Year = as.numeric(format(Date, format = "%Y"))) %>%
                    filter(DatasetYear == Year) %>%                                          # Filter to exclude relic data from past years
                    rename(Direction = "US/DS") %>%
                    select("Dataset","Period", "Date", "Species", "Stage","Sex", "Direction", "Length", "Weight", "Comments")
        
    ### * * Format Adult Data ----
    data_bio_fall <- simms_bio_Adult %>%
                  mutate(Period = "Fall",
                         Stage = "",
                         DatasetYear = as.numeric(str_extract(Dataset, "(\\d)+")),
                         Date = as.Date(Date, origin = "1899-12-30"),
                         Year = as.numeric(format(Date, format = "%Y"))) %>%
                  filter(DatasetYear == Year) %>%                                          # Filter to exclude relic data from past years
                  rename(Direction = "US/DS") %>%
                  select("Dataset","Period","Date", "Species", "Stage","Sex", "Direction", "Length", "Weight", "Comments")
    
    ### * * Join and Format Juvenile and Adult Datasets ----
        ## Join Spring and Fall Bio data
        simms_bio <- rbind(data_bio_spring,data_bio_fall)


    data_all_bio <- simms_bio %>%     
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
                  left_join(., data_all_bio, by = c("Date","Period"), suffix = c("","_Bio"), relationship = "many-to-many") %>%
                  mutate(Year = as.numeric(format(Date, format = "%Y")),
                         Period = factor(Period, levels = c("Spring","Fall"))) %>%
                  select(Year, Period, Date, Month, Time,    Air.Temp, Water.Temp, 
                         pH,   DO,     TDS,  Gauge, Weather, Direction, Species, Length, Weight, Comments)
                
    xx <- data_all %>% filter(complete.cases(Period))
    
    # write.csv(xx, "SimmsCreekData.csv")
    
# x- - - - - - - - - - - - - - - - - - - - - - -  ----   
# Add External Data     ----
    
# * Add Regional Abundance Data ----  
    data_regional <- read.csv("Data/RegionalAbundance.csv")  %>%
      filter(region == "Vancouver Island & Mainland Inlets",
             year >= 2005)  
    
# * Add Local Climate Data ----
    ## * * Load Daily Climate Data ----
    ## list all files in directory
    all_daily_files <- list.files("Data/Climate/Daily/", pattern = '\\.csv$', full.names = TRUE)
    
    ## ID Columns to Import
    cols <- c("STATION_NAME", "LOCAL_DAY","LOCAL_YEAR","LOCAL_MONTH", "LOCAL_DATE",
              "TOTAL_RAIN","TOTAL_PRECIPITATION","MIN_TEMPERATURE","MAX_TEMPERATURE","MEAN_TEMPERATURE")
    
    ## Load all files in directory and select specific columns
    wx.daily.data <- map_df(all_daily_files,
                            ~.x %>% readr::read_csv() %>% select(cols), id = 'filenum')
    
    ## * * Load Hourly Climate Data ----
    ## list all files in directory
    all_hr_files <- list.files("Data/Climate/Hourly/", pattern = '\\.csv$', full.names = TRUE)
    
    ## ID Columns to Import
    cols <- c("Station.Name", "Year","Month", "Date.Time..LST." , "Day", "Time..LST." , "Temp...C.",
              "Precip..Amount..mm.", "Stn.Press..kPa.", "Hmdx", "Wind.Chill", "Weather")
    
    ## Load all files in directory 
    wx.hr.data <- all_hr_files %>%
      ## Convert all inputs to character format
      map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
      mutate(DateTime      = ymd_hms(`Date/Time (LST)`),
             Date          = as.Date(DateTime),
             Temp          = as.numeric(as.character(`Temp (°C)`)),
             Rel.Humid     = as.numeric(as.character(`Rel Hum (%)`)),
             WindSpeed.kmh = as.numeric(as.character(`Wind Spd (km/h)`)),
             Baro.kPa      = as.numeric(as.character(`Stn Press (kPa)`))) %>%
      select(StationName = "Station Name", Date, Temp, Rel.Humid, WindSpeed.kmh, Baro.kPa, Weather)
    
    wx.hr.summary <- wx.hr.data %>%
                      group_by(StationName,Date) %>%
                      summarize(Temp_Avg          = mean(Temp, na.rm = TRUE),
                                Temp_SD           = sd(Temp, na.rm = TRUE),
                                Rel.Hum_Avg       = mean(Rel.Humid, na.rm=TRUE),
                                Rel.Hum_SD        = sd(Rel.Humid, na.rm=TRUE),
                                Windspeed.kmh_Avg = mean(WindSpeed.kmh, na.rm=TRUE),
                                Windspeed.kmh_SD  = sd(WindSpeed.kmh, na.rm=TRUE),
                                Baro.kPa_Avg      = mean(Baro.kPa, na.rm=TRUE),
                                Baro.kPa_SD       = sd(Baro.kPa, na.rm=TRUE))
                
                
                
      
    
    
