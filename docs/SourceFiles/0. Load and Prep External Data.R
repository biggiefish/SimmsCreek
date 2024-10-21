# External Data Sources

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


# Add Regional Abundance Data ----  
data_regional <- read.csv("Data/RegionalAbundance.csv")  %>%
                  filter(region == "Vancouver Island & Mainland Inlets",
                         year >= 2005)  

# Add Local Climate Data ----
    ## Load Daily Climate Data
          ## list all files in directory
          all_daily_files <- list.files("Data/Climate/Daily/", pattern = '\\.csv$', full.names = TRUE)
          
          ## ID Columns to Import
             cols <- c("STATION_NAME", "LOCAL_DAY","LOCAL_YEAR","LOCAL_MONTH", "LOCAL_DATE",
                       "TOTAL_RAIN","TOTAL_PRECIPITATION","MIN_TEMPERATURE","MAX_TEMPERATURE","MEAN_TEMPERATURE")
             
          ## Load all files in directory and select specific columns
              wx.daily.data <- map_df(all_daily_files,
                                ~.x %>% readr::read_csv() %>% select(cols), id = 'filenum')
          
    ## Load Hourly Climate Data
          ## list all files in directory
          all_hr_files <- list.files("Data/Climate/Hourly/", pattern = '\\.csv$', full.names = TRUE)
          
          ## ID Columns to Import
          cols <- c("Station.Name", "Year","Month", "Date.Time..LST." , "Day", "Time..LST." , "Temp...C.",
                    "Precip..Amount..mm.", "Stn.Press..kPa.", "Hmdx", "Wind.Chill", "Weather")

          ## Load all files in directory 
          wx.hr.data <- all_hr_files %>%
            ## Convert all inputs to character format
            map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
                mutate(DateTime = ymd_hms(`Date/Time (LST)`),
                       Temp     = as.numeric(as.character(`Temp (°C)`)),
                       Rel.Humid = as.numeric(as.character(`Rel Hum (%)`)),
                       WindSpeed.kmh = as.numeric(as.character(`Wind Spd (km/h)`)),
                       Baro.kPa  = as.numeric(as.character(`Stn Press (kPa)`))) %>%
                select(StationName = "Station Name", Year, Month, Day, DateTime, Temp, Rel.Humid, WindSpeed.kmh, Baro.kPa, Weather)


