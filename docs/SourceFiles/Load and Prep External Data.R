# External Data Sources


# Add Regional Abundance Data ----  
data_regional <- read.csv("Data/RegionalAbundance.csv")  %>%
                  filter(region == "Vancouver Island & Mainland Inlets",
                         year >= 2005)  

# Local Climate Data ----
    ## Load Daily Climate Data
          ## list all files in directory
          all_daily_files <- list.files("Data/Climate/Daily/", pattern = '\\.csv$', full.names = TRUE)
          
          ## ID Columns to Import
             cols <- c("STATION_NAME", "LOCAL_DAY","LOCAL_YEAR","LOCAL_MONTH", "LOCAL_DATE",
                       "TOTAL_RAIN","TOTAL_PRECIPITATION","MIN_TEMPERATURE","MAX_TEMPERATURE","MEAN_TEMPERATURE")
             
          ## Load all files in directory and select specific columns
          wx.daily.data <- map_df(all_daily_files,
                            ~.x %>% readr::read_csv() %>%select(cols), id = 'filenum')
          
    ## Load Hourly Climate Data
          ## list all files in directory
          all_hr_files <- list.files("Data/Climate/Hourly/", pattern = '\\.csv$', full.names = TRUE)
          
          ## ID Columns to Import
          cols <- c("Station.Name", "Year","Month", "Date.Time..LST." , "Day", "Time..LST." , "Temp...C.",
                    "Precip..Amount..mm.", "Stn.Press..kPa.", "Hmdx", "Wind.Chill", "Weather")

          ## Load all files in directory and select specific columns
          wx.hr.data <- map_df(all_hr_files,
                            ~.x %>% readr::read_csv() %>%select(cols), id = 'filenum')          
          
          read.csv("Data/Climate/Hourly/en_climate_hourly_BC_1021267_11-2023_P1H.csv")
          colnames(read.csv("Data/Climate/Hourly/en_climate_hourly_BC_1021267_11-2023_P1H.csv"))


