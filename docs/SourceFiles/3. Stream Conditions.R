##____________________________________________________________________________________________________________________________----
# 3. Stream Conditions ----
#
# - Conditions Tables (long and wide format) - show mean +/- SD of AT, WT, pH, DO, TDS and Stage Level per season.
# - Conditions Plot - Mean annual conditions during spring and fall sampling (faceted by parameter). 
# - WQ Guidelines Table - Critical thresholds from WQ guidelines for each parameter. 
#
##____________________________________________________________________________________________________________________________----


## Confirm data is loaded 
ifelse(exists('data_all') && is.data.frame(get('data_all')), 
       " ", 
       source("SourceFiles/1. Load and Prep Data.R"))

library(rMR)
# https://search.r-project.org/CRAN/refmans/rMR/html/DO.unit.convert.html
# convert DO % Sat to mg/L


# Prepare Conditions Data ----
cond_dat <- left_join(data_all, wx.hr.summary, by = "Date")

## * Table 1. Condition Summary ----
cond_data <- cond_dat %>% 
             group_by(Year,Period, Date)%>%
                ## Check for DO values >20. If DO >20, convert to mg/L (from % Saturation)
                mutate(DO = ifelse(DO<20,DO,
                                   DO.unit.convert(DO,"pct","mg/L","kpa",Baro.kPa_Avg, Water.Temp))) %>%
                summarise(Air.Temp   = mean(Air.Temp, na.rm = TRUE),
                          Water.Temp = mean(Water.Temp, na.rm = TRUE),
                          pH         = mean(pH, na.rm = TRUE),
                          DO         = mean(DO, na.rm = TRUE),
                          TDS        = mean(TDS, na.rm = TRUE),
                          Gauge      = mean(Gauge, na.rm = TRUE)) %>%
                group_by(Year,Period) %>%
                summarise(Air.Temp_Avg   = mean(Air.Temp, na.rm = TRUE),
                          Air.Temp_SD    = sd(Air.Temp, na.rm = TRUE),
                          Water.Temp_Avg = mean(Water.Temp, na.rm = TRUE),
                          Water.Temp_SD  = sd(Water.Temp, na.rm = TRUE),
                          pH_Avg         = mean(pH, na.rm = TRUE),
                          pH_SD          = sd(pH, na.rm = TRUE),
                          DO_Avg         = mean(DO, na.rm = TRUE),
                          DO_SD          = sd(DO, na.rm = TRUE),
                          TDS_Avg        = mean(TDS, na.rm = TRUE),
                          TDS_SD         = sd(TDS, na.rm = TRUE),
                          Gauge_Avg      = mean(Gauge, na.rm = TRUE),
                          Gauge_SD       = sd(Gauge, na.rm = TRUE)) %>%
                filter(!is.na(Period))

        ## * * Table 2. Condition Summary (Wide Format) ----
        cond_data_wide <- cond_data %>%
                          pivot_wider(names_from = Period, 
                                      values_from = c(Air.Temp_Avg, Air.Temp_SD, 
                                                      Water.Temp_Avg, Water.Temp_SD, 
                                                      pH_Avg, pH_SD, 
                                                      DO_Avg, DO_SD, 
                                                      TDS_Avg, TDS_SD,
                                                      Gauge_Avg, Gauge_SD)) %>%
                          select(Year, 
                                 Air.Temp_Avg_Spring, Air.Temp_SD_Spring, 
                                 Water.Temp_Avg_Spring, Water.Temp_SD_Spring,
                                 pH_Avg_Spring, pH_SD_Spring, 
                                 DO_Avg_Spring, DO_SD_Spring, 
                                 TDS_Avg_Spring, TDS_SD_Spring, 
                                 Gauge_Avg_Spring, Gauge_SD_Spring,
                                 Air.Temp_Avg_Fall, Air.Temp_SD_Fall, 
                                 Water.Temp_Avg_Fall, Water.Temp_SD_Fall,
                                 pH_Avg_Fall, pH_SD_Fall, 
                                 DO_Avg_Fall, DO_SD_Fall, 
                                 TDS_Avg_Fall, TDS_SD_Fall, 
                                 Gauge_Avg_Fall, Gauge_SD_Fall)
        
        
        param_names <- c("Air Temp. (\u00B0C)", "Water Temp. (\u00B0C)","pH","DO (mg/L or % Sat.)","TDS (ppm)","Stage Level (m)")
        
        ## * * Table 3. Conditions Summary (Long Format) ----
        cond_data_long <- cond_data %>% 
                          select(Year, Period, Air.Temp_Avg, Water.Temp_Avg,pH_Avg,DO_Avg, TDS_Avg, Gauge_Avg) %>%
                          pivot_longer(c(Air.Temp_Avg, Water.Temp_Avg,pH_Avg,DO_Avg, TDS_Avg, Gauge_Avg), 
                                       names_to = "Parameter", 
                                       values_to = "value") %>%
                          mutate(ParameterName = str_replace_all(Parameter, 
                                                             c("Air.Temp_Avg"   = "Air Temp. \n(\u00B0C)",
                                                               "Water.Temp_Avg" = "Water Temp. \n(\u00B0C)",
                                                               "pH_Avg"         = "pH",
                                                               "DO_Avg"         = "DO \n(mg/L or % Sat.)",
                                                               "TDS_Avg"        = "TDS \n(ppm)",
                                                               "Gauge_Avg"      = "Water Level \n(m)" )),
                                 ParameterName = factor(ParameterName, levels = c("Air Temp. \n(\u00B0C)",
                                                                                  "Water Temp. \n(\u00B0C)",
                                                                                  "pH",
                                                                                  "DO \n(mg/L or % Sat.)",
                                                                                  "TDS \n(ppm)",
                                                                                  "Water Level \n(m)")))

## Prepare Figures ----
# * Figure 1. Conditions Plot ----
cond_plot <- ggplot(cond_data_long, 
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


# Water Quality Guidelines ----
#BC WQ Guidlines 
#  - https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/wqg_summary_aquaticlife_wildlife_agri.pdf
# Levy et al. 1993
#  - https://www2.gov.bc.ca/assets/gov/environment/natural-resource-stewardship/nr-laws-policy/risc/background/habitat_capacity_for_salmon_spawning_and_rearing.pdf 
# Carter 2005
#  - https://www.noaa.gov/sites/default/files/legacy/document/2020/Oct/07354626438.pdf 

## * Table 4. Water Quality Guidelines ----
WQ_Guidelines <- data.frame(pH_val = c("<6.5",
                                       "6.5 to 8.5",
                                       ">9.0"),
                            pH_desc = c("Acidic", 
                                        "Preferred",
                                        "Alkaline"),
                            pH_source = c("BC WQ Guidelines for Aquatic Life",
                                          "BC WQ Guidelines for Aquatic Life",
                                          "BC WQ Guidelines for Aquatic Life"),
                            Water.Temp.valCO = c("<1.7",
                                                 "12 to 14", 
                                                 ">26"),
                            Water.Temp.valCT = c("<0.6",
                                                 "14 to 16", 
                                                 ">22.8"),
                            Water.Temp.Desc = c("Lower Lethal Limit", 
                                                "Preferred",
                                                "Upper Lethal Limit"),
                            Water.Temp.Source = c("Levy et al. 1993",
                                                  "Levy et al. 1993",
                                                  "Levy et al. 1993"),
                            DO.val   = c("<3 mg/L",
                                         "<6 mg/L",
                                         ">8 mg/L"),
                            DO.desc  = c("Lethal Limit",
                                         "Oxygen Deprived",
                                         "Preferred DO Concentration"),
                            DO.source = c("Carter 2005",
                                          "Levy et al. 1993",
                                          "Levy et al. 1993"))



