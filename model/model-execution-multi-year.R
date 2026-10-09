# ------------------------------------------------------------------------------- Laith --------------------------------

library(knitr)
library(tidyverse)
library(odbc)
library(DBI)
library(glue)
library(dplyr)
library(tidyr)
library(dbplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(openxlsx)
library(readxl)
library(rmarkdown)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(timeDate)
library(forecast)

accuracy <- fabletools::accuracy

# -------------------------------------------------------------------------------- Constants --------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# Load Baseline Data
baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
    LOC_NAME = case_when(
      LOC_NAME == 'THE MOUNT SINAI HOSPITAL' ~ 'MSH',
      LOC_NAME == 'MOUNT SINAI QUEENS'       ~ 'MSQ',
      LOC_NAME == 'MOUNT SINAI BROOKLYN'     ~ 'MSB',
      LOC_NAME == 'MOUNT SINAI BETH ISRAEL'  ~ 'MSBI',
      LOC_NAME == 'MOUNT SINAI MORNINGSIDE'  ~ 'MSM',
      LOC_NAME == 'MOUNT SINAI WEST'         ~ 'MSW',
      TRUE ~ LOC_NAME),
    FACILITY_MSX = case_when(
      FACILITY_MSX == "BIB" ~ "MSB",
      FACILITY_MSX == "BIP" ~ "MSBI",
      FACILITY_MSX == "RVT" ~ "MSW",
      FACILITY_MSX == "STL" ~ "MSM",
      TRUE ~ FACILITY_MSX))

#pool NA SERVICE_GROUP vals as "Other"
baseline <- baseline %>%
  mutate(
    SERVICE_GROUP = if_else(
      is.na(SERVICE_GROUP),
      "Other",
      SERVICE_GROUP
    )
  )

baseline <- baseline %>%
  mutate(
    LOC_NAME = case_when(
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSH" ~ "MSH",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSQ" ~ "MSQ",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSBI" ~ "MSBI",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSB" ~ "MSB",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSM" ~ "MSM",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSW" ~ "MSW",
      TRUE ~ LOC_NAME
    )
  )
#  --------------------------------------------------------------------------- Functions ------------------------------------


source("functions/location_swap.R")
source("functions/emergency_exclusion.R")
source("functions/los_adjustment.R")
source("functions/unit_capacity.R")
source("functions/excel_add_to_wb.R")
source("functions/save_parameters.R")
source("functions/volume_projections.R")
source("functions/dow_service_group.R")
source("functions/dow_unit.R")
source("functions/excel_add_to_wb_dow.R")
source("functions/NA_cleanup.R")
source("functions/daily_demand.R")

# execute ip utiliziation script
source("model/model-ip-utilization.R")


# ------------------------------------------------------------- Parameters --------------------------------

#file with volume projections
vol_projections_file <- "2026_budget_volume.csv"

#file with los adjustments
los_projections_file <- "los_adjustments_2027Q4.csv"


datasets_processed <- list(
  "baseline" = baseline,
  "scenario" = baseline)

#generate daily_demand
daily_demand <- daily_demand(
  datasets_processed)

#group by SERVICE_GROUP
daily_demand <- lapply(daily_demand,function(df) {
  df %>%
    group_by(
      LOC_NAME,
      SERVICE_GROUP,
      SERVICE_MONTH,
      SERVICE_DATE
    ) %>%
    summarise(
      DAILY_DEMAND = sum(BED_CHARGES, na.rm = TRUE),
      .groups = "drop"
    )
}
)


#change the year of SERVICE_DATE and SERVICE_MONTH from 2025 to 2026 for SCENARIO
daily_demand[["scenario"]] <- daily_demand[["scenario"]] %>%
  mutate(
    SERVICE_DATE = update(SERVICE_DATE, year = 2026),
    SERVICE_MONTH = update(SERVICE_MONTH, year = 2026)
  )

#merge scenario and baseline in daily_demand in one list that contains 2025 and 2026
merged_df <- bind_rows(
  daily_demand[["baseline"]],
  daily_demand[["scenario"]]
)

holidays <- as.Date(c(
  holidayNYSE(2025),
  holidayNYSE(2026)
))

holiday_window <- sort(unique(c(
  holidays - 1,  
  holidays,      
  holidays + 1   
)))

# ------------------------------------------------------------- Total Demand Analysis -------------------------------- --------------------------------

total_baseline <- merged_df %>%
  filter(
    LOC_NAME != "MSBI"
    #,SERVICE_DATE <= as.Date("2025-12-28")
  ) %>%
  group_by(SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(DAILY_DEMAND), .groups = "drop") %>%
  as_tsibble(index = SERVICE_DATE) %>%
  fill_gaps(DAILY_DEMAND = 0) %>%
  mutate(
    HOLIDAY_WINDOW = factor(
      SERVICE_DATE %in% holiday_window,
      levels = c(FALSE, TRUE),
      labels = c("No", "Yes")
    ),
    DOW = wday(SERVICE_DATE, label = TRUE),
    MONTH = month(SERVICE_DATE, label = TRUE)
  )


model_fit_total <- total_baseline %>%
  model(
    arima_reg = ARIMA(
      DAILY_DEMAND ~ DOW + MONTH + HOLIDAY_WINDOW
    ),
    
    ets = ETS(
      DAILY_DEMAND
    ),
    
    snaive = SNAIVE(
      DAILY_DEMAND ~ lag("week")
    ),
    
    tslm = TSLM(
      DAILY_DEMAND ~ DOW + MONTH + HOLIDAY_WINDOW
    ),
    
    stl_ets = decomposition_model(
      STL(DAILY_DEMAND),
      ETS(season_adjust)
    )
  )

accuracy_metrics_total <- fabletools::accuracy(model_fit_total)

rmse_pct_total <- accuracy_metrics_total %>%
  mutate(
    RMSE_PCT = RMSE / mean(total_baseline$DAILY_DEMAND, na.rm = TRUE) * 100
  ) %>%
  arrange(RMSE)



future_data <- tibble(
  SERVICE_DATE = seq.Date(
    from = max(total_baseline$SERVICE_DATE) + 1,
    to = max(total_baseline$SERVICE_DATE) + years(3),
    by = "day"
  )
) %>%
  mutate(
    HOLIDAY_WINDOW = factor(
      SERVICE_DATE %in% holiday_window,
      levels = c(FALSE, TRUE),
      labels = c("No", "Yes")
    ),
    DOW = wday(SERVICE_DATE, label = TRUE),
    MONTH = month(SERVICE_DATE, label = TRUE)
  ) %>%
  as_tsibble(index = SERVICE_DATE)


future_forecast <- model_fit_total %>%
  forecast(new_data = future_data)


projected_points <- future_forecast %>%
  as_tibble() %>%
  select(.model, SERVICE_DATE, .mean)


# ------------------------------------------------------------- Hospital Demand Analysis -------------------------------- --------------------------------


hospital_baseline <- merged_df %>%
  filter(LOC_NAME != "MSBI") %>%
  group_by(LOC_NAME, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(DAILY_DEMAND)) %>%
  as_tsibble(key = c(LOC_NAME), index = SERVICE_DATE) %>%
  fill_gaps(DAILY_DEMAND = 0)


# ------------------------------------------------------------- Service-Group Demand Analysis -------------------------------- --------------------------------


service_group_baseline <- merged_df %>%
  filter(LOC_NAME != "MSBI") %>%
  group_by(LOC_NAME, SERVICE_GROUP, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(DAILY_DEMAND)) %>%
  as_tsibble(key = c(LOC_NAME, SERVICE_GROUP), index = SERVICE_DATE) %>%
  fill_gaps(DAILY_DEMAND = 0)






























# ----------------------------------- Greg --------------------------------

library(knitr)
library(tidyverse)
library(odbc)
library(DBI)
library(glue)
library(dplyr)
library(tidyr)
library(dbplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(openxlsx)
library(readxl)
library(rmarkdown)
library(tsibble)
library(fable)
library(fabletools)

accuracy <- fabletools::accuracy

# ----------------------------------- Constants --------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# Load Baseline Data
baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
    LOC_NAME = case_when(
      LOC_NAME == 'THE MOUNT SINAI HOSPITAL' ~ 'MSH',
      LOC_NAME == 'MOUNT SINAI QUEENS'       ~ 'MSQ',
      LOC_NAME == 'MOUNT SINAI BROOKLYN'     ~ 'MSB',
      LOC_NAME == 'MOUNT SINAI BETH ISRAEL'  ~ 'MSBI',
      LOC_NAME == 'MOUNT SINAI MORNINGSIDE'  ~ 'MSM',
      LOC_NAME == 'MOUNT SINAI WEST'         ~ 'MSW',
      TRUE ~ LOC_NAME),
    FACILITY_MSX = case_when(
      FACILITY_MSX == "BIB" ~ "MSB",
      FACILITY_MSX == "BIP" ~ "MSBI",
      FACILITY_MSX == "RVT" ~ "MSW",
      FACILITY_MSX == "STL" ~ "MSM",
      TRUE ~ FACILITY_MSX))

#pool NA SERVICE_GROUP vals as "Other"
baseline <- baseline %>%
  mutate(
    SERVICE_GROUP = if_else(
      is.na(SERVICE_GROUP),
      "Other",
      SERVICE_GROUP
    )
  )

baseline <- baseline %>%
  mutate(
    LOC_NAME = case_when(
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSH" ~ "MSH",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSQ" ~ "MSQ",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSBI" ~ "MSBI",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSB" ~ "MSB",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSM" ~ "MSM",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSW" ~ "MSW",
      TRUE ~ LOC_NAME
    )
  )
#  ------------------------------ Functions ------------------------------------


source("functions/location_swap.R")
source("functions/emergency_exclusion.R")
source("functions/los_adjustment.R")
source("functions/unit_capacity.R")
source("functions/excel_add_to_wb.R")
source("functions/save_parameters.R")
source("functions/volume_projections.R")
source("functions/dow_service_group.R")
source("functions/dow_unit.R")
source("functions/excel_add_to_wb_dow.R")
source("functions/NA_cleanup.R")
source("functions/daily_demand.R")

# execute ip utiliziation script
source("model/model-ip-utilization.R")


# # ---------------------------- Scenario Parameters -----------------------------
# 
# # file with volume projections
# vol_projections_file <- "2026_budget_volume.csv"
# 
# # file with los adjustments
# los_projections_file <- "los_adjustments_2027Q4.csv"
# 
# # calculate # of weekdays and # of all days in dataset
# num_days <- as.numeric(difftime(max(baseline$SERVICE_DATE),
#                                 min(baseline$SERVICE_DATE), 
#                                 units = "days")) + 1
# weekdays <- seq(min(baseline$SERVICE_DATE), max(baseline$SERVICE_DATE), by = "day")

# years
n_years <- "3 year"

# -------------------- Execute Model ------------------------------------------

# read in processed data from data refresh script
datasets_processed <- list(
  "baseline" = baseline,
  "scenario" = baseline)

# call daily_demand function
daily_demand <- daily_demand(
  datasets_processed)

names(daily_demand) <- names(datasets_processed)


daily_demand_forecast <- lapply(daily_demand,function(df) {
  df %>%
    group_by(
      LOC_NAME,
      SERVICE_GROUP,
      SERVICE_MONTH,
      SERVICE_DATE
    ) %>%
    summarise(
      DAILY_DEMAND = sum(BED_CHARGES, na.rm = TRUE),
      .groups = "drop"
    )
}
)

################# Baseline Total Demand ##############################
# baseline forecast on unaltered 2025 data
baseline_df <- daily_demand_forecast[["baseline"]] %>%
  filter(LOC_NAME != "MSBI",
         #### manual adjustment for nursing strike census drop #####
         SERVICE_DATE <= as.Date("2025-12-28"),
  ) %>%
  group_by(SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(DAILY_DEMAND)) %>%
  as_tsibble(index = SERVICE_DATE) %>%
  fill_gaps(DAILY_DEMAND = 0)

# fit linear and ets model based on 2025 data
baseline_fit <- baseline_df %>%
  model(
    linear = TSLM(log(DAILY_DEMAND + 1) ~ trend() + season("week") + fourier(period = "year", K = 4)),
    ets = ETS(DAILY_DEMAND ~ error("A") + trend("A") + season("A")))

# grade model predictions
baseline_fit_accuracy <- accuracy(baseline_fit) %>%
  arrange(RMSE)

# forecast n years based on both models
baseline_fc <- baseline_fit %>% forecast(h = n_years)

# plot projected demand
ggplot() +
  geom_line(data = daily_demand_forecast[["baseline"]] %>% group_by(SERVICE_DATE) %>% summarise(DAILY_DEMAND = sum(DAILY_DEMAND)),
            aes(x = SERVICE_DATE, y = DAILY_DEMAND)) +
  geom_line(data = baseline_fc %>% filter(.model == "linear"),
            aes(x = SERVICE_DATE, y = .mean))

################# Baseline Total Hospital Demand ##############################
# baseline forecast on unaltered 2025 data
baseline_hosp_df <- daily_demand_forecast[["baseline"]] %>%
  filter(LOC_NAME != "MSBI",
         SERVICE_DATE <= as.Date("2025-12-28"),
  ) %>%
  group_by(LOC_NAME, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(DAILY_DEMAND)) %>%
  as_tsibble(key = c(LOC_NAME), index = SERVICE_DATE) %>%
  fill_gaps(DAILY_DEMAND = 0)

# fit linear and ets model based on 2025 data
baseline_hosp_fit <- baseline_hosp_df %>%
  model(
    linear = TSLM(log(DAILY_DEMAND + 1) ~ trend() + season("week") + fourier(period = "year", K = 4)),
    ets = ETS(DAILY_DEMAND ~ error("A") + trend("A") + season("A")))

# grade model predictions
baseline_hosp_fit_accuracy <- accuracy(baseline_hosp_fit) %>%
  arrange(LOC_NAME, RMSE)

# forecast n years based on both models
baseline_hosp_fc <- baseline_hosp_fit %>% forecast(h = n_years)

# plot projected demand
ggplot() +
  geom_line(data = daily_demand_forecast[["baseline"]] %>% group_by(LOC_NAME, SERVICE_DATE) %>% summarise(DAILY_DEMAND = sum(DAILY_DEMAND)),
            aes(x = SERVICE_DATE, y = DAILY_DEMAND, color = LOC_NAME)) +
  geom_line(data = baseline_hosp_fc %>% filter(.model == "linear"),
            aes(x = SERVICE_DATE, y = .mean, color = LOC_NAME))

##################### Baseline Service Group Demand ###########################
# baseline forecast on unaltered 2025 data
baseline_sg_df <- daily_demand_forecast[["baseline"]] %>%
  filter(LOC_NAME != "MSBI",
         SERVICE_DATE <= as.Date("2025-12-28"),
  ) %>%
  group_by(LOC_NAME, SERVICE_GROUP, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(DAILY_DEMAND)) %>%
  as_tsibble(key = c(LOC_NAME, SERVICE_GROUP), index = SERVICE_DATE) %>%
  fill_gaps(DAILY_DEMAND = 0)

# fit linear and ets model based on 2025 data
baseline_sg_fit <- baseline_sg_df %>%
  model(
    linear = TSLM(log(DAILY_DEMAND + 1) ~ trend() + season("week") + fourier(period = "year", K = 2)),
    ets = ETS(DAILY_DEMAND ~ error("A") + trend("A") + season("A")))

# grade model predictions
baseline_sg_fit_accuracy <- accuracy(baseline_sg_fit) %>%
  arrange(LOC_NAME, RMSE)

# forecast n years based on both models
baseline_sg_fc <- baseline_sg_fit %>% forecast(h = n_years)

# plot projected demand for each hospital
for (hosp in unique(baseline_sg_df$LOC_NAME)) {
  p <- ggplot() +
    geom_line(data = daily_demand_forecast[["baseline"]] %>% filter(LOC_NAME == hosp) %>% group_by(LOC_NAME, SERVICE_GROUP, SERVICE_DATE) %>% summarise(DAILY_DEMAND = sum(DAILY_DEMAND)),
              aes(x = SERVICE_DATE, y = DAILY_DEMAND, color = SERVICE_GROUP)) +
    geom_line(data = baseline_sg_fc %>% filter(.model == "linear", LOC_NAME == hosp),
              aes(x = SERVICE_DATE, y = .mean, color = SERVICE_GROUP)) +
    labs(title = paste("Location:", hosp),
         y = "Daily Demand", x = "Date", color = "SERVICE GROUP")
  print(p)
}