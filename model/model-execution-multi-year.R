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

# # load all functions
# source("functions/los_adjustment.R")
# source("functions/volume_projections.R")
# source("functions/daily_demand.R")
# 
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
daily_demand_forecast <- daily_demand(
  datasets_processed,
  level = "SERVICE_GROUP")
names(daily_demand_forecast) <- names(datasets_processed)

################# Baseline Total Demand ##############################
# baseline forecast on unaltered 2025 data
baseline_df <- daily_demand_forecast[["baseline"]] %>%
  filter(LOC_NAME != "MSBI",
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
