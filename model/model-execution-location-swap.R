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

# -------------------------------------------------------- Load Data -------------------------------------------------------------------------

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
#  ---------------------------------------------------------------- Render Models ----------------------------------------------------------------

# load all functions
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

# ---------------------------------------------------------- Scenario Parameters ----------------------------------------------------------

# set hospitals in scenario
hospitals <- list("MSH", "MSM")

services <- list("CARDIOVASCULAR SURGERY", 
                 "VASCULAR SURGERY")

# how the rerouted service group should be distributed at destination hospital
reroute_service_group_percent <- list(
 c("Med Surg" = 0.80,
   "Critical Care" = 0.20),
 c("Heart" = 0.65,
   "Critical Care" = 0.35))


# file with unit capacity adjustments
unit_capacity_adjustments <- "tisch_cancer_center_12.4.2025.csv"

# file with volume projections
vol_projections_file <- "2026_budget_volume.csv"

# file with los adjustments
los_projections_file <- "los_adjustments_2027Q4.csv"

# emergency exclusions
exclusion_hosp1 <- TRUE
exclusion_hosp2 <- TRUE

# percentage of service line moving from hospital n
percentage_to_hosp1_list <- c(0.4, 0.9)
percentage_to_hosp2_list <- c(0.9,0.4)

# calculate # of weekdays and # of all days in dataset
all_dates <- seq(min(baseline$SERVICE_DATE),max(baseline$SERVICE_DATE), by = "day")
num_days <- length(all_dates)
num_weekdays <- sum(!wday(all_dates) %in% c(1, 7))
num_weekend_days <- sum(wday(all_dates) %in% c(1, 7))
dow_counts <- table(
  factor(
    toupper(weekdays(all_dates)),
    levels = c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
  )
)


# run code for IP_Utilization
utilizations <- list()
dow_unit_outputs <- list()
dow_sg_outputs   <- list()

# -------------------------------------------------------- Execute model --------------------------------------------------------
for (i in 1:length(percentage_to_hosp1_list)) {
  print(paste0("Running scenario ", i, "/", length(percentage_to_hosp1_list)))
  
  # specify num of simulations
  n_simulations = 1

  # define global percentage_to_hosp1 and percentage_to_hosp2 here
  percentage_to_hosp1 <- ifelse(is.null(percentage_to_hosp1_list), 1, percentage_to_hosp1_list[i])
  percentage_to_hosp2 <- ifelse(is.null(percentage_to_hosp2_list), 1, percentage_to_hosp2_list[i])
  
  # adjust n_simulations according to percentages
  if (percentage_to_hosp1 %in% c(0, 1) & percentage_to_hosp2 %in% c(0, 1)) {
    n_simulations <- 1
  }
  
  results <- ip_utilization_model (
    generator = "location_swap",
    n_simulations = n_simulations
  )
  
  # Unpack values from IP result list
  ip_utilization_output = results$ip_utilization_output
  ip_comparison_total = results$ip_comparison_total
  ip_comparison_monthly = results$ip_comparison_monthly
  ip_comparison_daily = results$ip_comparison_daily
  ip_comparison_dow_service_group     = results$ip_comparison_dow_service_group
  ip_comparison_dow_unit     = results$ip_comparison_dow_unit

  
  #cleanup NA values using na_cleanup function
  ip_utilization_output <- na_cleanup(ip_utilization_output)
  ip_comparison_dow_service_group <- na_cleanup(ip_comparison_dow_service_group)

  # save utilizations outputs in list to loop through for workbook saving
    list_name <- paste0(hospitals[[1]], percentage_to_hosp2_list[i] * 100, " - ",
                        hospitals[[2]], percentage_to_hosp1_list[i] * 100)
    utilizations[[list_name]] <- ip_utilization_output
    dow_unit_outputs[[list_name]] <- ip_comparison_dow_unit
    dow_sg_outputs[[list_name]]   <- ip_comparison_dow_service_group
  
  # visualization script
html_output_path <- paste0(cap_dir, "Model Outputs/Visualizations/",
                               "model-visualizations-", 
                               hospitals[[1]], services[[1]], "-",
                               hospitals[[2]], services[[2]], "_",
                               percentage_to_hosp1_list[i] * 100, "-",
                               percentage_to_hosp2_list[i] * 100, "_",
                               Sys.Date(), ".html")
    render(input = "model/model-visualizations.Rmd",
           output_file = html_output_path)
  }


# -------------------------------------------------------- Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# save parameters and unit capacity changes as necessary
save_parameters(generator = "location_swap", wb = wb)

# create a sheet for each percentage pair
for (i in 1:length(utilizations)) {
  add_to_wb(df = utilizations[[i]],
            sheetname = names(utilizations[i]))
}

saveWorkbook(wb, 
               file = paste0(cap_dir, "Model Outputs/Workbooks/",
                             hospitals[[1]],"_", services[[1]], "-",
                             hospitals[[2]],"_", services[[2]],"_",
                             Sys.Date(), ".xlsx"),
               overwrite = TRUE )

# -------------------------------------------------------- Save DOW Workbook ----------------------------------------------------------------
# create excel workbook for DOW outputs
wb_dow <- createWorkbook()

save_parameters(generator = "location_swap",wb = wb_dow)


# for each scenario: write SG first, then UNIT
for (i in seq_along(dow_unit_outputs)) {
  
  base_name <- names(dow_unit_outputs)[i]
  
  sheet_sg   <- paste0(base_name, " - SG")
  sheet_unit <- paste0(base_name, " - UNIT")
  
  add_to_wb_dow(df = dow_sg_outputs[[i]],   sheetname = sheet_sg)
  add_to_wb_dow(df = dow_unit_outputs[[i]], sheetname = sheet_unit)
}

# save workbook
saveWorkbook(
  wb_dow,
  file = paste0(cap_dir, "Model Outputs/Workbooks/DOW_",
                hospitals[[1]],"_", services[[1]], "-",
                hospitals[[2]],"_", services[[2]], "_",
                Sys.Date(), ".xlsx"),
  overwrite = TRUE
)
