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

# -------------------------------------------------------- Functions & Constants --------------------------------------------------------

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
      SERVICE_GROUP == "Other" & FACILITY_MSX == "BIP" ~ "MSBI",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "BIB" ~ "MSB",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "STL" ~ "MSM",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "RVT" ~ "MSW",
      TRUE ~ LOC_NAME
    )
  )
#  ---------------------------------------------------------------- Render Models ----------------------------------------------------------------

# load all functions
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

# file with unit capacity adjustments
unit_capacity_adjustments <- "tisch_cancer_center_12.4.2025.csv"

# file with volume projections
vol_projections_file <- "2026_budget_volume.csv"

# file with los adjustments
los_projections_file <- "los_adjustments_2027Q4.csv"

# calculate # of weekdays and # of all days in dataset
num_days <- as.numeric(difftime(max(baseline$SERVICE_DATE),
                                min(baseline$SERVICE_DATE), 
                                units = "days")) + 1
weekdays <- seq(min(baseline$SERVICE_DATE), max(baseline$SERVICE_DATE), by = "day")

# run code for IP_Utilization
utilizations <- list()
dow_unit_outputs <- list()
dow_sg_outputs   <- list()
# -------------------------------------------------------- Execute model --------------------------------------------------------
results <- ip_utilization_model()

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


list_name <- "MSHS IP Utilization"

utilizations[[list_name]]   <- ip_utilization_output
dow_unit_outputs[[list_name]] <- ip_comparison_dow_unit
dow_sg_outputs[[list_name]]   <- ip_comparison_dow_service_group

render(input = "model/model-visualizations.Rmd",
       output_file = paste0(cap_dir, "Model Outputs/Visualizations/",
                            "MSHS_IP_Utilization_", Sys.Date(), ".html"))

# -------------------------------------------------------- Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# save parameters and unit capacity changes as necessary
save_parameters(wb = wb)

add_to_wb(df = utilizations[["MSHS IP Utilization"]],
          sheetname = "MSHS IP Utilization")

saveWorkbook(wb,
             file = paste0(cap_dir, "Model Outputs/Workbooks/",
                           "MSHS_IP Utilization_", Sys.Date(), ".xlsx"),
             overwrite = TRUE)


# -------------------------------------------------------- Save DOW Workbook ----------------------------------------------------------------

# create excel workbook for DOW outputs
wb_dow <- createWorkbook()

save_parameters(wb = wb_dow)


# store SG sheet first, then UNIT sheet (per scenario key)
for (i in seq_along(dow_sg_outputs)) {
  
  base_name <- names(dow_sg_outputs)[i]
  
  sheet_sg   <- paste0(base_name, " - SG")
  sheet_unit <- paste0(base_name, " - UNIT")
  
  add_to_wb_dow(df = dow_sg_outputs[[i]],   sheetname = sheet_sg)
  add_to_wb_dow(df = dow_unit_outputs[[i]], sheetname = sheet_unit)
}

# save workbook
saveWorkbook(
  wb_dow,
  file = paste0(cap_dir, "Model Outputs/Workbooks/",
                "DOW_MSHS_IP_Utilization", Sys.Date(), ".xlsx"),
  overwrite = TRUE
)
