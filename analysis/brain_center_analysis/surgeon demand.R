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

neuro_npi <- read_xlsx(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Faculty and NPI.xlsx")) 
neuro_npi_codes <- trimws(neuro_npi$NPI)

# ---------------------------------- Surgeon Demand ---------------------------
surgeon_cases <- tbl(con_prod, in_schema("MS_INSIGHT", 
                                         "OR_QUALITY_DASHBOARD_CASE_DETAILS")) %>%
  filter(SURGEON_NPI %in% neuro_npi_codes,
         CASE_STATUS == "Completed",
         SURGERY_DATE >= as.Date("2025-01-01")) %>%
  group_by(SURGEON_NPI) %>%
  summarise(CASES = n()) %>%
  collect()

surgeon_demand <- neuro_npi %>%
  mutate(NPI = trimws(NPI)) %>%
  left_join(surgeon_cases, 
            by = c("NPI" = "SURGEON_NPI")) %>%
  arrange(desc(CASES))

# quality check
qc <- tbl(con_prod, "MSX_PROVIDER_V") %>%
  filter(NPI %in% neuro_npi_codes) %>%
  collect()

# surgeon check to see how many cases each surgeon has ever had
surgeon_check <- tbl(con_prod, "MSX_PROVIDER_V") %>%
  filter(NPI %in% neuro_npi_codes) %>% 
  left_join(
    tbl(con_prod, in_schema("MS_INSIGHT", "OR_QUALITY_DASHBOARD_CASE_DETAILS")), 
    by = c("NPI" = "SURGEON_NPI")
  ) %>%
  group_by(NPI, LAST_NAME, FIRST_NAME) %>%
  summarise(CASES = sum(ifelse(is.na(OR_CASE_ID), 0, 1), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(CASES)) %>%
  collect()

# get list of surgeons with no surgeries
surgeon_check_zero <- surgeon_check %>%
  filter(CASES == 0)
