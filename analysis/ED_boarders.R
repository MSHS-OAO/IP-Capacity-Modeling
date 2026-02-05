library(tidyverse)
library(odbc)
library(DBI)
library(glue)
library(dplyr)
library(tidyr)
library(dbplyr)
library(lubridate)

# -------------------------------------------------------- Functions & Constants --------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# Load Baseline Data
baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = floor_date(SERVICE_DATE, "month"),
    LOC_NAME = case_when(
      LOC_NAME == 'THE MOUNT SINAI HOSPITAL' ~ 'MSH',
      LOC_NAME == 'MOUNT SINAI QUEENS'       ~ 'MSQ',
      LOC_NAME == 'MOUNT SINAI BROOKLYN'     ~ 'MSB',
      LOC_NAME == 'MOUNT SINAI BETH ISRAEL'  ~ 'MSBI',
      LOC_NAME == 'MOUNT SINAI MORNINGSIDE'  ~ 'MSM',
      LOC_NAME == 'MOUNT SINAI WEST'         ~ 'MSW',
      TRUE ~ LOC_NAME
    )
  )

emergency <- baseline %>%
  filter(EPIC_DEPT_ID %in% c("8503006",
                             "5119226",
                             "8501001",
                             "8504009",
                             "8502025"),
         !(UNIT_DESC_MSX %in% c('MOUNT SINAI EMERGENCY DEPARTMENT',
                                'EMERGENCY DEPT MORNINGSIDE',
                                'EMERGENCY DEPT WEST',
                                'EMERGENCY DEPT QUEENS',
                                'EMERGENCY DEPT BI BROOKLYN',
                                'EMERGENCY DEPT BI')), 
         (ADMIT_DT_SRC != DSCH_DT_SRC)) %>%
  group_by(ENCOUNTER_NO, MSDRG_CD_SRC,EPIC_DEPT_ID, EPIC_DEPT_NAME, SERVICE_MONTH, 
           SERVICE_DATE, LOS_NO_SRC) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(BED_CHARGES = case_when(
    BED_CHARGES > 1 ~ 1,
    TRUE ~ BED_CHARGES))



all_dates  <- seq(as.Date("2024-06-01"), as.Date("2025-09-30"), by = "day")

emergency_daily_total <- emergency %>%
  group_by(EPIC_DEPT_ID, EPIC_DEPT_NAME, SERVICE_DATE) %>%
  summarise(BOARDER_COUNT = sum(BED_CHARGES, na.rm = TRUE), .groups = "drop") %>% 
  mutate(SERVICE_DATE = as.Date(SERVICE_DATE)) %>%
  complete(
    nesting(EPIC_DEPT_ID, EPIC_DEPT_NAME),
    SERVICE_DATE = all_dates,
    fill = list(BOARDER_COUNT = 0)
  ) %>%
  arrange(EPIC_DEPT_ID, SERVICE_DATE)


emergency_daily_total <- emergency_daily_total %>%
  mutate(EPIC_DEPT_NAME = if_else(EPIC_DEPT_NAME == "EMERGENCY DEPARTMENT","EMERGENCY DEPARTMENT MSH",EPIC_DEPT_NAME))



library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Emergency_Daily_Total")
writeData(wb, "Emergency_Daily_Total", emergency_daily_total)
saveWorkbook(
  wb,
  "~/IP-Capacity-Modeling/analysis/ED_BOARDERS.xlsx",
  overwrite = TRUE
)
