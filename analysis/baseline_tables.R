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


# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")
# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/SixSigma/Project Based/System/Capacity Modeling/"



baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
    FACILITY_MSX = case_when(
      FACILITY_MSX == 'STL' ~ 'MSM',
      FACILITY_MSX == 'RVT' ~ 'MSW',
      FACILITY_MSX == 'BIB' ~ 'MSB',
      FACILITY_MSX == 'BIP' ~ 'MSBI',
      TRUE ~ FACILITY_MSX
    )
  )



bed_cap_csv <- list.files(paste0(cap_dir, "Tableau Data/Bed Capacity/"),
                          pattern = "\\.csv$", full.names = TRUE)
bed_cap <- bed_cap_csv %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE) %>% mutate(source_file = basename(.x))) %>%
  rename(HOSPITAL = Location,
         SERVICE_GROUP = `Service Group`,
         MEASURE = `Measure Names`,
         SERVICE_MONTH = `Day of Census Start`) %>%
  filter(MEASURE == 'Avg Total Beds') %>%
  mutate(
    HOSPITAL = case_when(
      HOSPITAL == "MOUNT SINAI BETH ISRAEL" ~ "MSBI",
      HOSPITAL == "MOUNT SINAI BROOKLYN" ~ "MSB",
      HOSPITAL == "MOUNT SINAI MORNINGSIDE" ~ "MSM",
      HOSPITAL == "MOUNT SINAI QUEENS" ~ "MSQ",
      HOSPITAL == "MOUNT SINAI WEST" ~ "MSW",
      HOSPITAL == "THE MOUNT SINAI HOSPITAL" ~ "MSH"),
    SERVICE_MONTH = mdy(SERVICE_MONTH)) %>%
  group_by(HOSPITAL, SERVICE_GROUP, SERVICE_MONTH) %>%
  summarise(AVG_BED_CAPACITY = sum(`Measure Values`, na.rm = TRUE))



ip_mapping <- tbl(con_prod, "IPCAP_BILLING_CAT_DESC") %>%
  filter(CATEGORY == "IP") %>%
  collect() %>%
  pull(BILLING_CAT_DESC)




baseline2 <- baseline %>%
  filter(!is.na(EXTERNAL_NAME),
         BILLING_CAT_DESC == ip_mapping) %>%
  group_by(FACILITY_MSX, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(QUANTITY), .groups = "drop") %>%
  collect() %>%
  left_join(bed_cap, by = c("FACILITY_MSX" = "HOSPITAL", 
                            "SERVICE_GROUP" = "SERVICE_GROUP",
                            "SERVICE_MONTH" = "SERVICE_MONTH")) %>%
  mutate(UTILIZATION = DAILY_DEMAND/AVG_BED_CAPACITY,
         UTILIZATION_85 = case_when(
           UTILIZATION > .85 ~ TRUE,
           TRUE ~ FALSE),
         UTILIZATION_95 = case_when(
           UTILIZATION > .95 ~ TRUE,
           TRUE ~ FALSE))




ip_comparison_total <- baseline2 %>%
  group_by(FACILITY_MSX, SERVICE_GROUP, SERVICE_MONTH, AVG_BED_CAPACITY) %>%
  summarise(TOTAL_DEMAND = sum(DAILY_DEMAND, na.rm = TRUE),
            TOTAL_85 = sum(UTILIZATION_85),
            TOTAL_95 = sum(UTILIZATION_95),
            AVG_BED_CAPACITY = mean(AVG_BED_CAPACITY)) %>%
  mutate(AVG_DAILY_DEMAND= TOTAL_DEMAND/days_in_month(SERVICE_MONTH),
         AVG_PERCENT_85 = TOTAL_85/days_in_month(SERVICE_MONTH),
         AVG_PERCENT_95 = TOTAL_95/days_in_month(SERVICE_MONTH),
         AVG_UTILIZATION = AVG_DAILY_DEMAND/AVG_BED_CAPACITY) %>%
  mutate(across(where(is.numeric), \(x) coalesce(x, 0)))




# unique_facilities <- unique(ip_comparison_total$FACILITY_MSX)
# df_list <- split(ip_comparison_total, ip_comparison_total$FACILITY_MSX)




ip_utilization_output <- ip_comparison_total %>%
  group_by(FACILITY_MSX, SERVICE_GROUP) %>%
  summarise(
    AVG_BED_CAPACITY = mean(AVG_BED_CAPACITY, na.rm = TRUE),
    AVG_DAILY_DEMAND = mean(AVG_DAILY_DEMAND, na.rm = TRUE),
    AVG_UTILIZATION = mean(AVG_UTILIZATION, na.rm = TRUE),
    AVG_PERCENT_85 = mean(AVG_PERCENT_85, na.rm = TRUE),
    AVG_PERCENT_95 = mean(AVG_PERCENT_95, na.rm = TRUE),
    .groups = "drop"
  )


ip_utilization_output <- ip_utilization_output %>%
  mutate(
    AVG_BED_CAPACITY = round(as.numeric(AVG_BED_CAPACITY), 2),
    AVG_DAILY_DEMAND = round(as.numeric(AVG_DAILY_DEMAND), 2),
    AVG_UTILIZATION = round(as.numeric(AVG_UTILIZATION) * 100, 0) / 100,
    AVG_PERCENT_85 = round(as.numeric(AVG_PERCENT_85) * 100, 0) / 100,
    AVG_PERCENT_95 = round(as.numeric(AVG_PERCENT_95) * 100, 0) / 100
  ) %>%
  mutate(
    AVG_BED_CAPACITY = ifelse(is.finite(AVG_BED_CAPACITY), AVG_BED_CAPACITY, NA_real_),
    AVG_DAILY_DEMAND = ifelse(is.finite(AVG_DAILY_DEMAND), AVG_DAILY_DEMAND, NA_real_),
    AVG_UTILIZATION = ifelse(is.finite(AVG_UTILIZATION), AVG_UTILIZATION, NA_real_),
    AVG_PERCENT_85 = ifelse(is.finite(AVG_PERCENT_85), AVG_PERCENT_85, NA_real_),
    AVG_PERCENT_95 = ifelse(is.finite(AVG_PERCENT_95), AVG_PERCENT_95, NA_real_)
  )

wb <- createWorkbook()
addWorksheet(wb, "IP_Utilization")
writeData(wb, sheet = "IP_Utilization", ip_utilization_output,
          headerStyle = createStyle(fgFill = "#D9D9D9", textDecoration = "bold"))

pct_style <- createStyle(numFmt = "0%")
num2_style <- createStyle(numFmt = "0.00")

pct_cols <- which(names(ip_utilization_output) %in% c("AVG_UTILIZATION", "AVG_PERCENT_85", "AVG_PERCENT_95"))
num2_cols <- which(names(ip_utilization_output) %in% c("AVG_BED_CAPACITY", "AVG_DAILY_DEMAND"))

addStyle(wb, sheet = "IP_Utilization", style = pct_style,
         cols = pct_cols, rows = 2:(nrow(ip_utilization_output) + 1), gridExpand = TRUE)
addStyle(wb, sheet = "IP_Utilization", style = num2_style,
         cols = num2_cols, rows = 2:(nrow(ip_utilization_output) + 1), gridExpand = TRUE)

width_cols <- which(names(ip_utilization_output) %in% c("AVG_BED_CAPACITY", "AVG_DAILY_DEMAND", "AVG_UTILIZATION", "AVG_PERCENT_85", "AVG_PERCENT_95"))
setColWidths(wb, sheet = "IP_Utilization", cols = width_cols, widths = 20)

saveWorkbook(wb,
             file = paste0(cap_dir, "Model Outputs/Workbooks/", "Baseline_", Sys.Date(), ".xlsx"),
             overwrite = TRUE)

