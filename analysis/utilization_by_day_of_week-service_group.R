library(shiny)
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
library(shiny)
library(shinyWidgets)   
library(DT)     
library(rmarkdown)

con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"


base <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
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
      TRUE ~ LOC_NAME
    )
  )

num_days <- as.numeric(difftime(max(base$SERVICE_DATE),
                                min(base$SERVICE_DATE), 
                                units = "days")) + 1

baseline <- base %>%
  filter(!is.na(EXTERNAL_NAME)) %>%
  group_by(ENCOUNTER_NO, MSDRG_CD_SRC, LOC_NAME,
           ATTENDING_VERITY_REPORT_SERVICE, SERVICE_GROUP,
           SERVICE_MONTH, SERVICE_DATE, LOS_NO_SRC) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(BED_CHARGES = case_when(
    BED_CHARGES > 1 ~ 1,
    TRUE ~ BED_CHARGES)) 



# load mapping file for all Epic IDs
epic_mapping <- tbl(con_prod, "IPCAP_SERVICE_GROUPS") %>%
  collect() %>%
  mutate(VALID_TO = case_when(
    is.na(VALID_TO) ~ Sys.Date(),
    TRUE ~ VALID_TO),
    VALID_FROM = as.Date(VALID_FROM),
    VALID_TO = as.Date(VALID_TO))

# read each CSV and list average bed capacity for each unit monthly
bed_cap <- read_csv(paste0(cap_dir, "Tableau Data/Detail_data.csv"),
                    show_col_types = FALSE) %>%
  rename(HOSPITAL = Location,
         SERVICE_GROUP = `Service Group`,
         EXTERNAL_NAME = Unit) %>%
  mutate(SERVICE_DATE = mdy(`Day of Census Day`)) %>%
  group_by(HOSPITAL,EXTERNAL_NAME, SERVICE_DATE) %>%
  summarise(DATASET = "BASELINE",
            BED_CAPACITY = sum(`Count of Custom SQL Query`, na.rm = TRUE)) %>%
  mutate(BED_CAPACITY = case_when(
    EXTERNAL_NAME == "MSH KP2 L&D" ~ 20,
    TRUE ~ BED_CAPACITY)) %>%
  filter(HOSPITAL != "MOUNT SINAI BETH ISRAEL",
         SERVICE_DATE >= min(baseline$SERVICE_DATE),
         SERVICE_DATE <= max(baseline$SERVICE_DATE)) %>%
  left_join(epic_mapping, 
            by = join_by(EXTERNAL_NAME == EXTERNAL_NAME,
                         SERVICE_DATE >= VALID_FROM,
                         SERVICE_DATE <= VALID_TO)) %>%
  mutate(
    LOC_NAME = case_when(
      LOC_NAME == "MOUNT SINAI BETH ISRAEL" ~ "MSBI",
      LOC_NAME == "MOUNT SINAI BROOKLYN" ~ "MSB",
      LOC_NAME == "MOUNT SINAI MORNINGSIDE" ~ "MSM",
      LOC_NAME == "MOUNT SINAI QUEENS" ~ "MSQ",
      LOC_NAME == "MOUNT SINAI WEST" ~ "MSW",
      LOC_NAME == "THE MOUNT SINAI HOSPITAL" ~ "MSH")) %>%
  ungroup() %>%
  select(SERVICE_DATE, LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, EPIC_DEPT_ID, BED_CAPACITY, DATASET)


bed_cap <- bed_cap %>% 
  group_by(LOC_NAME,SERVICE_GROUP,SERVICE_DATE) %>% 
  mutate(BED_CAPACITY = sum(BED_CAPACITY)) %>%
  select(-EPIC_DEPT_ID,-DATASET,-EXTERNAL_NAME) %>%
  unique()



bed_cap_avg <- bed_cap %>%
  group_by(LOC_NAME, SERVICE_GROUP) %>%
  summarise(
    AVG_BED_CAPACITY = round(mean(BED_CAPACITY, na.rm = TRUE), 2),
    .groups = "drop"
  )



baseline <- baseline %>%
  group_by(LOC_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop")

baseline <- baseline %>%
  left_join(bed_cap, by = c("LOC_NAME" = "LOC_NAME",
                            "SERVICE_GROUP" = "SERVICE_GROUP",
                            "SERVICE_DATE" = "SERVICE_DATE")) %>%
  mutate(UTILIZATION = DAILY_DEMAND / BED_CAPACITY)

daily_demand_avg <- baseline %>%
  group_by(LOC_NAME, SERVICE_GROUP) %>%
  summarise(
    AVG_DAILY_DEMAND = round(mean(DAILY_DEMAND, na.rm = TRUE), 2),
    .groups = "drop"
  )

# *************************************************  Overall utilization ***************************************************

df_overall <- baseline

df_overall_day_count <- df_overall %>%
  add_count(LOC_NAME, SERVICE_GROUP, name = "COUNT_OF_SERVICE_DAYS") %>%
  select(-DAILY_DEMAND, -SERVICE_MONTH, -BED_CAPACITY)

df_overall <- df_overall %>%
  group_by(LOC_NAME, SERVICE_GROUP) %>%
  summarise(UTILIZATION_SUM = sum(UTILIZATION), .groups = "drop")

df_overall <- df_overall %>%
  left_join(df_overall_day_count,
            by = c("LOC_NAME", "SERVICE_GROUP")) %>%
  select(-UTILIZATION, -SERVICE_DATE) %>%
  mutate(AVG_BED_UTILIZATION = UTILIZATION_SUM / COUNT_OF_SERVICE_DAYS) %>%
  select(-UTILIZATION_SUM, -COUNT_OF_SERVICE_DAYS) %>%
  filter(!is.na(AVG_BED_UTILIZATION)) %>%
  mutate(AVG_BED_UTILIZATION = round(AVG_BED_UTILIZATION * 100, 2)) %>%
  distinct()


# ************************************************* weekday only  utilization ***************************************************


df_weekdays <- baseline

df_weekdays$DAY_OF_WEEK <- weekdays(df_weekdays$SERVICE_DATE)

df_weekdays <- df_weekdays %>% filter(!DAY_OF_WEEK %in% c("Saturday", "Sunday"))

df_weekdays <- df_weekdays %>% select(-BED_CAPACITY, -DAILY_DEMAND, -DAY_OF_WEEK)

df_weekdays <- df_weekdays %>%
  add_count(LOC_NAME, SERVICE_GROUP, name = "DAY_REPITITIONS")

df_weekdays <- df_weekdays %>%
  group_by(LOC_NAME, SERVICE_GROUP) %>%
  mutate(SUM_OF_UTILIZATION = sum(UTILIZATION)) %>%
  select(-SERVICE_MONTH, -SERVICE_DATE, -UTILIZATION) %>%
  mutate(WEEKDAY_AVG_UTILIZATION = SUM_OF_UTILIZATION / DAY_REPITITIONS) %>%
  select(-DAY_REPITITIONS, -SUM_OF_UTILIZATION) %>%
  filter(!is.na(WEEKDAY_AVG_UTILIZATION)) %>%
  mutate(WEEKDAY_AVG_UTILIZATION = round(WEEKDAY_AVG_UTILIZATION * 100, 2)) %>%
  distinct()

# ************************************************* individual day of week utilization ***************************************************

df_day_of_week <- baseline

df_day_of_week$DAY_OF_WEEK <- weekdays(df_day_of_week$SERVICE_DATE)

df_day_of_week <- df_day_of_week %>% select(-BED_CAPACITY, -DAILY_DEMAND)

df_day_of_week <- df_day_of_week %>%
  add_count(LOC_NAME, SERVICE_GROUP, DAY_OF_WEEK,
            name = "DAY_OF_WEEK_REPITITIONS")

df_day_of_week <- df_day_of_week %>%
  group_by(LOC_NAME, SERVICE_GROUP, DAY_OF_WEEK) %>%
  mutate(SUM_OF_UTILIZATION = sum(UTILIZATION)) %>%
  select(-SERVICE_MONTH, -SERVICE_DATE, -UTILIZATION) %>%
  filter(!is.na(SUM_OF_UTILIZATION)) %>%
  mutate(AVG_UTILIZATION_DAY_OF_WEEK =
           SUM_OF_UTILIZATION / DAY_OF_WEEK_REPITITIONS) %>%
  select(-DAY_OF_WEEK_REPITITIONS, -SUM_OF_UTILIZATION) %>%
  mutate(AVG_UTILIZATION_DAY_OF_WEEK =
           round(AVG_UTILIZATION_DAY_OF_WEEK * 100, 2)) %>%
  distinct()


# ************************************************* stats ***************************************************

df_stats <- baseline %>%
  mutate(UTILIZATION = round(UTILIZATION * 100, 2)) %>%
  group_by(LOC_NAME, SERVICE_GROUP) %>%
  mutate(
    OVERALL_MIN_UTILIZATION = if (all(is.na(UTILIZATION))) NA_real_ else min(UTILIZATION, na.rm = TRUE),
    OVERALL_MAX_UTILIZATION = if (all(is.na(UTILIZATION))) NA_real_ else max(UTILIZATION, na.rm = TRUE),
    UTILIZATION_SD = if (sum(!is.na(UTILIZATION)) < 2) NA_real_ else round(sd(UTILIZATION, na.rm = TRUE), 2)
  ) %>%
  ungroup()


df_stats <- df_stats %>% select(-SERVICE_MONTH,-SERVICE_DATE,-DAILY_DEMAND,-BED_CAPACITY,-UTILIZATION) %>%
  unique()


# ************************************************* merge everything in final_df  ***************************************************

final_df <- df_day_of_week %>%
  pivot_wider(
    names_from = DAY_OF_WEEK,
    values_from = AVG_UTILIZATION_DAY_OF_WEEK
  ) %>%
  mutate(
    WEEKEND_AVG_UTILIZATION = round((Saturday + Sunday) / 2, 2)
  )

final_df <- final_df %>%
  left_join(
    df_weekdays %>%
      select(LOC_NAME, SERVICE_GROUP, WEEKDAY_AVG_UTILIZATION),
    by = c("LOC_NAME", "SERVICE_GROUP")
  )

final_df <- final_df %>%
  left_join(
    df_overall %>%
      select(LOC_NAME, SERVICE_GROUP, AVG_BED_UTILIZATION),
    by = c("LOC_NAME", "SERVICE_GROUP")
  )


final_df <- final_df %>%
  left_join(
    df_stats %>%
      select(LOC_NAME, SERVICE_GROUP, OVERALL_MIN_UTILIZATION, OVERALL_MAX_UTILIZATION,UTILIZATION_SD),
    by = c("LOC_NAME", "SERVICE_GROUP")
  )


final_df <- final_df %>%
  rename_with(toupper)


dow_cols <- c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")



final_df <- final_df %>%
  rowwise() %>%
  mutate(
    .min_val = min(c_across(all_of(dow_cols)), na.rm = TRUE),
    .max_val = max(c_across(all_of(dow_cols)), na.rm = TRUE),
    
    DOW_MIN = {
      vals <- c_across(all_of(dow_cols))
      nm <- dow_cols[which(vals == .min_val)]
      paste(nm, collapse = ", ")
    },
    
    DOW_MAX = {
      vals <- c_across(all_of(dow_cols))
      nm <- dow_cols[which(vals == .max_val)]
      paste(nm, collapse = ", ")
    },
    
    DOW_MIN = paste0(DOW_MIN, ": ", sprintf("%.2f", .min_val), "%"),
    DOW_MAX = paste0(DOW_MAX, ": ", sprintf("%.2f", .max_val), "%"),
    
    DOW_DIFF = .max_val - .min_val
  ) %>%
  ungroup() %>%
  select(-.min_val, -.max_val)


final_df <- final_df %>%
  left_join(
    bed_cap_avg,
    by = c("LOC_NAME", "SERVICE_GROUP")
  ) %>% 
  left_join(
    daily_demand_avg,
    by = c("LOC_NAME", "SERVICE_GROUP")
  )

final_df <- final_df %>%
  mutate(WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE =
           WEEKEND_AVG_UTILIZATION - WEEKDAY_AVG_UTILIZATION) %>%
  select(
    LOC_NAME,
    SERVICE_GROUP,
    AVG_BED_CAPACITY,
    AVG_DAILY_DEMAND,
    AVG_BED_UTILIZATION,
    UTILIZATION_SD,
    WEEKEND_AVG_UTILIZATION,
    WEEKDAY_AVG_UTILIZATION,
    WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE,
    OVERALL_MIN_UTILIZATION,
    OVERALL_MAX_UTILIZATION,
    DOW_MIN,
    DOW_MAX,
    DOW_DIFF,
    everything()
  )



final_df_service_group <- final_df %>%
  mutate(
    across(
      c(
        WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE,
        WEEKEND_AVG_UTILIZATION,
        WEEKDAY_AVG_UTILIZATION,
        AVG_BED_UTILIZATION,
        OVERALL_MIN_UTILIZATION,
        OVERALL_MAX_UTILIZATION,
        #UTILIZATION_SD,
        DOW_DIFF,
        SATURDAY, SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY
      ),
      ~ round(.x / 100, 4)
    )
  )



#final_df_service_group$UTILIZATION_SD <- round(final_df_service_group$UTILIZATION_SD * 100, 2)


wb <- createWorkbook()
addWorksheet(wb, "Sheet1")

writeData(wb, "Sheet1", final_df_service_group)

# Columns that should be displayed as percentages
pct_cols <- c(
  "WEEKEND_AVG_UTILIZATION",
  "WEEKDAY_AVG_UTILIZATION",
  "AVG_BED_UTILIZATION",
  "WEEKEND_AVG_UTILIZATION",
  "WEEKDAY_AVG_UTILIZATION",
  "UTILIZATION_SD",
  "OVERALL_MIN_UTILIZATION",
  "OVERALL_MAX_UTILIZATION",
  "DOW_DIFF",
  "SATURDAY", "SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY",
  "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE"
)

pct_style <- createStyle(numFmt = "0.00%")
sd_style  <- createStyle(numFmt = "0.00")

pct_cols_no_sd <- setdiff(pct_cols, "UTILIZATION_SD")

for (col in pct_cols_no_sd) {
  col_index <- which(names(final_df_service_group) == col)
  addStyle(
    wb, sheet = 1,
    style = pct_style,
    cols = col_index,
    rows = 2:(nrow(final_df_service_group) + 1),
    gridExpand = TRUE
  )
}

sd_col <- which(names(final_df_service_group) == "UTILIZATION_SD")
addStyle(
  wb, sheet = 1,
  style = sd_style,
  cols = sd_col,
  rows = 2:(nrow(final_df_service_group) + 1),
  gridExpand = TRUE
)

#saveWorkbook(wb, "~/IP-Capacity-Modeling/analysis/DOW_SERVICE_GROUP.xlsx", overwrite = TRUE)
 