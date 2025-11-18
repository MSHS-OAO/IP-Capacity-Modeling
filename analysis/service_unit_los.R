library(DBI)
library(odbc)
library(glue)
library(dplyr)
library(lubridate)
library(dbplyr)

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# read in unit mapping data
unit_mappings <- tbl(con_prod, in_schema("DASHBD_USER", "CLARITY_DEP_REF")) %>%
  select(EXTERNAL_NAME, LOC_NAME) %>%
  collect()

# read data
care_team <- tbl(con_prod, "IPCAP_CARE_TEAM") %>%
  filter(ADMIT_DT_SRC >= as.Date("2025-01-01"),
         DSCH_DT_SRC <= as.Date("2025-06-30")) %>%
  arrange(SERVICE_DATE, ENCOUNTER_NO) %>%
  collect()

# Get data for encounters without a care team
no_care_team <- care_team %>% filter(is.na(CARE_TEAM_MD))
no_team_encounters <- no_care_team %>% select(ENCOUNTER_NO) %>% distinct() %>% count() %>% pull()
print(glue("There are {no_team_encounters} encounters without a care team"))

# LOS for each care team (care team must have a service line) on IP units
encounter_by_day <- care_team %>%
  #filter(!is.na(EXTERNAL_NAME),
  #       !is.na(VERITY_DIV_DESC_SRC)) %>%
  group_by(ENCOUNTER_NO, FACILITY_MSX, LOS_NO_SRC, CARE_TEAM_MD, VERITY_DEPT_1_DESC_SRC, 
           VERITY_DIV_DESC_SRC, ATTEND_FROM_DTTM, ATTEND_TO_DTTM, EXTERNAL_NAME, 
           SERVICE_GROUP, SERVICE_DATE) %>%
  summarise(BED_CHARGES = sum(QUANTITY)) %>%
  mutate(BED_CHARGES = case_when(
    BED_CHARGES > 1 ~ 1,
    TRUE ~ BED_CHARGES)) %>%
  arrange(ENCOUNTER_NO, SERVICE_DATE)

# UNIT SWITCHES ----------------------------------------------------------------

# break down each encounters stay into unit stays by number of days
encounter_unit_stays <- encounter_by_day %>%  
  arrange(ENCOUNTER_NO, SERVICE_DATE) %>%
  group_by(ENCOUNTER_NO) %>%
  mutate(UNIT_CHANGE = EXTERNAL_NAME != lag(EXTERNAL_NAME),
         DATE_GAP    = SERVICE_DATE != lag(SERVICE_DATE) + days(1),
         NEW_STAY    = is.na(lag(SERVICE_DATE)) | UNIT_CHANGE | DATE_GAP,
         STAY_ID     = cumsum(NEW_STAY)) 

# get LOS for each encounter for every unit they visit
encounter_unit_los <- encounter_unit_stays %>%
  group_by(ENCOUNTER_NO, VERITY_DIV_DESC_SRC, EXTERNAL_NAME, SERVICE_GROUP, STAY_ID) %>%
  summarise(STAY_LENGTH = as.integer(max(SERVICE_DATE) - min(SERVICE_DATE)) + 1,
            STAY_START  = min(SERVICE_DATE),
            STAY_END    = max(SERVICE_DATE),
            .groups = "drop") %>%
  arrange(ENCOUNTER_NO, STAY_ID)

# calculate avg and median LOS by service line by unit
service_unit_los <- encounter_unit_los %>%
  group_by(VERITY_DIV_DESC_SRC, EXTERNAL_NAME, SERVICE_GROUP) %>%
  summarise(STAY_COUNT = n(),
            AVG_LOS = mean(STAY_LENGTH),
            MED_LOS = median(STAY_LENGTH)) %>%
  arrange(VERITY_DIV_DESC_SRC, EXTERNAL_NAME, AVG_LOS) %>%
  left_join(unit_mappings, by = c("EXTERNAL_NAME" = "EXTERNAL_NAME")) %>%
  mutate(LOC_NAME = case_when(
    LOC_NAME == "THE MOUNT SINAI HOSPITAL" ~ "MSH",
    LOC_NAME == "MOUNT SINAI QUEENS"       ~ "MSQ",
    LOC_NAME == "MOUNT SINAI WEST"         ~ "MSW",
    LOC_NAME == "MOUNT SINAI MORNINGSIDE"  ~ "MSM",
    LOC_NAME == "MOUNT SINAI BROOKLYN"     ~ "MSB",
    LOC_NAME == "MOUNT SINAI BETH ISRAEL"  ~ "MSBI")) %>%
  rename(HOSPITAL = LOC_NAME) %>%
  relocate(HOSPITAL)

# MD SWITCHES ------------------------------------------------------------------

care_team_switches <- encounter_by_day %>%
  arrange(ENCOUNTER_NO, SERVICE_DATE) %>%
  group_by(ENCOUNTER_NO) %>%
  mutate(MD_CHANGE = CARE_TEAM_MD != lag(CARE_TEAM_MD),
         DATE_GAP    = SERVICE_DATE != lag(SERVICE_DATE) + days(1),
         NEW_MD    = is.na(lag(SERVICE_DATE)) | MD_CHANGE | DATE_GAP,
         MD_ID     = cumsum(NEW_MD)) 

