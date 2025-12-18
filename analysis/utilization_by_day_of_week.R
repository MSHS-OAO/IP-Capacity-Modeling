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

# -------------------------------------------------------- Functions & Constants --------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"


# List all CSV files in the directory
bed_cap_csv <- list.files(paste0(cap_dir, "Tableau Data/Bed Capacity/"),
                          pattern = "\\.csv$", full.names = TRUE)


# read each CSV and list average bed capacity for each unit monthly
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
    SERVICE_MONTH = mdy(SERVICE_MONTH),
    EXTERNAL_NAME = Unit) %>%
  group_by(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_MONTH) %>%
  summarise(BED_CAPACITY = sum(`Measure Values`, na.rm = TRUE))




# Load Baseline Data
base <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
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


num_days <- as.numeric(difftime(max(base$SERVICE_DATE),
                                min(base$SERVICE_DATE), 
                                units = "days")) + 1


baseline <- base %>%
  filter(!is.na(EXTERNAL_NAME)) %>%
  group_by(ENCOUNTER_NO, MSDRG_CD_SRC, FACILITY_MSX, ATTENDING_VERITY_REPORT_SERVICE, 
           UNIT_DESC_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, 
           SERVICE_DATE, LOS_NO_SRC) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(BED_CHARGES = case_when(
    BED_CHARGES > 1 ~ 1,
    TRUE ~ BED_CHARGES)) 

# get total daily volume for each service line and unit type
baseline <- baseline %>%
  group_by(FACILITY_MSX, SERVICE_GROUP,EXTERNAL_NAME,ATTENDING_VERITY_REPORT_SERVICE, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop")



baseline <- baseline %>%
  group_by(FACILITY_MSX, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(DAILY_DEMAND, na.rm = TRUE), .groups = "drop") %>%
  collect() %>%
  left_join(bed_cap, by = c("FACILITY_MSX" = "HOSPITAL", 
                            "SERVICE_GROUP" = "SERVICE_GROUP",
                            "SERVICE_MONTH" = "SERVICE_MONTH",
                            "EXTERNAL_NAME" = "EXTERNAL_NAME")) 

baseline <- baseline %>%
  mutate(UTILIZATION = DAILY_DEMAND/BED_CAPACITY)


# *************************************************  Overall utilization ***************************************************

df_overall <- baseline

df_overall_day_count <- df_overall %>%
  add_count(FACILITY_MSX, SERVICE_GROUP,EXTERNAL_NAME, name = "COUNT_OF_SERVICE_DAYS") %>%
  select(-DAILY_DEMAND,-SERVICE_MONTH,-BED_CAPACITY)

df_overall <- df_overall %>%
  group_by(FACILITY_MSX, SERVICE_GROUP,EXTERNAL_NAME) %>%
  summarise(UTILIZATION_SUM = sum(UTILIZATION), .groups = "drop")

df_overall <- df_overall %>% left_join(df_overall_day_count,by = c("FACILITY_MSX" = "FACILITY_MSX",
                                                                   "SERVICE_GROUP" = "SERVICE_GROUP",
                                                                   "EXTERNAL_NAME" = "EXTERNAL_NAME"))
df_overall <- df_overall %>% select(-UTILIZATION,-SERVICE_DATE)

df_overall <- df_overall %>% mutate(AVG_UTILIZATION_OVERALL = UTILIZATION_SUM/COUNT_OF_SERVICE_DAYS)

df_overall <- unique(df_overall)

df_overall <- df_overall %>% select(-UTILIZATION_SUM,-COUNT_OF_SERVICE_DAYS)

df_overall$EXTERNAL_NAME <- sub("^\\S+\\s+", "", df_overall$EXTERNAL_NAME)

df_overall <- df_overall %>% filter(!(is.na(AVG_UTILIZATION_OVERALL)))


# ************************************************* weekday only  utilization ***************************************************

df_weekdays <- baseline

df_weekdays$DAY_OF_WEEK <- weekdays(df_weekdays$SERVICE_DATE)

df_weekdays <- df_weekdays %>% filter(!DAY_OF_WEEK %in% c("Saturday", "Sunday"))

df_weekdays <- df_weekdays %>% select(-BED_CAPACITY,-DAILY_DEMAND,-DAY_OF_WEEK)

df_weekdays <- df_weekdays %>%
  add_count(FACILITY_MSX, SERVICE_GROUP,EXTERNAL_NAME, name = "DAY_REPITITIONS") 

df_weekdays <- df_weekdays %>%
  group_by(FACILITY_MSX,SERVICE_GROUP,EXTERNAL_NAME) %>%
  mutate(SUM_OF_UTILIZATION = sum(UTILIZATION))

df_weekdays <- df_weekdays %>% select(-SERVICE_MONTH,-SERVICE_DATE,-UTILIZATION)


df_weekdays <- df_weekdays %>% mutate(AVG_WEEKDAY_UTILIZATION = SUM_OF_UTILIZATION/DAY_REPITITIONS)

df_weekdays <- df_weekdays %>% select(-DAY_REPITITIONS,-SUM_OF_UTILIZATION)

df_weekdays <- unique(df_weekdays)

df_weekdays <- df_weekdays %>% filter(!is.na(AVG_WEEKDAY_UTILIZATION))

df_weekdays$EXTERNAL_NAME <- sub("^\\S+\\s+", "", df_weekdays$EXTERNAL_NAME)


# ************************************************* day of week utilization ***************************************************
  # load dataset based on name of list element
  df_day_of_week <- baseline
  
  #calculate day of week
  df_day_of_week$DAY_OF_WEEK <- weekdays(df_day_of_week$SERVICE_DATE)
    
  #take out daily_demand and avg_bed_capacity
  df_day_of_week <- df_day_of_week %>% select(-BED_CAPACITY,-DAILY_DEMAND)
    
  # COUNT # of rows with facility/unit/day of week combination for avg calculation
  df_day_of_week <- df_day_of_week %>%
      add_count(FACILITY_MSX, SERVICE_GROUP,EXTERNAL_NAME, DAY_OF_WEEK, name = "DAY_OF_WEEK_REPITITIONS") #DAY_OF_WEEK_REPITITIONS

    
  df_day_of_week <- df_day_of_week %>%
      group_by(FACILITY_MSX,SERVICE_GROUP,EXTERNAL_NAME,DAY_OF_WEEK) %>%
      mutate(SUM_OF_UTILIZATION = sum(UTILIZATION))
    
  df_day_of_week <-df_day_of_week %>% select(-SERVICE_MONTH,-SERVICE_DATE,-UTILIZATION)
    
  df_day_of_week <- df_day_of_week %>%
      filter(!is.na(SUM_OF_UTILIZATION))
    
  df_day_of_week <- df_day_of_week %>% mutate(AVG_UTILIZATION_DAY_OF_WEEK = SUM_OF_UTILIZATION/DAY_OF_WEEK_REPITITIONS)
    
  df_day_of_week <- unique(df_day_of_week)
    
  df_day_of_week <- df_day_of_week %>% select(-DAY_OF_WEEK_REPITITIONS,-SUM_OF_UTILIZATION)
  
  df_day_of_week$EXTERNAL_NAME <- sub("^\\S+\\s+", "", df_day_of_week$EXTERNAL_NAME)
  
    
    
    
# ************************************************* UI **************************************************************


  
  
  ui <- fluidPage(
    titlePanel("Inpatient Unit Type Analysis"),
    fluidRow(
      style = "background-color: #f0f0f0; padding: 15px; border-radius: 8px;",
      
      column(
        width = 4,
        pickerInput(
          inputId = "dropdown1",
          label   = "Select Hospital:",
          choices = hospital_choices,
          selected = "MSH",
          multiple = TRUE,
          width = "100%",
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = "startsWith",
            actionsBox = TRUE,             
            selectedTextFormat = "count > 2",
            dropupAuto = FALSE
          )
        )
      ),
      
      column(
        width = 4,
        pickerInput(
          inputId = "dropdown2",
          label   = "Select Service Line:",
          choices = NULL,
          multiple =TRUE,
          width   = "100%",
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = "startsWith",
            actionsBox = TRUE,
            liveSearchNormalize = TRUE,
            noneSelectedText = "Search or select a service line..."
          )
        )),
      
      column(
        width = 4,
        pickerInput(
          inputId = "dropdown3",
          label   = "Select Unit Type:",
          choices = unit_choices,
          selected = "CRITICAL CARE",
          multiple = TRUE,
          width = "100%",
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = "startsWith",
            actionsBox = TRUE,
            selectedTextFormat = "count > 3",
            dropupAuto = FALSE
          )
        )
      ),
      
    ),
    
    br(),
    textOutput("selected_options"),
    br(),
    DTOutput("service_output"),
    br(),
    DTOutput("surgeon_output")
  )
  
