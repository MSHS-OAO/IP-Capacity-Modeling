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

# Capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# Load Baseline Data
baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>%
  collect() %>%
  filter(FACILITY_MSX != "MSSM") %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
    
    LOC_NAME = case_when(
      LOC_NAME == "THE MOUNT SINAI HOSPITAL" ~ "MSH",
      LOC_NAME == "MOUNT SINAI QUEENS"       ~ "MSQ",
      LOC_NAME == "MOUNT SINAI BROOKLYN"     ~ "MSB",
      LOC_NAME == "MOUNT SINAI BETH ISRAEL"  ~ "MSBI",
      LOC_NAME == "MOUNT SINAI MORNINGSIDE"  ~ "MSM",
      LOC_NAME == "MOUNT SINAI WEST"         ~ "MSW",
      TRUE ~ LOC_NAME
    ),
    
    FACILITY_MSX = case_when(
      FACILITY_MSX == "BIB" ~ "MSB",
      FACILITY_MSX == "BIP" ~ "MSBI",
      FACILITY_MSX == "RVT" ~ "MSW",
      FACILITY_MSX == "STL" ~ "MSM",
      TRUE ~ FACILITY_MSX
    )
  ) %>%
  filter(
    SERVICE_DATE >= as.Date("2025-01-01"),
    SERVICE_DATE <= as.Date("2025-12-31"),
    FACILITY_MSX != "MSSN"
  )


# ------------------------------------------------ Pool Missing Service Groups ------------------------------------------------

# Pool NA SERVICE_GROUP values as "Other"
baseline <- baseline %>%
  mutate(
    SERVICE_GROUP = if_else(
      is.na(SERVICE_GROUP),
      "Other",
      SERVICE_GROUP
    )
  )

# Assign LOC_NAME for "Other" service group based on FACILITY_MSX
baseline <- baseline %>%
  mutate(
    LOC_NAME = case_when(
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSH"  ~ "MSH",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSQ"  ~ "MSQ",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSBI" ~ "MSBI",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSB"  ~ "MSB",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSM"  ~ "MSM",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSW"  ~ "MSW",
      TRUE ~ LOC_NAME
    )
  )


# ------------------------------------------------ Create Baseline UNIQUE_ID ------------------------------------------------

# Concatenate LOC_NAME and ATTENDING_VERITY_REPORT_SERVICE
# with no separator between them
baseline <- baseline %>%
  mutate(
    UNIQUE_ID = paste0(
      LOC_NAME,
      ATTENDING_VERITY_REPORT_SERVICE
    )
  )


# ------------------------------------------------ Volume Projection File ------------------------------------------------

# Check one file for one year
projection_file <- "BCG_2026_CON.csv"

# Read projection file
projection_data <- readr::read_csv(
  paste0(
    cap_dir,
    "Mapping Info/volume projections/",
    projection_file
  ),
  show_col_types = FALSE
)


# ------------------------------------------------ Check UNIQUE_ID Mismatches ------------------------------------------------

# Get unique UNIQUE_ID values from baseline
baseline_unique_ids <- baseline %>%
  distinct(UNIQUE_ID) %>%
  filter(
    !is.na(UNIQUE_ID),
    UNIQUE_ID != ""
  ) %>%
  pull(UNIQUE_ID)


# Find UNIQUE_ID values in projection file that do not exist in baseline
unique_id_mismatches <- projection_data %>%
  filter(
    !is.na(UNIQUE_ID),
    UNIQUE_ID != "",
    !(UNIQUE_ID %in% baseline_unique_ids)
  ) %>%
  select(
    UNIQUE_ID,
    HOSPITAL,
    VERITY_REPORT_SERVICE_MSX,
    PERCENT
  ) %>%
  distinct() %>%
  transmute(
    `Output File` = projection_file,
    `Mismatch UNIQUE_ID` = UNIQUE_ID,
    `Hospital` = HOSPITAL,
    `Verity Report Service` = VERITY_REPORT_SERVICE_MSX,
    `Percent` = PERCENT
  ) %>%
  arrange(`Mismatch UNIQUE_ID`)


unique_id_mismatches_non_zero <- unique_id_mismatches %>% filter(Percent != 0)




# analysis #2 

# analysis #2

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

# Prevent scientific notation
options(scipen = 999)

# -------------------------------------------------------- Load Data -------------------------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(
  odbc(),
  "OAO Cloud DB Production"
)

# Capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# ------------------------------------------------ Load Baseline Data ------------------------------------------------

baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>%
  collect() %>%
  filter(FACILITY_MSX != "MSSM") %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
    
    LOC_NAME = case_when(
      LOC_NAME == "THE MOUNT SINAI HOSPITAL" ~ "MSH",
      LOC_NAME == "MOUNT SINAI QUEENS"       ~ "MSQ",
      LOC_NAME == "MOUNT SINAI BROOKLYN"     ~ "MSB",
      LOC_NAME == "MOUNT SINAI BETH ISRAEL"  ~ "MSBI",
      LOC_NAME == "MOUNT SINAI MORNINGSIDE"  ~ "MSM",
      LOC_NAME == "MOUNT SINAI WEST"         ~ "MSW",
      TRUE ~ LOC_NAME
    ),
    
    FACILITY_MSX = case_when(
      FACILITY_MSX == "BIB" ~ "MSB",
      FACILITY_MSX == "BIP" ~ "MSBI",
      FACILITY_MSX == "RVT" ~ "MSW",
      FACILITY_MSX == "STL" ~ "MSM",
      TRUE ~ FACILITY_MSX
    )
  ) %>%
  filter(
    SERVICE_DATE >= as.Date("2025-01-01"),
    SERVICE_DATE <= as.Date("2025-12-31"),
    FACILITY_MSX != "MSSN"
  )

# ------------------------------------------------ Pool NA SERVICE_GROUP ------------------------------------------------

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
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSH"  ~ "MSH",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSQ"  ~ "MSQ",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSBI" ~ "MSBI",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSB"  ~ "MSB",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSM"  ~ "MSM",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSW"  ~ "MSW",
      TRUE ~ LOC_NAME
    )
  )

# ------------------------------------------------ Create Baseline UNIQUE_ID ------------------------------------------------

baseline <- baseline %>%
  mutate(
    UNIQUE_ID = paste0(
      LOC_NAME,
      ATTENDING_VERITY_REPORT_SERVICE
    )
  )

# ------------------------------------------------ Baseline Encounter Counts ------------------------------------------------

baseline_counts <- baseline %>%
  filter(
    !is.na(UNIQUE_ID),
    UNIQUE_ID != ""
  ) %>%
  group_by(UNIQUE_ID) %>%
  summarise(
    BASELINE_ENCOUNTERS = n_distinct(ENCOUNTER_NO),
    .groups = "drop"
  )

# ------------------------------------------------ Volume Projection Path ------------------------------------------------

volume_projection_dir <- paste0(
  cap_dir,
  "Mapping Info/volume projections/"
)

# ------------------------------------------------ CON Files ------------------------------------------------

vol_projections_CON <- list(
  "2026" = "BCG_2026_CON.csv",
  "2027" = "BCG_2027_CON.csv",
  "2028" = "BCG_2028_CON.csv",
  "2029" = "BCG_2029_CON.csv",
  "2030" = "BCG_2030_CON.csv"
)

# ------------------------------------------------ AGG Files ------------------------------------------------

vol_projections_AGG <- list(
  "2026" = "BCG_2026_AGG.csv",
  "2027" = "BCG_2027_AGG.csv",
  "2028" = "BCG_2028_AGG.csv",
  "2029" = "BCG_2029_AGG.csv",
  "2030" = "BCG_2030_AGG.csv"
)

# ------------------------------------------------ Projection Function ------------------------------------------------

create_projection_output <- function(file_list) {
  
  # ------------------------------------------------ Read 2026 ------------------------------------------------
  
  data_2026 <- readr::read_csv(
    paste0(volume_projection_dir, file_list[["2026"]]),
    show_col_types = FALSE
  ) %>%
    select(
      HOSPITAL,
      VERITY_REPORT_SERVICE_MSX,
      PERCENT,
      UNIQUE_ID
    ) %>%
    rename(
      PERCENT_2026 = PERCENT
    )
  
  # ------------------------------------------------ Read 2027 ------------------------------------------------
  
  data_2027 <- readr::read_csv(
    paste0(volume_projection_dir, file_list[["2027"]]),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2027 = PERCENT
    )
  
  # ------------------------------------------------ Read 2028 ------------------------------------------------
  
  data_2028 <- readr::read_csv(
    paste0(volume_projection_dir, file_list[["2028"]]),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2028 = PERCENT
    )
  
  # ------------------------------------------------ Read 2029 ------------------------------------------------
  
  data_2029 <- readr::read_csv(
    paste0(volume_projection_dir, file_list[["2029"]]),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2029 = PERCENT
    )
  
  # ------------------------------------------------ Read 2030 ------------------------------------------------
  
  data_2030 <- readr::read_csv(
    paste0(volume_projection_dir, file_list[["2030"]]),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2030 = PERCENT
    )
  
  # ------------------------------------------------ Combine All Years ------------------------------------------------
  
  projection_data <- data_2026 %>%
    left_join(
      data_2027,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      data_2028,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      data_2029,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      data_2030,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      baseline_counts,
      by = "UNIQUE_ID"
    )
  
  # ------------------------------------------------ Remove Zero / Missing Baseline Counts ------------------------------------------------
  
  projection_data <- projection_data %>%
    filter(
      !is.na(BASELINE_ENCOUNTERS),
      BASELINE_ENCOUNTERS > 0
    )
  
  # ------------------------------------------------ Calculate 2026-2030 ------------------------------------------------
  
  projection_data <- projection_data %>%
    mutate(
      
      `2026` = if_else(
        is.na(PERCENT_2026) | PERCENT_2026 == 0,
        BASELINE_ENCOUNTERS,
        BASELINE_ENCOUNTERS +
          (PERCENT_2026 * BASELINE_ENCOUNTERS)
      ),
      
      `2027` = if_else(
        is.na(PERCENT_2027) | PERCENT_2027 == 0,
        `2026`,
        `2026` +
          (PERCENT_2027 * `2026`)
      ),
      
      `2028` = if_else(
        is.na(PERCENT_2028) | PERCENT_2028 == 0,
        `2027`,
        `2027` +
          (PERCENT_2028 * `2027`)
      ),
      
      `2029` = if_else(
        is.na(PERCENT_2029) | PERCENT_2029 == 0,
        `2028`,
        `2028` +
          (PERCENT_2029 * `2028`)
      ),
      
      `2030` = if_else(
        is.na(PERCENT_2030) | PERCENT_2030 == 0,
        `2029`,
        `2029` +
          (PERCENT_2030 * `2029`)
      )
    )
  
  # ------------------------------------------------ Round Values ------------------------------------------------
  
  projection_data <- projection_data %>%
    mutate(
      across(
        c(
          PERCENT_2026,
          PERCENT_2027,
          PERCENT_2028,
          PERCENT_2029,
          PERCENT_2030
        ),
        ~ round(.x, 4)
      ),
      
      across(
        c(
          `2026`,
          `2027`,
          `2028`,
          `2029`,
          `2030`
        ),
        ~ round(.x, 2)
      )
    )
  
  # ------------------------------------------------ Organize Output ------------------------------------------------
  
  projection_data <- projection_data %>%
    select(
      HOSPITAL,
      VERITY_REPORT_SERVICE_MSX,
      UNIQUE_ID,
      BASELINE_ENCOUNTERS,
      
      PERCENT_2026,
      PERCENT_2027,
      PERCENT_2028,
      PERCENT_2029,
      PERCENT_2030,
      
      `2026`,
      `2027`,
      `2028`,
      `2029`,
      `2030`
    ) %>%
    arrange(
      HOSPITAL,
      VERITY_REPORT_SERVICE_MSX
    )
  
  # ------------------------------------------------ Add Total Row ------------------------------------------------
  
  total_row <- projection_data %>%
    summarise(
      across(
        c(
          `2026`,
          `2027`,
          `2028`,
          `2029`,
          `2030`
        ),
        ~ round(sum(.x, na.rm = TRUE), 2)
      )
    )
  
  projection_data <- bind_rows(
    projection_data,
    total_row
  )
  
  return(projection_data)
}

# ------------------------------------------------ Create CON Dataframe ------------------------------------------------

CON_output <- create_projection_output(
  vol_projections_CON
)

# ------------------------------------------------ Create AGG Dataframe ------------------------------------------------

AGG_output <- create_projection_output(
  vol_projections_AGG
)

# ------------------------------------------------ View Dataframes ------------------------------------------------

View(CON_output)

View(AGG_output)

 
#analysis 3


library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)

# ------------------------------------------------ File Path ------------------------------------------------

simulation_file <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/Model Outputs/Workbooks/BCG/OR data/AGGxAGG/simulation_1.xlsx"


# ------------------------------------------------ Years / Sheets ------------------------------------------------

years <- c(
  "2026",
  "2027",
  "2028",
  "2029",
  "2030"
)


# ------------------------------------------------ Read Sheets and Count Distinct Encounters ------------------------------------------------

simulation_1_counts <- purrr::map_dfr(
  years,
  function(year) {
    
    read_excel(
      simulation_file,
      sheet = year
    ) %>%
      mutate(
        UNIQUE_ID = paste0(
          LOC_NAME,
          ATTENDING_VERITY_REPORT_SERVICE
        )
      ) %>%
      group_by(
        LOC_NAME,
        ATTENDING_VERITY_REPORT_SERVICE,
        UNIQUE_ID
      ) %>%
      summarise(
        DISTINCT_ENCOUNTERS = n_distinct(ENCOUNTER_NO),
        .groups = "drop"
      ) %>%
      mutate(
        YEAR = year
      )
  }
)


# ------------------------------------------------ Pivot Years to Columns ------------------------------------------------

simulation_1_counts_wide <- simulation_1_counts %>%
  select(
    LOC_NAME,
    ATTENDING_VERITY_REPORT_SERVICE,
    UNIQUE_ID,
    YEAR,
    DISTINCT_ENCOUNTERS
  ) %>%
  pivot_wider(
    names_from = YEAR,
    values_from = DISTINCT_ENCOUNTERS,
    values_fill = 0
  ) %>%
  arrange(
    LOC_NAME,
    ATTENDING_VERITY_REPORT_SERVICE
  )


simulation_1_counts_wide <- simulation_1_counts_wide %>% filter(!is.na(simulation_1_counts_wide$ATTENDING_VERITY_REPORT_SERVICE))


# ------------------------------------------------ Create Total Row ------------------------------------------------

total_row <- simulation_1_counts_wide %>%
  summarise(
    `2026` = sum(`2026`, na.rm = TRUE),
    `2027` = sum(`2027`, na.rm = TRUE),
    `2028` = sum(`2028`, na.rm = TRUE),
    `2029` = sum(`2029`, na.rm = TRUE),
    `2030` = sum(`2030`, na.rm = TRUE)
  ) %>%
  mutate(
    LOC_NAME = "TOTAL",
    ATTENDING_VERITY_REPORT_SERVICE = NA_character_,
    UNIQUE_ID = NA_character_
  ) %>%
  select(
    LOC_NAME,
    ATTENDING_VERITY_REPORT_SERVICE,
    UNIQUE_ID,
    `2026`,
    `2027`,
    `2028`,
    `2029`,
    `2030`
  )


# ------------------------------------------------ Add Total Row to Bottom ------------------------------------------------

simulation_1_counts_wide <- bind_rows(
  simulation_1_counts_wide,
  total_row
)


# ------------------------------------------------ View Final Dataframe ------------------------------------------------

View(simulation_1_counts_wide)




# anti join 


unique_encounter_no_dheeraj <- readr::read_csv("New_Query.csv")

unique_encounter_no <- read_excel(
  simulation_file,
  sheet = "2026"
)

# Keep only distinct ENCOUNTER_NO values
unique_encounter_no <- unique_encounter_no %>%
  distinct(ENCOUNTER_NO)

unique_encounter_no_dheeraj <- unique_encounter_no_dheeraj %>%
  distinct(ENCOUNTER_NO)

# ENCOUNTER_NO values in simulation file but NOT in Dheeraj file
anti <- anti_join(
  unique_encounter_no,
  unique_encounter_no_dheeraj,
  by = "ENCOUNTER_NO"
)

View(anti)



#check additions



check_2026 <- read_excel(
  simulation_file,
  sheet = "2026"
) %>%
  mutate(
    UNIQUE_ID = paste0(
      LOC_NAME,
      ATTENDING_VERITY_REPORT_SERVICE
    )
  )


encounters_multiple_unique_ids <- check_2026 %>%
  filter(
    !is.na(ATTENDING_VERITY_REPORT_SERVICE)
  ) %>%
  distinct(
    ENCOUNTER_NO,
    UNIQUE_ID,
    LOC_NAME,
    ATTENDING_VERITY_REPORT_SERVICE
  ) %>%
  group_by(
    ENCOUNTER_NO
  ) %>%
  summarise(
    NUMBER_OF_UNIQUE_IDS = n_distinct(UNIQUE_ID),
    UNIQUE_IDS = paste(
      unique(UNIQUE_ID),
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  filter(
    NUMBER_OF_UNIQUE_IDS > 1
  ) %>%
  arrange(
    desc(NUMBER_OF_UNIQUE_IDS)
  )

View(encounters_multiple_unique_ids)





# Analysis 4


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

# Prevent scientific notation
options(scipen = 999)

# -------------------------------------------------------- Load Data -------------------------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(
  odbc(),
  "OAO Cloud DB Production"
)

# Capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# ------------------------------------------------ Load Baseline Data ------------------------------------------------

baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>%
  collect() %>%
  filter(FACILITY_MSX != "MSSM") %>%
  mutate(
    SERVICE_DATE = as.Date(
      SERVICE_DATE,
      format = "%Y%m%d"
    ),
    
    SERVICE_MONTH = lubridate::floor_date(
      SERVICE_DATE,
      "month"
    ),
    
    LOC_NAME = case_when(
      LOC_NAME == "THE MOUNT SINAI HOSPITAL" ~ "MSH",
      LOC_NAME == "MOUNT SINAI QUEENS"       ~ "MSQ",
      LOC_NAME == "MOUNT SINAI BROOKLYN"     ~ "MSB",
      LOC_NAME == "MOUNT SINAI BETH ISRAEL"  ~ "MSBI",
      LOC_NAME == "MOUNT SINAI MORNINGSIDE"  ~ "MSM",
      LOC_NAME == "MOUNT SINAI WEST"         ~ "MSW",
      TRUE ~ LOC_NAME
    ),
    
    FACILITY_MSX = case_when(
      FACILITY_MSX == "BIB" ~ "MSB",
      FACILITY_MSX == "BIP" ~ "MSBI",
      FACILITY_MSX == "RVT" ~ "MSW",
      FACILITY_MSX == "STL" ~ "MSM",
      TRUE ~ FACILITY_MSX
    )
  ) %>%
  filter(
    SERVICE_DATE >= as.Date("2025-01-01"),
    SERVICE_DATE <= as.Date("2025-12-31"),
    FACILITY_MSX != "MSSN"
  )

# ------------------------------------------------ Pool NA SERVICE_GROUP ------------------------------------------------

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
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSH"  ~ "MSH",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSQ"  ~ "MSQ",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSBI" ~ "MSBI",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSB"  ~ "MSB",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSM"  ~ "MSM",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSW"  ~ "MSW",
      TRUE ~ LOC_NAME
    )
  )

# ------------------------------------------------ Create Baseline UNIQUE_ID ------------------------------------------------

baseline <- baseline %>%
  mutate(
    UNIQUE_ID = paste0(
      LOC_NAME,
      ATTENDING_VERITY_REPORT_SERVICE
    )
  )

# ------------------------------------------------ Baseline Encounter Counts ------------------------------------------------

baseline_counts <- baseline %>%
  filter(
    !is.na(UNIQUE_ID),
    UNIQUE_ID != ""
  ) %>%
  group_by(
    UNIQUE_ID
  ) %>%
  summarise(
    BASELINE_ENCOUNTERS = n_distinct(
      ENCOUNTER_NO
    ),
    .groups = "drop"
  )

# ------------------------------------------------ Volume Projection Path ------------------------------------------------

volume_projection_dir <- paste0(
  cap_dir,
  "Mapping Info/volume projections/"
)

# ------------------------------------------------ CON Files ------------------------------------------------

vol_projections_CON <- list(
  "2026" = "BCG_2026_CON.csv",
  "2027" = "BCG_2027_CON.csv",
  "2028" = "BCG_2028_CON.csv",
  "2029" = "BCG_2029_CON.csv",
  "2030" = "BCG_2030_CON.csv"
)

# ------------------------------------------------ AGG Files ------------------------------------------------

vol_projections_AGG <- list(
  "2026" = "BCG_2026_AGG.csv",
  "2027" = "BCG_2027_AGG.csv",
  "2028" = "BCG_2028_AGG.csv",
  "2029" = "BCG_2029_AGG.csv",
  "2030" = "BCG_2030_AGG.csv"
)

# ------------------------------------------------ Projection Function ------------------------------------------------

create_projection_output <- function(file_list) {
  
  # ------------------------------------------------ Read 2026 ------------------------------------------------
  
  data_2026 <- readr::read_csv(
    paste0(
      volume_projection_dir,
      file_list[["2026"]]
    ),
    show_col_types = FALSE
  ) %>%
    select(
      HOSPITAL,
      VERITY_REPORT_SERVICE_MSX,
      PERCENT,
      UNIQUE_ID
    ) %>%
    rename(
      PERCENT_2026 = PERCENT
    )
  
  # ------------------------------------------------ Read 2027 ------------------------------------------------
  
  data_2027 <- readr::read_csv(
    paste0(
      volume_projection_dir,
      file_list[["2027"]]
    ),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2027 = PERCENT
    )
  
  # ------------------------------------------------ Read 2028 ------------------------------------------------
  
  data_2028 <- readr::read_csv(
    paste0(
      volume_projection_dir,
      file_list[["2028"]]
    ),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2028 = PERCENT
    )
  
  # ------------------------------------------------ Read 2029 ------------------------------------------------
  
  data_2029 <- readr::read_csv(
    paste0(
      volume_projection_dir,
      file_list[["2029"]]
    ),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2029 = PERCENT
    )
  
  # ------------------------------------------------ Read 2030 ------------------------------------------------
  
  data_2030 <- readr::read_csv(
    paste0(
      volume_projection_dir,
      file_list[["2030"]]
    ),
    show_col_types = FALSE
  ) %>%
    select(
      UNIQUE_ID,
      PERCENT
    ) %>%
    rename(
      PERCENT_2030 = PERCENT
    )
  
  # ------------------------------------------------ Combine All Years ------------------------------------------------
  
  projection_data <- data_2026 %>%
    left_join(
      data_2027,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      data_2028,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      data_2029,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      data_2030,
      by = "UNIQUE_ID"
    ) %>%
    left_join(
      baseline_counts,
      by = "UNIQUE_ID"
    )
  
  # ------------------------------------------------ Keep Services With Baseline Encounters ------------------------------------------------
  
  projection_data <- projection_data %>%
    filter(
      !is.na(BASELINE_ENCOUNTERS),
      BASELINE_ENCOUNTERS > 0
    )
  
  # ------------------------------------------------ Identify Services With No Projected Change ------------------------------------------------
  
  projection_data <- projection_data %>%
    mutate(
      ALL_PERCENT_ZERO =
        coalesce(PERCENT_2026, 0) == 0 &
        coalesce(PERCENT_2027, 0) == 0 &
        coalesce(PERCENT_2028, 0) == 0 &
        coalesce(PERCENT_2029, 0) == 0 &
        coalesce(PERCENT_2030, 0) == 0
    )
  
  # ------------------------------------------------ Calculate 2026-2030 ------------------------------------------------
  
  projection_data <- projection_data %>%
    mutate(
      
      # 2026
      `2026` = case_when(
        
        # If all projection percentages are zero,
        # preserve baseline encounters unchanged
        ALL_PERCENT_ZERO ~ as.numeric(
          BASELINE_ENCOUNTERS
        ),
        
        # Otherwise if 2026 itself is zero or missing,
        # preserve the baseline for 2026
        is.na(PERCENT_2026) |
          PERCENT_2026 == 0 ~ as.numeric(
            BASELINE_ENCOUNTERS
          ),
        
        # Otherwise apply 2026 growth
        TRUE ~ BASELINE_ENCOUNTERS +
          (
            PERCENT_2026 *
              BASELINE_ENCOUNTERS
          )
      ),
      
      
      # 2027
      `2027` = case_when(
        
        # All years zero = keep original baseline
        ALL_PERCENT_ZERO ~ as.numeric(
          BASELINE_ENCOUNTERS
        ),
        
        # No 2027 change = carry 2026 forward
        is.na(PERCENT_2027) |
          PERCENT_2027 == 0 ~ `2026`,
        
        # Otherwise apply 2027 growth to 2026
        TRUE ~ `2026` +
          (
            PERCENT_2027 *
              `2026`
          )
      ),
      
      
      # 2028
      `2028` = case_when(
        
        ALL_PERCENT_ZERO ~ as.numeric(
          BASELINE_ENCOUNTERS
        ),
        
        is.na(PERCENT_2028) |
          PERCENT_2028 == 0 ~ `2027`,
        
        TRUE ~ `2027` +
          (
            PERCENT_2028 *
              `2027`
          )
      ),
      
      
      # 2029
      `2029` = case_when(
        
        ALL_PERCENT_ZERO ~ as.numeric(
          BASELINE_ENCOUNTERS
        ),
        
        is.na(PERCENT_2029) |
          PERCENT_2029 == 0 ~ `2028`,
        
        TRUE ~ `2028` +
          (
            PERCENT_2029 *
              `2028`
          )
      ),
      
      
      # 2030
      `2030` = case_when(
        
        ALL_PERCENT_ZERO ~ as.numeric(
          BASELINE_ENCOUNTERS
        ),
        
        is.na(PERCENT_2030) |
          PERCENT_2030 == 0 ~ `2029`,
        
        TRUE ~ `2029` +
          (
            PERCENT_2030 *
              `2029`
          )
      )
    )
  
  # ------------------------------------------------ Round Values ------------------------------------------------
  
  projection_data <- projection_data %>%
    mutate(
      
      across(
        c(
          PERCENT_2026,
          PERCENT_2027,
          PERCENT_2028,
          PERCENT_2029,
          PERCENT_2030
        ),
        ~ round(.x, 4)
      ),
      
      across(
        c(
          `2026`,
          `2027`,
          `2028`,
          `2029`,
          `2030`
        ),
        ~ round(.x, 2)
      )
    )
  
  # ------------------------------------------------ Organize Output ------------------------------------------------
  
  projection_data <- projection_data %>%
    select(
      HOSPITAL,
      VERITY_REPORT_SERVICE_MSX,
      UNIQUE_ID,
      BASELINE_ENCOUNTERS,
      
      PERCENT_2026,
      PERCENT_2027,
      PERCENT_2028,
      PERCENT_2029,
      PERCENT_2030,
      
      ALL_PERCENT_ZERO,
      
      `2026`,
      `2027`,
      `2028`,
      `2029`,
      `2030`
    ) %>%
    arrange(
      HOSPITAL,
      VERITY_REPORT_SERVICE_MSX
    )
  
  # ------------------------------------------------ Add Total Row ------------------------------------------------
  
  total_row <- projection_data %>%
    summarise(
      
      BASELINE_ENCOUNTERS = sum(
        BASELINE_ENCOUNTERS,
        na.rm = TRUE
      ),
      
      across(
        c(
          `2026`,
          `2027`,
          `2028`,
          `2029`,
          `2030`
        ),
        ~ round(
          sum(.x, na.rm = TRUE),
          2
        )
      )
    )
  
  projection_data <- bind_rows(
    projection_data,
    total_row
  )
  
  return(
    projection_data
  )
}

# ------------------------------------------------ Create CON Dataframe ------------------------------------------------

CON_output <- create_projection_output(
  vol_projections_CON
)

# ------------------------------------------------ Create AGG Dataframe ------------------------------------------------

AGG_output <- create_projection_output(
  vol_projections_AGG
)

# ------------------------------------------------ View Dataframes ------------------------------------------------

View(CON_output)

View(AGG_output)


