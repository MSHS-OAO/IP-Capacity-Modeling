source('functions/or_constants_functions.R')


# Get volume projection data ----
file_location <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/archive/"
file_name <- "daily_demand_encounter.xlsx"


data_baseline_ip <- read_xlsx(paste0(file_location,file_name),sheet = 'baseline')
data_volume_projections_ip <- read_xlsx(paste0(file_location,file_name),sheet = 'scenario')


# max admin date, mrn list and min discharge date
min_admin_date <- format(min(data_baseline_ip$ADMIT_DT_SRC),'%Y-%m-%d')
max_discharge_date <- format(max(data_baseline_ip$DSCH_DT_SRC),'%Y-%m-%d')
mrn_list_ip <- data_baseline_ip %>%
  mutate( MSMRN = trimws(MSMRN)) %>%
  select(MSMRN,ADMIT_DT_SRC,DSCH_DT_SRC) %>%
  distinct()


# Get OR data based on ipdata ----
or_cases_baseline <- get_or_data(sched_start_date = min_admin_date, sched_end_date = max_discharge_date,status = 'Completed', mrn_list = mrn_list_ip)


# Calculate baseline metrics ----
summary_metrics_baseline <- summary_metrics(or_cases_baseline)