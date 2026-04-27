rm(list = ls())
library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
# library(gdtools)
# library(kableExtra)
library(kableExtra, "~/R/x86_64-pc-linux-gnu-library/4.2")
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(gsubfn)
library(tidyr)
library(pool)
library(DBI)
library(odbc)
library(dbplyr)
library(glue)
library(assertr)
library(doParallel)
library(readr)
library(zip)
library(here)
library(hms)
library(ggplot2)
library(patchwork)
library(grid)
library(ggtext)
library(tidyverse)
library(shadowtext)
library(VennDiagram)
library(scales)




# --------------------- Source functions and Constants ---------------------
# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# mshs colors and theme ----
mshs_colors <- c("#221F72", "#00AEFF", "#D80B8C", "#7F7F7F", "#000000", 
                 "#800080", "#FFFF00", "#CC0000", "#38761D", "#F39C12")
mshs_theme <- theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.title = element_text(size = 12),
                    legend.text = element_text(size = 8),
                    strip.text = element_text(size = 12, face = "bold"),
                    legend.margin = margin(l = 50,r = 50),
                    panel.grid = element_blank())


# --------------------- Table Names ---------------------
ip_or_master_table_name <- 'IPCAP_OR_CASE_DATA'
or_case_details_table_name <- 'MS_INSIGHT.OR_QUALITY_DASHBOARD_CASE_DETAILS'
or_case_room_utilization_table_name <- 'MS_INSIGHT.OR_QUALITY_ROOM_UTIL_V'

# --------------------- Filters ---------------------
# current_date <- Sys.Date()
# sched_date <- '2024-09-01'
sched_start_date <- '2025-01-01'
sched_end_date <- '2025-12-31'
status <- 'Completed'
# facilities <- "('MSH','RVT','STL')"
# room_exclusion_list <- "('MSW OR 23','MSM OR 08','MSM OR 15')"

# --------------------- Queries ---------------------

ip_or_cases <- glue("SELECT ip_or.*,util_or.HOLIDAY_YN
                    FROM {ip_or_master_table_name} ip_or
                    LEFT JOIN {or_case_room_utilization_table_name} util_or
                         ON util_or.LOG_ID = ip_or.OR_CASE_ID 
                         AND util_or.HOLIDAY_YN = 'N'
                    WHERE SURGERY_DATE BETWEEN TO_DATE('{sched_start_date}','YYYY-MM-DD') AND TO_DATE('{sched_end_date}','YYYY-MM-DD');")


all_or_cases <- glue("SELECT or_cases.*,util_or.HOLIDAY_YN
                     FROM {or_case_details_table_name} or_cases
                     LEFT JOIN {or_case_room_utilization_table_name} util_or 
                         ON util_or.LOG_ID = or_cases.OR_CASE_ID 
                         AND util_or.HOLIDAY_YN = 'N'
                     WHERE SURGERY_DATE BETWEEN TO_DATE('{sched_start_date}','YYYY-MM-DD') AND TO_DATE('{sched_end_date}','YYYY-MM-DD');")




# --------------------- Connect to database and query the data ---------------------
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)
dbExecute(conn, "ALTER SESSION SET TIME_ZONE = 'America/New_York'")
ip_or_cases_data <- dbGetQuery(conn,ip_or_cases)
all_or_cases_data <- dbGetQuery(conn,all_or_cases)
dbDisconnect(conn)


all_or_cases_data <- all_or_cases_data %>%
  rename(MSMRN = PAT_MRN_ID)

# ---- OR Volume Analysis ----

or_cases_ip <- unique(ip_or_cases_data$OR_CASE_ID)
or_cases_all <- unique(all_or_cases_data$OR_CASE_ID)


venn_list <- list(
  # orcases_using_drg_ip = or_cases_drg_ip,
  or_cases_ip_exclusive = or_cases_ip,
  or_cases_all_cases = or_cases_all
  )

myCol <- mshs_colors[1:2]



venn.diagram(
  x = venn_list,
  category.names = names(venn_list),
  filename = paste0(cap_dir, "Adhoc/MS Brain Health/Output/venn_ip_or_overlap.png"),
  output = TRUE,
  
  # Output features — bump size up; 480x480 @ 300dpi is tiny for a 5-set diagram
  imagetype = "png",
  height = 2400,
  width = 2400,
  resolution = 300,
  compression = "lzw",
  
  # Circles / ellipses
  lwd = 2,
  lty = "blank",
  fill = myCol,
  
  # Numbers
  cex = 0.7,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names — must have length 5 for a 5-set Venn
  cat.cex = 0.8,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(-10, -10,-10, -10),
  # cat.dist = c(0.22, 0.22, 0.22, 0.22),
  cat.fontfamily = "sans",
  
  # rotation = 1 is only valid for 3-set diagrams; drop it or use rotation.degree
  rotation.degree = 0,
  
  margin = 0.15
)


# ---- OR Room Demand Estimation ----


procedure_minutes <- all_or_cases_data %>%
  distinct(OR_CASE_ID, MSMRN, .keep_all = TRUE) %>%
  mutate(
    PATIENT_IN_ROOM_DTTM  = as.POSIXct(PATIENT_IN_ROOM_DTTM),
    PATIENT_OUT_ROOM_DTTM = as.POSIXct(PATIENT_OUT_ROOM_DTTM),
    TURNOVER_FROM_PRIOR_CASE =  if_else(is.na(TURNOVER_FROM_PRIOR_CASE),0,as.numeric(TURNOVER_FROM_PRIOR_CASE)),
    procedure_minutes     = as.numeric(difftime(PATIENT_OUT_ROOM_DTTM,
                                                PATIENT_IN_ROOM_DTTM,
                                                units = "mins")),
    procedure_and_tat = procedure_minutes+TURNOVER_FROM_PRIOR_CASE,
    surgery_month = lubridate::floor_date(as.Date(PATIENT_IN_ROOM_DTTM), "month"),
    surgery_start_hour = hour(lubridate::ymd_hms(as_datetime(PATIENT_IN_ROOM_DTTM))),
    OR_CLASSIFICATION = if_else(OR_CASE_ID %in% or_cases_ip, "IP", "NON_IP")
  ) %>%
  filter(procedure_minutes > 0) %>%
  select(OR_CASE_ID,
         HOSPITAL,
         SURGERY_DATE,
         PATIENT_IN_ROOM_DTTM,
         PATIENT_OUT_ROOM_DTTM,
         TURNOVER_FROM_PRIOR_CASE,
         procedure_minutes,
         procedure_and_tat,
         surgery_month,
         surgery_start_hour,
         SURGEON_NPI,
         PRIMARY_PROC_CODE, 
         PRIMARY_PROCEDURE,
         OR_CLASSIFICATION)%>%
  filter(PATIENT_IN_ROOM_DTTM < PATIENT_OUT_ROOM_DTTM)


# hourly_calculations <- all_or_cases_data %>%
#   filter(!is.na(PATIENT_IN_ROOM_DTTM), !is.na(PATIENT_OUT_ROOM_DTTM)) %>%
#   distinct(OR_CASE_ID, MSMRN, .keep_all = TRUE) %>%
#   filter(PATIENT_IN_ROOM_DTTM < PATIENT_OUT_ROOM_DTTM)
# 
# hourly_calculations <- hourly_calculations%>%
#   mutate( TURNOVER_FROM_PRIOR_CASE =  if_else(is.na(TURNOVER_FROM_PRIOR_CASE),0,as.numeric(TURNOVER_FROM_PRIOR_CASE)),
#           PATIENT_OUT_ROOM_DTTM1 = PATIENT_OUT_ROOM_DTTM + minutes(TURNOVER_FROM_PRIOR_CASE)) %>%
#   rowwise() %>%
#   mutate(
#     hour_bucket = list(
#       seq(floor_date(PATIENT_IN_ROOM_DTTM,  "hour"),
#           floor_date(PATIENT_OUT_ROOM_DTTM1, "hour"),
#           by = "1 hour")
#     )
#   ) %>%
#   ungroup() %>% 
#   unnest(hour_bucket) %>%
#   mutate(
#     hour_end        = hour_bucket + hours(1),
#     minutes_in_hour = as.numeric(
#       difftime(
#         pmin(PATIENT_OUT_ROOM_DTTM1, hour_end),
#         pmax(PATIENT_IN_ROOM_DTTM,  hour_bucket),
#         units = "mins"
#       )
#     ),
#     surgery_hour = hour(hour_bucket)
#   ) %>%
#   select(MSMRN, OR_CASE_ID, SURGERY_DATE, surgery_hour, hour_bucket, minutes_in_hour) %>%
#   distinct()


# ---- Plots ----


# Monthly Volume Demand Plot ----

volume_monthly <- procedure_minutes %>%
  group_by(HOSPITAL,surgery_month,OR_CLASSIFICATION) %>%
  summarise(Volume = n_distinct(OR_CASE_ID)) %>%
  filter(!is.na(HOSPITAL))


p1 <- ggplot(volume_monthly, aes(x = surgery_month, y = Volume, fill = OR_CLASSIFICATION)) +
  geom_col() +
  facet_wrap(~ HOSPITAL) +
  scale_fill_manual(values = c(IP = mshs_colors[1], NON_IP = mshs_colors[2])) +
  scale_y_continuous(labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  labs(title = "Surgical Volume by Hospital and Month",
       x = NULL, y = "Case Volume", fill = "Classification") +
  mshs_theme

print(p1)
ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/SurgicalVolumeSiteandClassification.png"),
       p1, width = 12, height = 6, dpi = 150)

# Daily Volume Demand Plot ----
volume_daily <- procedure_minutes %>%
  group_by(HOSPITAL,SURGERY_DATE,OR_CLASSIFICATION) %>%
  summarise(Volume = n_distinct(OR_CASE_ID)) %>%
  filter(!is.na(HOSPITAL)) %>%
  mutate(day_type = ifelse(lubridate::wday(SURGERY_DATE) %in% c(1, 7),
                           "Weekend", "Weekday"))
  



p2 <-ggplot(volume_daily, aes(x = OR_CLASSIFICATION, y = Volume, fill = OR_CLASSIFICATION)) +
  geom_boxplot(
    width         = 0.6,
    alpha         = 0.75,
    outlier.size  = 1.2,
    outlier.alpha = 0.5
  ) +
  geom_jitter(width = 0.15, size = 0.4, alpha = 0.1, color = "gray20") +
  facet_wrap(~ HOSPITAL, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = c(IP = mshs_colors[1], NON_IP = mshs_colors[2])) +
  scale_y_continuous(labels = label_comma(accuracy = 1)) +
  labs(
    title = "Daily Case Volume Distribution by Hospital",
    x     = NULL,
    y     = "Daily Case Volume"
  ) +
  mshs_theme

print(p2)
ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/DailySurgicalVolumeSiteandClassification.png"),
       p2, width = 12, height = 6, dpi = 150)



# Room Demand ----


room_demand <- procedure_minutes %>%
  filter(!is.na(procedure_and_tat)) %>%
  filter(!is.na(HOSPITAL)) %>%
  # filter(surgery_month < as.Date('2026-01-01','YYYY-MM-DD')) %>%
  group_by(HOSPITAL, surgery_month) %>%
  summarise(
    n_cases            = n(),
    total_room_minutes = sum(procedure_and_tat, na.rm = TRUE),
    n_operating_days   = n_distinct(SURGERY_DATE),
    .groups            = "drop"
  ) %>%
  mutate(
    daily_room_minutes = total_room_minutes / n_operating_days,
    month_label        = format(as.Date(surgery_month), "%b %Y"),
    month_label        = factor(month_label,
                                levels = format(sort(unique(as.Date(surgery_month))), "%b %Y"))
  ) %>%
  filter(!month_label %in% c("Jan 2026"))


shifts <- tibble(
  scenario             = c("10-hour shift", "8-hour shift", "6-hour shift"),
  shift_hours          = c(10, 8, 6),
  shift_label          = c("8 AM – 6 PM", "8 AM – 4 PM", "8 AM – 2 PM"),
  minutes_per_room_day = shift_hours*60,
  bar_color            = mshs_colors[c(1, 2, 3)]
) %>%
  mutate(scenario = factor(scenario,
                           levels = c("10-hour shift", "8-hour shift", "6-hour shift")))


demand_table <- room_demand %>%
  crossing(shifts) %>%
  mutate(
    rooms_exact   = daily_room_minutes / minutes_per_room_day,
    rooms_rounded = ceiling(rooms_exact)
  )


p3 <- ggplot(demand_table %>% filter(surgery_month <= as.Date('2025-06-01',"%Y-%m-%d")), aes(y = scenario, x = shift_hours, fill = scenario)) +
  geom_col(width = 0.65, alpha = 0.9) +
  
  # Shift window inside the bar
  geom_text(aes(x = shift_hours / 2, label = shift_label),
            color = "white", fontface = "bold", size = 2.8) +
  
  # Rooms-needed label past the bar
  geom_text(aes(x = shift_hours + 0.4,
                label = paste0(rooms_rounded, " rooms (", round(rooms_exact, 1), ")")),
            hjust = 0, size = 2.8, fontface = "bold", color = "gray15") +
  
  scale_fill_manual(values = setNames(shifts$bar_color, shifts$scenario)) +
  scale_x_continuous(limits = c(0, 16), expand = c(0, 0)) +
  facet_grid(HOSPITAL ~ month_label, switch = "y") +
  labs(
    title    = "OR Room Demand by Hospital, Month, and Shift Length",
    subtitle = "Bar width = shift hours  |  Label = rooms needed (exact)",
    x        = NULL, y = NULL
  ) +
  theme_gray(base_size = 10) +
  theme(
    plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "gray30"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_text(size = 8),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    legend.position  = "none",
    strip.text.x     = element_text(face = "bold", size = 10),
    strip.text.y.left = element_text(face = "bold", size = 10, angle = 0),
    strip.placement  = "outside",
    panel.spacing    = unit(0.6, "lines")
  )
print(p3)

ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/MonthlyRoomDemandJanJun2025.png"),
       p3, width = 20, height = 12, dpi = 150)


p4 <- ggplot(demand_table %>% filter(surgery_month >= as.Date('2025-07-01',"%Y-%m-%d")), aes(y = scenario, x = shift_hours, fill = scenario)) +
  geom_col(width = 0.65, alpha = 0.9) +
  
  # Shift window inside the bar
  geom_text(aes(x = shift_hours / 2, label = shift_label),
            color = "white", fontface = "bold", size = 2.8) +
  
  # Rooms-needed label past the bar
  geom_text(aes(x = shift_hours + 0.4,
                label = paste0(rooms_rounded, " rooms (", round(rooms_exact, 1), ")")),
            hjust = 0, size = 2.8, fontface = "bold", color = "gray15") +
  
  scale_fill_manual(values = setNames(shifts$bar_color, shifts$scenario)) +
  scale_x_continuous(limits = c(0, 16), expand = c(0, 0)) +
  facet_grid(HOSPITAL ~ month_label, switch = "y") +
  labs(
    title    = "OR Room Demand by Hospital, Month, and Shift Length",
    subtitle = "Bar width = shift hours  |  Label = rooms needed (exact)",
    x        = NULL, y = NULL
  ) +
  theme_gray(base_size = 10) +
  theme(
    plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "gray30"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_text(size = 8),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    legend.position  = "none",
    strip.text.x     = element_text(face = "bold", size = 10),
    strip.text.y.left = element_text(face = "bold", size = 10, angle = 0),
    strip.placement  = "outside",
    panel.spacing    = unit(0.6, "lines")
  )
print(p4)
ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/MonthlyRoomDemandJulDec2025.png"),
       p4, width = 20, height = 12, dpi = 150)