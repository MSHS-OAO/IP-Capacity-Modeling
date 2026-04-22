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
library(readr)
library(VennDiagram)
library(grid)
library(scales)

mshs_colors <- c("#221F72", "#00AEFF", "#D80B8C", "#7F7F7F", "#000000", 
                 "#800080", "#FFFF00", "#CC0000", "#38761D", "#F39C12")


msh_theme <- theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 12, face = "bold"),
      panel.spacing = unit(1.5, "lines"))



neuro_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/Adhoc/MS Brain Health/Mappings"
dsn <- "OAO Cloud DB Production"

conn <- dbConnect(odbc(), dsn)
ip_baseline <- tbl(conn, "IPCAP_OR_CASE_DATA") %>% collect()
dbDisconnect(conn)

# filter 
conn <- dbConnect(odbc(), dsn)
or_baseline <- tbl(conn, in_schema("MS_INSIGHT", "OR_QUALITY_DASHBOARD_CASE_DETAILS")) %>%
  collect() %>%
  filter(
    grepl("Completed", CASE_STATUS, ignore.case = TRUE),
    year(SURGERY_DATE) == 2025
  )
dbDisconnect(conn)

#load cpt, drg, and npi columns from csv/xlsx files
drg_sheet <- read_csv(file.path(neuro_dir, "Neurosurgery DRG.csv"))
drg_list <- trimws(as.character(drg_sheet$`MS-DRG`))

cpt_sheet <- read_csv(file.path(neuro_dir, "Neurosurgery CPT.csv"))
cpt_list <- trimws(as.character(cpt_sheet$`CPT Code`))

npi_sheet <- read_excel(file.path(neuro_dir, "Faculty and NPI.xlsx"))
npi_list <- trimws(as.character(npi_sheet$NPI))

#study population with cpt match and drg mismatch
cpt_npi__match_drg_mismatch <- ip_baseline %>%
  filter(
    (PRIMARY_PROC_CODE %in% cpt_list | SURGEON_NPI %in% npi_list) &
      !(MSDRG_CD_SRC %in% drg_list) &
      FACILITY_MSX %in% c("MSH", "RVT", "STL")
  ) %>%
  select(MSDRG_CD_SRC, MSDRG_DESC_MSX) %>%
  count(MSDRG_CD_SRC, MSDRG_DESC_MSX,
        sort = TRUE, name = "NUM_OF_PROCEDURES") %>%
  rename(
    MISMATCHED_DRG = MSDRG_CD_SRC
  ) %>%
  filter(!is.na(MISMATCHED_DRG))

#study population with drg match and cpt mismatch
npi_match_cpt_mismatch <- or_baseline %>%
  filter(
    SURGEON_NPI %in% npi_list &
      !(PRIMARY_PROC_CODE %in% cpt_list) &
      HOSPITAL  %in% c('MSW', 'MSH', 'MSM') 
  ) %>%
  select( PRIMARY_PROC_CODE, PRIMARY_PROCEDURE) %>%
  count( PRIMARY_PROC_CODE, PRIMARY_PROCEDURE, sort = TRUE, name = "NUM_OF_PROCEDURES") %>%
  rename(
    MISMATCHED_CPT = PRIMARY_PROC_CODE
  ) %>%
  filter(!is.na(MISMATCHED_CPT))

df <- or_baseline %>%
  filter(
    PRIMARY_PROC_CODE %in% cpt_list | SURGEON_NPI %in% npi_list
  ) %>%
  mutate(
    PAT_DOB = as.Date(PAT_DOB),
    SURGERY_DATE = as.Date(SURGERY_DATE),
    AGE_ENC = as.integer((SURGERY_DATE - PAT_DOB) / 365.25)
  ) %>%
  filter(!is.na(AGE_ENC)) %>%
  mutate(
    AGE_BIN = cut(
      AGE_ENC,
      breaks = c(-Inf, 18, 37, 56, 75, Inf),
      right = TRUE,
      labels = c("0-18", "19-37", "38-56", "57-75", "76+")
    )
  )

# bar chart + percentages , SURGERY_DATE, CASE COMPLETED,  OR CASE, 
p <- ggplot(df, aes(x = AGE_BIN)) +
  geom_bar(fill = mshs_colors[1]) +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count / sum(count)), accuracy = 0.1)),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Age Distribution",
    x = "Age Group",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank()
  )

print(p)

ip_drg <- ip_baseline %>%
  filter(
    MSDRG_CD_SRC %in% drg_list &
      FACILITY_MSX %in% c("MSH", "RVT", "STL")
  ) %>% select(OR_CASE_ID)

or_npi <- or_baseline %>%
  filter(
    SURGEON_NPI %in% npi_list &
      HOSPITAL %in%  c('MSW', 'MSH', 'MSM')
  ) %>% select(OR_CASE_ID)

or_cpt <- or_baseline %>%
  filter(
    PRIMARY_PROC_CODE %in% cpt_list &
      HOSPITAL %in%  c('MSW', 'MSH', 'MSM')
  ) %>% select(OR_CASE_ID)

drg_cases <- unique(na.omit(ip_drg$OR_CASE_ID))
cpt_cases <- unique(na.omit(c(or_npi$OR_CASE_ID, or_cpt$OR_CASE_ID)))

both <- intersect(drg_cases, cpt_cases)
drg_only <- setdiff(drg_cases, cpt_cases)
cpt_only <- setdiff(cpt_cases, drg_cases)

total_unique <- length(union(drg_cases, cpt_cases))

drg_only_n <- length(drg_only)
cpt_only_n <- length(cpt_only)
both_n <- length(both)

drg_only_pct <- round(drg_only_n / total_unique * 100, 1)
cpt_only_pct <- round(cpt_only_n / total_unique * 100, 1)
both_pct <- round(both_n / total_unique * 100, 1)

summary_tbl <- tibble(
  GROUP = c("DRG Only", "CPT Only", "Both"),
  COUNT = c(drg_only_n, cpt_only_n, both_n),
  PERCENT = c(drg_only_pct, cpt_only_pct, both_pct)
)

print(summary_tbl)

grid.newpage()

venn.plot <- draw.pairwise.venn(
  area1 = length(drg_cases),
  area2 = length(cpt_cases),
  cross.area = both_n,
  category = c("DRG", "CPT"),
  fill = c("#5cd3ff", "#f75dbe"),
  alpha = c(0.5, 0.5),
  lty = "blank",
  cex = 1.5,
  cat.cex = 1.5,
  print.mode = c("raw", "percent"),
  sigdigs = 3
)


