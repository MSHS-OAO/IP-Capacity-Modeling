#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DBI)
library(dplyr)
library(htmltools)
library(odbc)
library(tidyverse)
library(stringr)
library(kableExtra)
library(DT)
library(shinyWidgets)


dsn <- "OAO Cloud DB Production"
conn <- dbConnect(drv = odbc::odbc(),
                  dsn = dsn)
baseline <- tbl(conn, "IPCAP_BEDCHARGES") %>%
  select(EXTERNAL_NAME,
         QUANTITY,
         ENCOUNTER_NO,
         SERVICE_DATE,
         FACILITY_MSX,
         VERITY_DIV_DESC_SRC,
         SERVICE_GROUP,
         ADMIT_TYPE_DESC_SRC,
         PRINCIPAL_SURGEON_NAME_MSX
  ) %>%
  collect() %>%
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
  ) %>%
  filter(!(is.na(SERVICE_GROUP) | is.na(PRINCIPAL_SURGEON_NAME_MSX ) | is.na(VERITY_DIV_DESC_SRC) | ADMIT_TYPE_DESC_SRC == "INFORMATION NOT AVAILABLE"))


dbDisconnect(conn)




#calculating period in dataset in days
min_date <- min(baseline$SERVICE_DATE)
max_date <- max(baseline$SERVICE_DATE)
diff_days <- as.numeric(difftime(max_date, min_date, units = "days"))




#dropping quantity column
baseline <- baseline %>% 
  mutate(SERVICE_GROUP = toupper(SERVICE_GROUP))



#calculating baseline with repetitive daily rows for patients/day calculations
# patients_per_day_baseline <- baseline %>%
#   arrange(SERVICE_DATE) %>%
#   distinct(ENCOUNTER_NO, SERVICE_DATE,SERVICE_GROUP, VERITY_DIV_DESC_SRC, FACILITY_MSX,ADMIT_TYPE_DESC_SRC,PRINCIPAL_SURGEON_NAME_MSX, .keep_all = TRUE)
# 

patients_per_day_baseline <- baseline %>%
  filter(!is.na(EXTERNAL_NAME)) %>%
  group_by(ENCOUNTER_NO, SERVICE_DATE, FACILITY_MSX,VERITY_DIV_DESC_SRC, SERVICE_GROUP,PRINCIPAL_SURGEON_NAME_MSX, ADMIT_TYPE_DESC_SRC) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(
    BED_CHARGES = case_when(
      BED_CHARGES > 1 ~ 1,
      TRUE ~ BED_CHARGES
    )
  ) %>%
  filter(BED_CHARGES != 0) %>%                 
  group_by(FACILITY_MSX, SERVICE_DATE, SERVICE_GROUP,VERITY_DIV_DESC_SRC, ADMIT_TYPE_DESC_SRC) %>%
  select(-BED_CHARGES)                         


#deselect quantity and external name, not needed after
baseline <- baseline %>%
  select(-QUANTITY,-EXTERNAL_NAME,-SERVICE_MONTH)


# calculating baseline for percentage calculation (only one row per encounter (first day of encounter))
baseline <- baseline %>%
  arrange(SERVICE_DATE) %>%
  distinct(ENCOUNTER_NO,FACILITY_MSX,SERVICE_GROUP, VERITY_DIV_DESC_SRC,ADMIT_TYPE_DESC_SRC,PRINCIPAL_SURGEON_NAME_MSX, .keep_all = TRUE)


# dropping unnecessary columns
baseline <- baseline %>%
  select(-ENCOUNTER_NO,-SERVICE_DATE)



# *****************************  CONSTRUCTION OF SERVICE TABLE *****************************
service_table <- baseline %>%
  count(FACILITY_MSX, SERVICE_GROUP,VERITY_DIV_DESC_SRC,ADMIT_TYPE_DESC_SRC, name = "n") %>%
  group_by(FACILITY_MSX, SERVICE_GROUP, VERITY_DIV_DESC_SRC) %>%
  mutate(
    total_in_service = sum(n),
    Percentage = 100 * n / total_in_service   
  ) %>%
  ungroup() %>%
  select(FACILITY_MSX, SERVICE_GROUP, VERITY_DIV_DESC_SRC, ADMIT_TYPE_DESC_SRC, Percentage)



# calculating avg patients/day for service table
service_table_patient_avg <- patients_per_day_baseline %>%
  count(FACILITY_MSX, SERVICE_DATE, SERVICE_GROUP, VERITY_DIV_DESC_SRC, ADMIT_TYPE_DESC_SRC, name = "n") %>%
  group_by(FACILITY_MSX, VERITY_DIV_DESC_SRC, SERVICE_GROUP,ADMIT_TYPE_DESC_SRC) %>%
  mutate(total_in_service = sum(n)) %>%
  select(-SERVICE_DATE, -n) %>%
  distinct() %>%
  mutate(
    AVG_PATIENTS_DAY = total_in_service / diff_days 
  ) %>%
  select(-total_in_service)


# join the two
service_table <- left_join(service_table, service_table_patient_avg, by = c("FACILITY_MSX", "SERVICE_GROUP","VERITY_DIV_DESC_SRC", "ADMIT_TYPE_DESC_SRC"))

#swich locations of VERITY_DIV_DESC_SRC and SERVICE_GROUP for aesthetics
service_table <- service_table %>%
  select(FACILITY_MSX, VERITY_DIV_DESC_SRC, SERVICE_GROUP, everything())




#   ***************************** CONSTRUCTION OF SURGEON TABLE   *****************************
surgeon_table <- baseline %>%
  group_by(FACILITY_MSX, SERVICE_GROUP, VERITY_DIV_DESC_SRC, PRINCIPAL_SURGEON_NAME_MSX, ADMIT_TYPE_DESC_SRC) %>%
  summarise(ROW_COUNT_BY_ADMIT_TYPE = n(), .groups = "drop_last") %>%
  mutate(Percentage = 100 * ROW_COUNT_BY_ADMIT_TYPE / sum(ROW_COUNT_BY_ADMIT_TYPE)) %>%  
  ungroup() %>%
  arrange(PRINCIPAL_SURGEON_NAME_MSX) %>%
  select(FACILITY_MSX, SERVICE_GROUP, VERITY_DIV_DESC_SRC, PRINCIPAL_SURGEON_NAME_MSX, ADMIT_TYPE_DESC_SRC, Percentage)




# calculating avg patients/day for surgeon table
surgeon_table_patient_avg_2 <- patients_per_day_baseline %>%
  count(FACILITY_MSX, SERVICE_DATE, SERVICE_GROUP, PRINCIPAL_SURGEON_NAME_MSX, VERITY_DIV_DESC_SRC, ADMIT_TYPE_DESC_SRC,name = "n") %>%
  group_by(FACILITY_MSX, VERITY_DIV_DESC_SRC,PRINCIPAL_SURGEON_NAME_MSX, ADMIT_TYPE_DESC_SRC ,SERVICE_GROUP) %>%
  mutate(total_in_service = sum(n)) %>%
  select (-SERVICE_DATE,-n) %>%
  distinct() %>%
  mutate(AVG_PATIENTS_DAY = total_in_service / diff_days) %>%  
  select (-total_in_service)


# join the two
surgeon_table <- left_join(surgeon_table, surgeon_table_patient_avg_2, by = c("FACILITY_MSX", "SERVICE_GROUP","PRINCIPAL_SURGEON_NAME_MSX","VERITY_DIV_DESC_SRC", "ADMIT_TYPE_DESC_SRC"))

#switch locations of VERITY_DIV_DESC_SRC and SERVICE_GROUP for aesthetics
surgeon_table <- surgeon_table %>%
  select(FACILITY_MSX, VERITY_DIV_DESC_SRC, SERVICE_GROUP, everything())




hospital_choices <- surgeon_table %>% select(FACILITY_MSX) %>% summarise(HOSPITAL = unique(FACILITY_MSX))
service_choices <- surgeon_table %>% select(VERITY_DIV_DESC_SRC) %>% summarise(SERVICE = unique(VERITY_DIV_DESC_SRC))

hospital_choices <- sort(unique(surgeon_table$FACILITY_MSX))
service_choices  <- sort(unique(surgeon_table$VERITY_DIV_DESC_SRC))



#assigning these dataframes to the global environment 
assign("baseline", baseline, envir = .GlobalEnv)
assign("patients_per_day_baseline", patients_per_day_baseline, envir = .GlobalEnv)
assign("surgeon_table", surgeon_table, envir = .GlobalEnv)
assign("service_table", service_table, envir = .GlobalEnv)
assign("hospital_choices", hospital_choices, envir = .GlobalEnv)
assign("service_choices", service_choices, envir = .GlobalEnv)
assign("diff_days", diff_days, envir = .GlobalEnv)



#  ***************************** UI  *****************************
ui <- fluidPage(
  titlePanel("Inpatient Admit Type Analysis"),
  
  tags$head(
    tags$style(HTML("
      #service_output table.dataTable thead th,
      #surgeon_output table.dataTable thead th {
        background-color:#212070; color:white; white-space:nowrap;
        font-size:18px;
      }
      #service_output table.dataTable tbody td,
      #surgeon_output table.dataTable tbody td {
        white-space:nowrap;
        font-size:16px;
      }
      #service_output caption, 
      #surgeon_output caption {
        caption-side: top; text-align:center; font-weight:700;
        color:white; background:#212070; padding:8px 10px; margin-bottom:6px;
        font-size:18px;
      }
      #service_output, 
      #surgeon_output {
        width:auto !important; display:block; margin-left:auto; margin-right:auto;
      }
      #service_output table.dataTable, 
      #surgeon_output table.dataTable {
        width:auto !important;
        margin-left:auto; margin-right:auto;
      }
    "))
  ),
  
  fluidRow(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 8px;",
    
    column(
      width = 6,
      selectInput(
        inputId = "dropdown1",
        label = "Select Hospital:",
        choices = hospital_choices,
        selected = "MSH"
      )
    ),
    
    column(
      width = 6,
      pickerInput(
        inputId = "dropdown2",
        label   = "Select Service Line:",
        choices = NULL,
        options = pickerOptions(
          liveSearch = TRUE,
          liveSearchStyle = "startsWith",   # <- prefix search
          liveSearchNormalize = TRUE,
          noneSelectedText = "Search or select a service line..."
        )
      ))
    
  ),
  
  br(),
  textOutput("selected_options"),
  br(),
  DTOutput("service_output"),
  br(),
  DTOutput("surgeon_output")
)



# SERVER
server <- function(input, output,session) {
  
  
  # update selections of service_line dropdown menu if hospital dropdown menu is changed
  observeEvent(input$dropdown1, {
    req(input$dropdown1)
    df <- surgeon_table %>%
      filter(FACILITY_MSX %in% input$dropdown1)
    
    picker_choices <- df %>%
      pull(VERITY_DIV_DESC_SRC) %>%
      unique() %>%
      sort()
    
    updatePickerInput(
      session, "dropdown2",
      choices  = picker_choices,
      selected = if (length(picker_choices)) picker_choices[1] else character(0)
    )
  }, ignoreInit = FALSE)  
  
  
  # SERVICE_OUTPUT
  output$service_output <- DT::renderDataTable({
    output_table <- service_table %>% 
      filter(FACILITY_MSX %in% input$dropdown1, VERITY_DIV_DESC_SRC %in% input$dropdown2)
    
    if (nrow(output_table) == 0) {
      return(
        datatable(
          data.frame(`No data for this selection.` = character()),
          rownames = FALSE,
          options = list(dom = 't', paging = FALSE, autoWidth = TRUE),
          escape = FALSE,
          caption = htmltools::tags$caption("Service Line Distribution", style="text-align:center;")
        )
      )
    }
    
    
    col_map <- c(
      FACILITY_MSX = "Hospital",
      SERVICE_GROUP = "Unit Type",
      VERITY_DIV_DESC_SRC = "Service Line",
      ADMIT_TYPE_DESC_SRC = "Admit Type",
      PERCENTAGE = "Percentage",
      AVG_PATIENTS_DAY = "AVG Patients/Day"
    )
    common_cols <- intersect(names(col_map), names(output_table))
    names(output_table)[match(common_cols, names(output_table))] <- unname(col_map[common_cols])
    
    pct_idx <- which(names(output_table) == "Percentage")
    
    datatable(
      output_table,
      rownames = FALSE,
      class = "stripe hover row-border order-column",
      options = list(
        paging = FALSE,    
        autoWidth  = TRUE,
        orderClasses = TRUE,
        columnDefs = if (length(pct_idx) == 1) list(list(
          targets = pct_idx - 1,
          render = JS(
            "function(data, type, row, meta){",
            "  if (data === null || data === undefined || data === '') return data;",
            "  var num = parseFloat(data);",
            "  if (isNaN(num)) return data;",
            "  if (type === 'display' || type === 'filter') {",
            "    return num.toFixed(2) + '%';",   
            "  }",
            "  return num;",                      
            "}"
          )
        )) else NULL
      ),
      caption = htmltools::tags$caption("Service Line Distribution", style="text-align:center;")
    ) |> DT::formatRound("AVG Patients/Day", 2)   |>
      formatStyle(c("Percentage", "AVG Patients/Day"),
                  backgroundColor = "#E6F8FF", fontWeight = "bold") |>
      formatStyle(c("Hospital", "Unit Type", "Service Line", "Admit Type"),
                  backgroundColor = "#f2f3f2", fontWeight = "bold") |>
      formatStyle(
        "AVG Patients/Day",
        `text-align` = "right"   
      )
  }) 
  
  
  
  # SURGEON_OUTPUT
  output$surgeon_output <- DT::renderDataTable({
    output_table <- surgeon_table %>% 
      filter(FACILITY_MSX %in% input$dropdown1, VERITY_DIV_DESC_SRC %in% input$dropdown2)
    
    if (nrow(output_table) == 0) {
      return(
        datatable(
          data.frame(`No data for this selection.` = character()),
          rownames = FALSE,
          options = list(dom = 't', paging = FALSE, autoWidth = TRUE),
          escape = FALSE,
          caption = htmltools::tags$caption("Surgeon Distribution", style="text-align:center;")
        )
      )
    }
    
    
    
    col_map <- c(
      FACILITY_MSX = "Hospital",
      SERVICE_GROUP = "Unit Type",
      VERITY_DIV_DESC_SRC = "Service Line",
      PRINCIPAL_SURGEON_NAME_MSX = "Primary Surgeon",
      ADMIT_TYPE_DESC_SRC = "Admit Type",
      PERCENTAGE = "Percentage",
      AVG_PATIENTS_DAY = "AVG Patients/Day"
    )
    common_cols <- intersect(names(col_map), names(output_table))
    names(output_table)[match(common_cols, names(output_table))] <- unname(col_map[common_cols])
    
    pct_idx <- which(names(output_table) == "Percentage")
    
    datatable(
      output_table,
      rownames = FALSE,
      class = "stripe hover row-border order-column",
      options = list(
        paging = FALSE,    
        autoWidth  = TRUE,
        orderClasses = TRUE,
        columnDefs = if (length(pct_idx) == 1) list(list(
          targets = pct_idx - 1,
          render = JS(
            "function(data, type, row, meta){",
            "  if (data === null || data === undefined || data === '') return data;",
            "  var num = parseFloat(data);",
            "  if (isNaN(num)) return data;",
            "  if (type === 'display' || type === 'filter') {",
            "    return num.toFixed(2) + '%';",   
            "  }",
            "  return num;",                      
            "}"
          )
        )) else NULL
      ),
      caption = htmltools::tags$caption("Surgeon Distribution", style="text-align:center;")
    ) |> DT::formatRound("AVG Patients/Day", 2) |>
      formatStyle(c("Percentage", "AVG Patients/Day"),
                  backgroundColor = "#E6F8FF", fontWeight = "bold") |>
      formatStyle(c("Hospital", "Unit Type", "Service Line", "Admit Type", "Primary Surgeon"),
                  backgroundColor = "#f2f3f2", fontWeight = "bold") |>
      formatStyle(
        "AVG Patients/Day",
        `text-align` = "right"   
      )
  }) 
  
}

shinyApp(ui = ui, server = server)
