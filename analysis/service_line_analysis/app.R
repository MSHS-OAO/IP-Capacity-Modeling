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
         PRINCIPAL_SURGEON_VERITY_DIV_DESC,
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
  filter(!(is.na(SERVICE_GROUP) | is.na(PRINCIPAL_SURGEON_VERITY_DIV_DESC) | is.na(PRINCIPAL_SURGEON_NAME_MSX))) %>%
  filter(!is.na(EXTERNAL_NAME)) %>%
  group_by(ENCOUNTER_NO, SERVICE_DATE, FACILITY_MSX, PRINCIPAL_SURGEON_VERITY_DIV_DESC, SERVICE_GROUP, PRINCIPAL_SURGEON_NAME_MSX, ADMIT_TYPE_DESC_SRC) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(
    BED_CHARGES = case_when(
      BED_CHARGES > 1 ~ 1,
      TRUE ~ BED_CHARGES
    )
  ) %>%
  filter(BED_CHARGES != 0) %>%
  select(-BED_CHARGES)

dbDisconnect(conn)


#replace NA surgeons with "NA" char
baseline$PRINCIPAL_SURGEON_NAME_MSX <- replace_na(baseline$PRINCIPAL_SURGEON_NAME_MSX, "NA")

# calculating period in dataset in days
min_date <- min(baseline$SERVICE_DATE)
max_date <- max(baseline$SERVICE_DATE)
diff_days <- as.numeric(difftime(max_date, min_date, units = "days")) +1



# dropping quantity column
baseline <- baseline %>% 
  mutate(SERVICE_GROUP = toupper(SERVICE_GROUP))



# calculating baseline with repetitive daily rows for patients/day calculations
patients_per_day_baseline <- baseline %>%
  group_by(FACILITY_MSX, SERVICE_DATE, SERVICE_GROUP, PRINCIPAL_SURGEON_VERITY_DIV_DESC)



# calculating baseline for percentage calculation (only one row per encounter (first day of encounter))
baseline <- baseline %>%
  arrange(SERVICE_DATE) %>%
  distinct(ENCOUNTER_NO,FACILITY_MSX,SERVICE_GROUP, PRINCIPAL_SURGEON_VERITY_DIV_DESC,PRINCIPAL_SURGEON_NAME_MSX, .keep_all = TRUE)



# dropping unnecessary columns
baseline <- baseline %>%
  select(-ENCOUNTER_NO,-SERVICE_DATE)



# *****************************  CONSTRUCTION OF SERVICE TABLE *****************************
service_table <- baseline %>%
  count(FACILITY_MSX, SERVICE_GROUP,PRINCIPAL_SURGEON_VERITY_DIV_DESC, name = "n") %>%
  group_by(FACILITY_MSX, PRINCIPAL_SURGEON_VERITY_DIV_DESC) %>%
  mutate(
    total_in_service = sum(n),
    Percentage = 100 * n / total_in_service   
  ) %>%
  ungroup() %>%
  select(FACILITY_MSX, SERVICE_GROUP, PRINCIPAL_SURGEON_VERITY_DIV_DESC, Percentage)


# service_table <- baseline %>%
#   count(FACILITY_MSX, SERVICE_GROUP,VERITY_DIV_DESC_SRC, name = "n") %>%
#   group_by(VERITY_DIV_DESC_SRC,SERVICE_GROUP) %>%
#   mutate(
#     total_in_service = sum(n),
#     Percentage = 100 * n / total_in_service   
#   ) %>%
#   ungroup() %>%
#   select(FACILITY_MSX, SERVICE_GROUP, VERITY_DIV_DESC_SRC, Percentage)





# calculating avg patients/day for service table
service_table_patient_avg <- patients_per_day_baseline %>%
  count(FACILITY_MSX, SERVICE_DATE,SERVICE_GROUP, PRINCIPAL_SURGEON_VERITY_DIV_DESC, name = "n") %>%
  group_by(FACILITY_MSX,SERVICE_GROUP, PRINCIPAL_SURGEON_VERITY_DIV_DESC) %>%
  mutate(total_in_service = sum(n)) %>%
  select(-SERVICE_DATE, -n) %>%
  distinct() %>%
  mutate(
    AVG_PATIENTS_DAY = total_in_service / diff_days 
  ) %>%
  select(-total_in_service)


# join the two
service_table <- left_join(service_table, service_table_patient_avg, by = c("FACILITY_MSX", "SERVICE_GROUP","PRINCIPAL_SURGEON_VERITY_DIV_DESC"))

#swich locations of PRINCIPAL_SURGEON_VERITY_DIV_DESC and SERVICE_GROUP for aesthetics
service_table <- service_table %>%
  select(FACILITY_MSX, PRINCIPAL_SURGEON_VERITY_DIV_DESC, SERVICE_GROUP, everything())




#   ***************************** CONSTRUCTION OF SURGEON TABLE   *****************************
surgeon_table <- baseline %>%
  group_by(FACILITY_MSX, SERVICE_GROUP, PRINCIPAL_SURGEON_VERITY_DIV_DESC, PRINCIPAL_SURGEON_NAME_MSX) %>%
  summarise(ROW_COUNT_BY_UNIT_TYPE = n(), .groups = "drop_last") %>%
  mutate(Percentage = 100 * ROW_COUNT_BY_UNIT_TYPE / sum(ROW_COUNT_BY_UNIT_TYPE)) %>%  
  ungroup() %>%
  arrange(FACILITY_MSX, PRINCIPAL_SURGEON_VERITY_DIV_DESC,SERVICE_GROUP) %>%
  select(FACILITY_MSX, SERVICE_GROUP, PRINCIPAL_SURGEON_VERITY_DIV_DESC, PRINCIPAL_SURGEON_NAME_MSX, Percentage)

# surgeon_table <- baseline %>%
#   group_by(FACILITY_MSX, SERVICE_GROUP, VERITY_DIV_DESC_SRC, PRINCIPAL_SURGEON_NAME_MSX) %>%
#   summarise(ROW_COUNT_BY_UNIT_TYPE = n(), .groups = "drop") %>%
#   group_by(FACILITY_MSX, VERITY_DIV_DESC_SRC, PRINCIPAL_SURGEON_NAME_MSX) %>%
#   mutate(Percentage = 100 * ROW_COUNT_BY_UNIT_TYPE / sum(ROW_COUNT_BY_UNIT_TYPE)) %>%
#   ungroup() %>%
#   arrange(FACILITY_MSX, VERITY_DIV_DESC_SRC, SERVICE_GROUP) %>%
#   select(FACILITY_MSX, SERVICE_GROUP, VERITY_DIV_DESC_SRC, PRINCIPAL_SURGEON_NAME_MSX, Percentage)



# calculating avg patients/day for surgeon table
surgeon_table_patient_avg <- patients_per_day_baseline %>%
  count(FACILITY_MSX, SERVICE_DATE, SERVICE_GROUP, PRINCIPAL_SURGEON_NAME_MSX, PRINCIPAL_SURGEON_VERITY_DIV_DESC,name = "n") %>%
  group_by(FACILITY_MSX, PRINCIPAL_SURGEON_VERITY_DIV_DESC,PRINCIPAL_SURGEON_NAME_MSX ,SERVICE_GROUP) %>%
  mutate(total_in_service = sum(n)) %>%
  select (-SERVICE_DATE,-n) %>%
  distinct() %>%
  mutate(AVG_PATIENTS_DAY = total_in_service / diff_days) %>%  
  select (-total_in_service)


# join the two
surgeon_table <- left_join(surgeon_table, surgeon_table_patient_avg, by = c("FACILITY_MSX", "SERVICE_GROUP","PRINCIPAL_SURGEON_NAME_MSX","PRINCIPAL_SURGEON_VERITY_DIV_DESC"))

#switch locations of PRINCIPAL_SURGEON_VERITY_DIV_DESC and SERVICE_GROUP for aesthetics
surgeon_table <- surgeon_table %>%
  select(FACILITY_MSX, PRINCIPAL_SURGEON_VERITY_DIV_DESC, SERVICE_GROUP, everything())



hospital_choices <- sort(unique(surgeon_table$FACILITY_MSX))
service_choices  <- sort(unique(surgeon_table$PRINCIPAL_SURGEON_VERITY_DIV_DESC))
unit_choices <- sort(unique(surgeon_table$SERVICE_GROUP))


#assigning these dataframes to the global environment 
assign("baseline", baseline, envir = .GlobalEnv)
assign("patients_per_day_baseline", patients_per_day_baseline, envir = .GlobalEnv)
assign("surgeon_table", surgeon_table, envir = .GlobalEnv)
assign("service_table", service_table, envir = .GlobalEnv)
assign("hospital_choices", hospital_choices, envir = .GlobalEnv)
assign("service_choices", service_choices, envir = .GlobalEnv)
assign("unit_choices", unit_choices, envir = .GlobalEnv)
assign("diff_days", diff_days, envir = .GlobalEnv)



#  ***************************** UI  *****************************
ui <- fluidPage(
  titlePanel("Inpatient Unit Type Analysis"),
  
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



# SERVER
server <- function(input, output,session) {
  
  
  # update selections of service_line dropdown menu if hospital dropdown menu is changed
  observeEvent(input$dropdown1, {
    req(input$dropdown1)
    
    df1 <- surgeon_table %>% filter(FACILITY_MSX %in% input$dropdown1)
    
    dept_choices <- df1 %>% pull(PRINCIPAL_SURGEON_VERITY_DIV_DESC) %>% unique() %>% sort()
    prev_dept <- isolate(input$dropdown2)
    dept_selected <- if (!is.null(prev_dept) && prev_dept %in% dept_choices) prev_dept else if (length(dept_choices)) dept_choices[1] else character(0)
    
    updatePickerInput(
      session, "dropdown2",
      choices  = dept_choices,
      selected = dept_selected
    )
    
    df_units <- if (length(dept_selected)) df1 %>% filter(PRINCIPAL_SURGEON_VERITY_DIV_DESC %in% dept_selected) else df1
    unit_choices <- df_units %>% pull(SERVICE_GROUP) %>% unique() %>% sort()
    prev_units <- isolate(input$dropdown3)
    keep_units <- intersect(prev_units %||% character(0), unit_choices)
    unit_selected <- if (length(keep_units)) keep_units else if (length(unit_choices)) unit_choices[1] else character(0)
    
    updateSelectInput(
      session, "dropdown3",
      choices  = unit_choices,
      selected = unit_selected
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$dropdown2, {
    req(input$dropdown1)
    
    df2 <- surgeon_table %>%
      filter(FACILITY_MSX %in% input$dropdown1,
             PRINCIPAL_SURGEON_VERITY_DIV_DESC %in% (input$dropdown2 %||% character(0)))
    
    unit_choices <- df2 %>% pull(SERVICE_GROUP) %>% unique() %>% sort()
    prev_units <- isolate(input$dropdown3)
    keep_units <- intersect(prev_units %||% character(0), unit_choices)
    unit_selected <- if (length(keep_units)) keep_units else if (length(unit_choices)) unit_choices[1] else character(0)
    
    updateSelectInput(
      session, "dropdown3",
      choices  = unit_choices,
      selected = unit_selected
    )
  }, ignoreInit = TRUE)
  
  
  
  # SERVICE_OUTPUT
  output$service_output <- DT::renderDataTable({
    ####
    output_table <- service_table %>% 
      filter(FACILITY_MSX %in% input$dropdown1, PRINCIPAL_SURGEON_VERITY_DIV_DESC %in% input$dropdown2,SERVICE_GROUP %in% input$dropdown3) %>%
      arrange(desc(AVG_PATIENTS_DAY))
    
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
      PRINCIPAL_SURGEON_VERITY_DIV_DESC = "Service Line",
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
      formatStyle(c("Hospital", "Unit Type", "Service Line"),
                  backgroundColor = "#f2f3f2", fontWeight = "bold") |>
      formatStyle(
        "AVG Patients/Day",
        `text-align` = "right"   
      )
  }) 
  
  
  
  # SURGEON_OUTPUT
  output$surgeon_output <- DT::renderDataTable({
    output_table <- surgeon_table %>% 
      filter(FACILITY_MSX %in% input$dropdown1, PRINCIPAL_SURGEON_VERITY_DIV_DESC %in% input$dropdown2,SERVICE_GROUP %in% input$dropdown3)
    
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
      PRINCIPAL_SURGEON_VERITY_DIV_DESC = "Service Line",
      PRINCIPAL_SURGEON_NAME_MSX = "Primary Surgeon",
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
      formatStyle(c("Hospital", "Unit Type", "Service Line", "Primary Surgeon"),
                  backgroundColor = "#f2f3f2", fontWeight = "bold") |>
      formatStyle(
        "AVG Patients/Day",
        `text-align` = "right"   
      )
  }) 
  
}

shinyApp(ui = ui, server = server)
