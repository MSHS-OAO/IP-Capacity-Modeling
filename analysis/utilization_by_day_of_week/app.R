#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# -------------------------------------------------------- Functions & Constants --------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"


# List all CSV files in the directory
bed_cap_csv <- list.files(paste0(cap_dir, "Tableau Data/Bed Capacity/"),
                          pattern = "\\.csv$", full.names = TRUE)




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



# read each CSV and list average bed capacity for each unit monthly
bed_cap <- read_csv(paste0(cap_dir, "Tableau Data/Detail_data.csv"),
                    show_col_types = FALSE) %>%
  rename(HOSPITAL = Location,
         SERVICE_GROUP = `Service Group`,
         EXTERNAL_NAME = Unit) %>%
  mutate(
    HOSPITAL = case_when(
      HOSPITAL == "MOUNT SINAI BETH ISRAEL" ~ "MSBI",
      HOSPITAL == "MOUNT SINAI BROOKLYN" ~ "MSB",
      HOSPITAL == "MOUNT SINAI MORNINGSIDE" ~ "MSM",
      HOSPITAL == "MOUNT SINAI QUEENS" ~ "MSQ",
      HOSPITAL == "MOUNT SINAI WEST" ~ "MSW",
      HOSPITAL == "THE MOUNT SINAI HOSPITAL" ~ "MSH"),
    SERVICE_GROUP = case_when(
      EXTERNAL_NAME == "MSH KCC 2 South" ~ "Rehab",
      TRUE ~ SERVICE_GROUP),
    SERVICE_DATE = mdy(`Day of Census Day`)) %>%
  group_by(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_DATE) %>%
  summarise(DATASET = "BASELINE",
            BED_CAPACITY = sum(`Count of Custom SQL Query`, na.rm = TRUE)) %>%
  filter(SERVICE_DATE >= min(base$SERVICE_DATE),
         SERVICE_DATE <= max(base$SERVICE_DATE))



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
                            "SERVICE_DATE" = "SERVICE_DATE",
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

df_overall <- df_overall %>% mutate(AVG_OVERALL_UTILIZATION = UTILIZATION_SUM/COUNT_OF_SERVICE_DAYS)

df_overall <- unique(df_overall)

df_overall <- df_overall %>% select(-UTILIZATION_SUM,-COUNT_OF_SERVICE_DAYS)

df_overall$EXTERNAL_NAME <- sub("^\\S+\\s+", "", df_overall$EXTERNAL_NAME)

df_overall <- df_overall %>% filter(!(is.na(AVG_OVERALL_UTILIZATION)))


df_overall <- df_overall %>%
  mutate(
    AVG_OVERALL_UTILIZATION = round(AVG_OVERALL_UTILIZATION * 100, 2)
  )

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

df_weekdays <- df_weekdays %>%
  mutate(
    AVG_WEEKDAY_UTILIZATION = round(AVG_WEEKDAY_UTILIZATION * 100, 2)
  )

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

df_day_of_week <- df_day_of_week %>%
  mutate(
    AVG_UTILIZATION_DAY_OF_WEEK = round(AVG_UTILIZATION_DAY_OF_WEEK * 100, 2)
  )



final_df <- df_day_of_week %>%
  pivot_wider(
    names_from = DAY_OF_WEEK,
    values_from = AVG_UTILIZATION_DAY_OF_WEEK
  ) %>%
  mutate(
    AVG_WEEKEND_UTILIZATION = round((Saturday + Sunday) / 2, 2)
  )


final_df <- final_df %>%
  left_join(
    df_weekdays %>% 
      select(FACILITY_MSX, SERVICE_GROUP, EXTERNAL_NAME, AVG_WEEKDAY_UTILIZATION),
    by = c("FACILITY_MSX", "SERVICE_GROUP", "EXTERNAL_NAME")
  )


final_df <- final_df %>%
  left_join(
    df_overall %>% 
      select(FACILITY_MSX, SERVICE_GROUP, EXTERNAL_NAME, AVG_OVERALL_UTILIZATION),
    by = c("FACILITY_MSX", "SERVICE_GROUP", "EXTERNAL_NAME")
  )


final_df <- final_df %>%
  mutate(WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE = AVG_WEEKEND_UTILIZATION - AVG_WEEKDAY_UTILIZATION)


final_df <- final_df %>%
  select(
    FACILITY_MSX, 
    SERVICE_GROUP,
    EXTERNAL_NAME,
    AVG_WEEKEND_UTILIZATION,
    AVG_WEEKDAY_UTILIZATION,
    WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE,
    AVG_OVERALL_UTILIZATION,
    everything(),
    -DATASET
  )

# removing KP2 L&D 
final_df <- final_df %>% filter(EXTERNAL_NAME != "KP2 L&D")

#creating polished excel sheet and storing
final_df2 <- final_df %>%
  mutate(
    WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE = round(WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE / 100, 4)
  ) %>%
  mutate(across(
    c(
      AVG_WEEKEND_UTILIZATION,
      AVG_WEEKDAY_UTILIZATION,
      AVG_OVERALL_UTILIZATION,
      Saturday, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday
    ),
    ~ round(.x / 100, 4)   # numeric decimals, no "%"
  ))

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Sheet1")

writeData(wb, "Sheet1", final_df2)

# Columns that should be displayed as percentages
pct_cols <- c(
  "AVG_WEEKEND_UTILIZATION",
  "AVG_WEEKDAY_UTILIZATION",
  "AVG_OVERALL_UTILIZATION",
  "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
  "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE"
)

pct_style <- createStyle(numFmt = "0.00%")  # always 2 decimals, shows like 12.87%

for (col in pct_cols) {
  col_index <- which(names(final_df2) == col)
  addStyle(
    wb, sheet = 1,
    style = pct_style,
    cols = col_index,
    rows = 2:(nrow(final_df2) + 1),  # row 1 is header
    gridExpand = TRUE
  )
}

saveWorkbook(wb, "final_df2.xlsx", overwrite = TRUE)

assign("final_df2", final_df2, envir = .GlobalEnv)


hospital_choices <- sort(unique(df_day_of_week$FACILITY_MSX))
msh_df <- baseline %>% filter(FACILITY_MSX == "MSH")
service_choices  <- sort(unique(msh_df$SERVICE_GROUP))
unit_choices <- sort(unique(df_day_of_week$EXTERNAL_NAME))

assign("df_day_of_week", df_day_of_week, envir = .GlobalEnv)
#assign("baseline", baseline, envir = .GlobalEnv)
assign("final_df", final_df, envir = .GlobalEnv)
assign("df_overall", df_overall, envir = .GlobalEnv)
assign("df_weekdays", df_weekdays, envir = .GlobalEnv)
assign("unit_choices", unit_choices, envir = .GlobalEnv)
assign("hospital_choices", hospital_choices, envir = .GlobalEnv)
assign("service_choices", service_choices, envir = .GlobalEnv)
assign("num_days", num_days, envir = .GlobalEnv)



# ************************************************* UI **************************************************************


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Inpatient Unit Type Analysis"),
  fluidRow(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 8px;",
    
    column(
      width = 6,
      pickerInput(
        inputId = "dropdown1",
        label   = "Select Hospital:",
        choices = hospital_choices,
        selected = "MSH",
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
      width = 6,
      pickerInput(
        inputId = "dropdown2",
        label   = "Select Service Line:",
        choices = NULL,
        width   = "100%",
        options = pickerOptions(
          liveSearch = TRUE,
          liveSearchStyle = "startsWith",
          actionsBox = TRUE,
          liveSearchNormalize = TRUE,
          noneSelectedText = "Search or select a service line..."
        )
      )
    )
  ),
  
  br(),
  textOutput("selected_options"),
  br(),
  uiOutput("utilization_plots")  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  library(dplyr)
  library(ggplot2)
  
  observeEvent(input$dropdown1, {
    service_choices <- df_day_of_week %>%
      filter(FACILITY_MSX %in% input$dropdown1) %>%
      distinct(SERVICE_GROUP) %>%
      arrange(SERVICE_GROUP) %>%
      pull(SERVICE_GROUP)
    service_choices <- unique(service_choices)
    updatePickerInput(
      session,
      inputId = "dropdown2",
      choices = service_choices,
      selected = NULL
    )
  })
  
  filtered_df <- reactive({
    req(input$dropdown1, input$dropdown2)
    df_day_of_week %>%
      filter(
        FACILITY_MSX  == input$dropdown1,
        SERVICE_GROUP == input$dropdown2
      )
  })
  
  output$selected_options <- renderText({
    paste0(
      "Hospital: ", input$dropdown1,
      " | Service Line: ", input$dropdown2
    )
  })
  
  output$utilization_plots <- renderUI({
    df <- filtered_df()
    validate(need(nrow(df) > 0, "No data for this hospital / service combination."))
    unit_types <- sort(unique(df$EXTERNAL_NAME))
    
    plot_output_list <- lapply(seq_along(unit_types), function(i) {
      plotname_dow     <- paste0("plot_dow_", i)
      plotname_summary <- paste0("plot_summary_", i)
      
      fluidRow(
        column(
          width = 12,
          h4(unit_types[i],
             style = "text-align: center; font-weight: bold; font-size: 22px;")
        ),
        column(
          width = 6,
          plotOutput(plotname_summary, height = "450px")
        ),
        column(
          width = 6,
          plotOutput(plotname_dow, height = "450px")
        )
      )
    })
    
    for (i in seq_along(unit_types)) {
      local({
        my_i      <- i
        this_unit <- unit_types[my_i]
        plotname_dow     <- paste0("plot_dow_", my_i)
        plotname_summary <- paste0("plot_summary_", my_i)
        
        output[[plotname_dow]] <- renderPlot({
          df_plot <- df %>% filter(EXTERNAL_NAME == this_unit)
          
          overall_row <- df_overall %>%
            filter(
              FACILITY_MSX  == input$dropdown1,
              SERVICE_GROUP == input$dropdown2,
              EXTERNAL_NAME == this_unit
            )
          
          weekday_row <- df_weekdays %>%
            filter(
              FACILITY_MSX  == input$dropdown1,
              SERVICE_GROUP == input$dropdown2,
              EXTERNAL_NAME == this_unit
            )
          
          overall_val <- if (nrow(overall_row) > 0) overall_row$AVG_OVERALL_UTILIZATION[1] else NA_real_
          weekday_val <- if (nrow(weekday_row) > 0) weekday_row$AVG_WEEKDAY_UTILIZATION[1] else NA_real_
          
          max_val <- suppressWarnings(max(c(overall_val, weekday_val), na.rm = TRUE))
          scale_factor <- if (!is.infinite(max_val) && max_val <= 1) 100 else 1
          
          overall_pct <- if (!is.na(overall_val)) round(overall_val * scale_factor, 2) else NA_real_
          weekday_pct <- if (!is.na(weekday_val)) round(weekday_val * scale_factor, 2) else NA_real_
          
          subtitle_parts <- c()
          if (!is.na(overall_pct)) subtitle_parts <- c(subtitle_parts, paste0("Overall: ", overall_pct, "%"))
          if (!is.na(weekday_pct)) subtitle_parts <- c(subtitle_parts, paste0("Weekdays: ", weekday_pct, "%"))
          subtitle_text <- if (length(subtitle_parts) > 0) paste(subtitle_parts, collapse = " | ") else NULL
          
          df_plot$DAY_OF_WEEK <- factor(
            df_plot$DAY_OF_WEEK,
            levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                       "Friday", "Saturday", "Sunday")
          )
          
          ggplot(
            df_plot,
            aes(x = DAY_OF_WEEK, y = AVG_UTILIZATION_DAY_OF_WEEK)
          ) +
            geom_col(fill = "#221f72") +
            geom_text(
              aes(label = paste0(AVG_UTILIZATION_DAY_OF_WEEK, "%")),
              vjust = -0.3,
              size = 5
            ) +
            labs(
              title    = "Average Utilization by Day of Week",
              subtitle = subtitle_text,
              x        = "Day of Week",
              y        = "Average Utilization (%)"
            ) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
            theme_minimal() +
            theme(
              plot.title    = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.text.x   = element_text(angle = 45, hjust = 1)
            )
        })
        
        output[[plotname_summary]] <- renderPlot({
          summary_row <- final_df %>%
            dplyr::filter(
              FACILITY_MSX  == input$dropdown1,
              SERVICE_GROUP == input$dropdown2,
              EXTERNAL_NAME == this_unit
            )
          
          validate(need(nrow(summary_row) > 0, "No summary data available."))
          
          weekend_val <- summary_row$AVG_WEEKEND_UTILIZATION[1]
          weekday_val <- summary_row$AVG_WEEKDAY_UTILIZATION[1]
          overall_val <- summary_row$AVG_OVERALL_UTILIZATION[1]
          
          max_val <- suppressWarnings(max(c(weekend_val, weekday_val, overall_val), na.rm = TRUE))
          scale_factor <- if (!is.infinite(max_val) && max_val <= 1) 100 else 1
          
          df_summary <- tibble::tibble(
            Metric = c("Weekend", "Weekday", "Overall"),
            Value  = c(weekend_val, weekday_val, overall_val) * scale_factor
          )
          
          ggplot(df_summary, aes(x = Metric, y = Value)) +
            geom_col(fill = "#221f72") +
            geom_text(
              aes(label = paste0(round(Value, 1), "%")),
              vjust = -0.3,
              size = 5
            ) +
            labs(
              title = "Average Utilization Summary",
              x     = "",
              y     = "Average Utilization (%)"
            ) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
            theme_minimal() +
            theme(
              plot.title  = element_text(hjust = 0.5),
              axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5)
            )
        })
      })
    }
    
    do.call(tagList, plot_output_list)
  })
  
  
  }


# Run the application 
shinyApp(ui = ui, server = server)
