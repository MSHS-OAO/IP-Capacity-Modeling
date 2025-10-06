library(DBI)
library(odbc)
library(glue)
library(lubridate)
library(dplyr) 
library(ggplot2)


conn <- dbConnect(odbc(), "OAO Cloud DB Production")


query <- glue("SELECT PRIM_ENC_CSN_ID, SERVICE_DATE, TX_POST_DATE FROM OE_CHARGE_DETAIL
WHERE BILLING_CAT_DESC = 'BED CHARGES' AND TO_DATE(SERVICE_DATE, 'YYYYMMDD') >= DATE '2024-06-01'
              ")

OUTPUT <- dbGetQuery(conn, query)
dbDisconnect(conn)

OUTPUT <- OUTPUT %>%
  mutate(
    SERVICE_DATE   = ymd(SERVICE_DATE),
    TX_POST_DATE   = ymd(TX_POST_DATE),
    DIFFERENCE     = as.numeric(difftime(TX_POST_DATE, SERVICE_DATE, units = "days")),
    SERVICE_MONTH  = format(SERVICE_DATE, "%m"),
    SERVICE_YEAR   = format(SERVICE_DATE, "%Y"),
    SERVICE_WEEK   = week(SERVICE_DATE)
  )

WEEKLY_OUTPUT <- OUTPUT %>%
  group_by(SERVICE_WEEK, SERVICE_YEAR) %>%
  summarize(
    TOTAL_DIFFERENCE = sum(DIFFERENCE, na.rm = TRUE),
    N_OBS = n(),
    AVG_DIFFERENCE = TOTAL_DIFFERENCE / N_OBS,
    .groups = "drop"
  ) %>%
  mutate(
    SERVICE_YEAR_WEEK = paste0(SERVICE_YEAR, "-", SERVICE_WEEK)
  ) %>%
  arrange(SERVICE_YEAR, SERVICE_WEEK) %>%
  mutate(
    SERVICE_YEAR_WEEK = factor(SERVICE_YEAR_WEEK, levels = unique(SERVICE_YEAR_WEEK))
  )


MONTHLY_OUTPUT <- OUTPUT %>%
  group_by(SERVICE_MONTH, SERVICE_YEAR)%>%
  summarize(
    TOTAL_DIFFERENCE = sum(DIFFERENCE, na.rm = TRUE),
    N_OBS = n(),
    AVG_DIFFERENCE = TOTAL_DIFFERENCE / N_OBS
  ) %>%
  mutate(SERVICE_YEAR_MONTH = paste0(SERVICE_YEAR,"-", SERVICE_MONTH))





ggplot(WEEKLY_OUTPUT, aes(x = SERVICE_YEAR_WEEK, y = AVG_DIFFERENCE)) +
  geom_col(fill = "steelblue",  width = 0.9) +
  labs(
    x = "Service Year-Week",
    y = "Average Difference (days)",
    title = "Average Difference by Service Week"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )


ggplot(WEEKLY_OUTPUT, aes(x = SERVICE_YEAR_WEEK, y = AVG_DIFFERENCE)) +
  geom_col(fill = "steelblue", width = 0.9) +
  labs(
    x = "Service Year-Week",
    y = "Average Difference (days)",
    title = "Average Difference by Service Week"
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = WEEKLY_OUTPUT$SERVICE_YEAR_WEEK[seq(1, nrow(WEEKLY_OUTPUT), by = 3)] # show every 3rd label
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8) # smaller labels
  )



ggplot(MONTHLY_OUTPUT, aes(x = SERVICE_YEAR_MONTH, y = AVG_DIFFERENCE)) +
  geom_col(fill = "steelblue",  width = 0.9) +
  labs(
    x = "Service Year-MONTH",
    y = "Average Difference (days)",
    title = "Average Difference by Service MONTH"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#adding column for difference in months (binning)
PERCENT_OUTPUT <- OUTPUT %>%
  mutate(
    MONTHS_DIFFERENCE = (DIFFERENCE %/% 30) + 1
  )


#calculating cumalatve percentage based on MONTHS_DIFFERENCE column
CUMULATIVE_PERCENT_OUTPUT <- PERCENT_OUTPUT %>%
  count(MONTHS_DIFFERENCE) %>%
  arrange(MONTHS_DIFFERENCE) %>%
  mutate(
    CUM_COUNT = cumsum(n),
    TOTAL = sum(n),
    CUM_PERCENT = (CUM_COUNT / TOTAL) * 100
  )




ggplot(CUMULATIVE_PERCENT_OUTPUT, aes(x = MONTHS_DIFFERENCE, y = CUM_PERCENT)) +
  geom_area(fill = "steelblue", alpha = 0.5) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_text(
    aes(label = paste0(floor(CUM_PERCENT * 100) / 100, "%")),
    vjust = -0.7, size = 2.5
  ) +
  scale_x_continuous(breaks = unique(CUMULATIVE_PERCENT_OUTPUT$MONTHS_DIFFERENCE)) +
  scale_y_continuous(
    limits = c(0, 105), breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = "Monthly Difference",
    y = "Cumulative Percentage",
    title = "Cumulative Distribution of Months Difference"
  ) +
  theme_minimal()



ggplot(CUMULATIVE_PERCENT_OUTPUT, aes(x = factor(MONTHS_DIFFERENCE), y = CUM_PERCENT)) +
  geom_col(fill = "#00AEFF", alpha = 0.7, color = "black", width = 0.6) +
  geom_text(
    aes(label = paste0(floor(CUM_PERCENT * 100) / 100, "%")),
    vjust = -0.5, size = 2.3
  ) +
  labs(
    x = "Monthly Difference",
    y = "Cumulative Percentage",
    title = "Cumulative Percentage by Months Difference"
  ) +
  theme_minimal()



CUMULATIVE_PERCENT_TABLE <- CUMULATIVE_PERCENT_OUTPUT %>%
  arrange(MONTHS_DIFFERENCE) %>%
  mutate(
    PERCENTAGE = n / sum(n) * 100,
    CUMULATIVE_PERCENTAGE = cumsum(PERCENTAGE)
  ) %>%
  select(MONTHS_DIFFERENCE, PERCENTAGE, CUMULATIVE_PERCENTAGE)





ggplot(CUMULATIVE_PERCENT_TABLE, aes(x = factor(MONTHS_DIFFERENCE))) +
  geom_col(aes(y = PERCENTAGE), fill = "gray70", alpha = 0.7, color = "black", width = 0.6) +
  geom_line(aes(y = CUMULATIVE_PERCENTAGE, group = 1), color = "steelblue", linewidth = 1) +
  geom_point(aes(y = CUMULATIVE_PERCENTAGE), color = "steelblue", size = 2) +
  geom_text(
    aes(y = CUMULATIVE_PERCENTAGE, label = paste0(floor(CUMULATIVE_PERCENTAGE * 100) / 100, "%")),
    vjust = -0.6, size = 2.5
  ) +
  scale_y_continuous(limits = c(0, 105), labels = function(x) paste0(x, "%")) +
  labs(
    x = "Monthly Difference",
    y = "Percentage",
    title = "Monthly Percentage (bars) with Cumulative Percentage (line)"
  ) +
  theme_minimal()








BOXPLOT_OUTPUT <- OUTPUT %>%
  mutate(
    SERVICE_DATE = ymd(SERVICE_DATE),
    TX_POST_DATE = ymd(TX_POST_DATE),
    DIFFERENCE = as.numeric(difftime(TX_POST_DATE, SERVICE_DATE, units = "days")),
    SERVICE_MONTH = format(SERVICE_DATE, "%m"),
    SERVICE_YEAR = format(SERVICE_DATE, "%Y"),
    SERVICE_WEEK = week(SERVICE_DATE),
    SERVICE_MONTH_YEAR = paste0(SERVICE_YEAR, "-", SERVICE_MONTH)
  )




ggplot(BOXPLOT_OUTPUT, aes(x = SERVICE_MONTH_YEAR, y = DIFFERENCE)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.color = "red") +
  labs(
    title = "Distribution of Days Difference by Service Month-Year",
    x = "Service Month-Year",
    y = "Difference (days)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )
  
