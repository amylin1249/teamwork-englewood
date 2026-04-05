### SETUP ###
library(tidyverse)
library(lubridate)
library(scales)
library(here)

# Read in Initiation dataset
data <- read_csv(here::here("data", "initiation.csv"))

# Outpath
outpath <- here::here("figures")
if (!dir.exists(outpath)) dir.create(outpath)

# Parameters for analysis
sections_of_interest <- c(
  "24-1.1(a)",
  "24-1.1(b)",
  "24-1.2-5(a)(2)",
  "24-1.2(a)(1)",
  "24-1.2(a)(2)",
  "24-1.2(a)(3)",
  "24-1.2(a)(4)",
  "24-1.2(a)(5)",
  "24-1.2(a)(6)",
  "24-1.5(a)",
  "24-1.5(b)",
  "24-1.6(a)(1)",
  "24-1.6(A)(1)1",
  "24-1.6(a)(2)",
  "24-1.7(a)",
  "24-1.8(a)(1)",
  "24-1.8(a)(2)",
  "24-1(a)(1)",
  "24-1(a)(10)",
  "24-1(a)(14)",
  "24-1(a)(4)",
  "24-1(a)(6)",
  "24-1(a)(7)(i)",
  "24-1(a)(7)(ii)",
  "24-1(a)(7)(iii)",
  "24-1(a)(8)",
  "24-1(a)(9)",
  "24-2.1(a)",
  "24-3.1(a)(1)",
  "24-3.1(a)(2)",
  "24-3.1(a)(4)",
  "24-3.2(c)",
  "24-3.5(b)",
  "24-3.5(c)",
  "24-3.7(a)",
  "24-3.8(a)",
  "24-3.9(a)(1)",
  "24-3(A)(a)",
  "24-3(A)(d)",
  "24-3(A)(g)",
  "24-3(A)(k)",
  "24-3(A)(l)",
  "24-3A(a)",
  "24-5.1(b)",
  "24-5.1(c)",
  "24-5(a)",
  "24-5(b)",
  "24.5-10",
  "24.5-5"
)

target_data <- data |>
  filter(RACE == "Black", 
         GENDER == "Male", 
         between(AGE_AT_INCIDENT, 18, 40),
         INCIDENT_CITY == "Chicago",
         CHAPTER == "720",
         ACT == "5",
         SECTION %in% sections_of_interest)

policy_date <- as.Date("2023-09-18")

# Clean dataset
bond_dates <- target_data |>
  filter(!is.na(BOND_DATE_CURRENT)) |>
  mutate(BOND_DATE_CURRENT = as.Date(parse_date_time(BOND_DATE_CURRENT, orders = "Y b d I:M:S p")))

window_days <- min(as.numeric(policy_date - min(bond_dates$BOND_DATE_CURRENT, na.rm = TRUE)),
                   as.numeric(max(bond_dates$BOND_DATE_CURRENT, na.rm = TRUE) - policy_date))

bond_data <- bond_dates |>
  filter(!is.na(BOND_TYPE_CURRENT),
         BOND_DATE_CURRENT >= (policy_date - window_days) & 
           BOND_DATE_CURRENT <= (policy_date + window_days)) |>
  mutate(period = factor(if_else(BOND_DATE_CURRENT < policy_date, "Pre", "Post"), 
                         levels = c("Pre", "Post")))

### FIGURES ###
# Figure 8: Bond Type Distribution Pre vs Post Pretrial Fairness Act
bond_summary <- bond_data |>
  count(period, BOND_TYPE_CURRENT) |>
  group_by(period) |>
  mutate(percent = n / sum(n) * 100) |>
  ungroup() |>
  mutate(BOND_TYPE_CURRENT = factor(BOND_TYPE_CURRENT, levels = c("No Bond", "C Bond", "I Bond", "D Bond"))) |>
  arrange(period, BOND_TYPE_CURRENT) |>
  group_by(period) |>
  mutate(ymax = cumsum(percent), ymin = lag(ymax, default = 0))

bond_colors <- c("No Bond" = "#368942", "C Bond" = "#66C2A5", "I Bond" = "#FC8D62", "D Bond" = "#8DA0CB")

fig8 <- ggplot(bond_summary, aes(x = period, ymin = ymin, ymax = ymax, fill = BOND_TYPE_CURRENT)) +
  geom_rect(aes(xmin = as.numeric(period)-0.4, xmax = as.numeric(period)+0.4)) +
  scale_fill_manual(values = bond_colors) +
  scale_y_continuous(labels = scales::percent_format(scale=1)) +
  labs(title = "Bond Type Distribution Pre vs Post Pretrial Fairness Act",
       subtitle = "Black Men Ages 18-40 in Chicago with Gun Possession Charges",
       x = "Period", y = "Percent of Gun Possession Cases", fill = "Bond Type") +
  theme_minimal(base_size = 14) + theme(legend.position = "top", panel.grid.major.x = element_blank())

ggsave(file.path(outpath, "Figure_8.png"), fig8, width = 10, height = 6, bg = "white")

# Figure 9: Pretrial Outcomes Pre vs Post Pretrial Fairness Act
bond_summary_collapsed <- bond_data |>
  mutate(outcome = factor(if_else(BOND_TYPE_CURRENT == "No Bond", "Detained", "Released"), 
                          levels = c("Detained", "Released"))) |>
  count(period, outcome) |>
  group_by(period) |>
  mutate(percent = n / sum(n) * 100) |>
  ungroup() |>
  arrange(period, outcome) |>
  group_by(period) |>
  mutate(ymax = cumsum(percent), ymin = lag(ymax, default = 0))

fig9 <- ggplot(bond_summary_collapsed, aes(x = period, ymin = ymin, ymax = ymax, fill = outcome)) +
  geom_rect(aes(xmin = as.numeric(period)-0.4, xmax = as.numeric(period)+0.4)) +
  scale_fill_manual(values = c("Detained" = "#368942", "Released" = "#A6A6A6")) +
  scale_y_continuous(labels = scales::percent_format(scale=1)) +
  labs(title = "Pretrial Outcomes Pre vs Post Pretrial Fairness Act",
       subtitle = "Black Men Ages 18-40 in Chicago with Gun Possession Charges",
       x = "Period", y = "Percent of Gun Possession Cases", fill = "Outcome") +
  theme_minimal(base_size = 14) + theme(legend.position = "top", panel.grid.major.x = element_blank())

ggsave(file.path(outpath, "Figure_9.png"), fig9, width = 10, height = 6, bg = "white")

# Figure 10: Monthly Felony Review Outcomes (Deviation from Mean)
monthly_long <- target_data |>
  filter(!is.na(FELONY_REVIEW_RESULT), !is.na(ARREST_DATE)) |>
  mutate(ARREST_DATE = as.Date(parse_date_time(ARREST_DATE, orders = "Y b d I:M:S p")),
         month = floor_date(ARREST_DATE, "month")) |>
  group_by(month) |>
  summarise(Approved = mean(FELONY_REVIEW_RESULT == "Approved") * 100,
            `Not Approved` = mean(FELONY_REVIEW_RESULT %in% c("Rejected", "Continued Investigation")) * 100,
            .groups = "drop") |>
  mutate(across(c(Approved, `Not Approved`), ~ . - mean(., na.rm = TRUE))) |>
  pivot_longer(-month, names_to = "type", values_to = "deviation")

fig10 <- ggplot(monthly_long, aes(x = month, y = deviation, color = type)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Approved" = "#368942", "Not Approved" = "#E27D60")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Monthly Felony Review Outcomes (Deviation from Mean)",
       subtitle = "Black Men Ages 18-40 in Chicago with Gun Possession Charges",
       x = NULL, y = "Percentage points above/below mean", color = "Outcome") +
  theme_minimal(base_size = 14) + theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(outpath, "Figure_10.png"), fig10, width = 14, height = 8, bg = "white")