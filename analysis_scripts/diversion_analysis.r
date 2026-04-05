### SETUP ###
library(tidyverse)
library(lubridate)
library(scales)
library(here)

# Read in Diversion dataset
data <- read_csv(here::here("data", "diversion.csv"))

# Outpath
outpath <- here::here("figures")
if (!dir.exists(outpath)) dir.create(outpath)

# Parameters for analysis
charges_of_interest <- c(
  "UNLAWFUL USE OF A WEAPON",
  "CARRY/POSSESS FIREARM LOCATION",
  "UNLAWFUL USE OR POSSESSION OF A WEAPON BY A FELON",
  "AGGRAVATED DISCHARGE OF A FIREARM",
  "RECKLESS DISCHARGE OF A FIREARM",
  "AGG UUW/LOADED/NO FCCA/FOID",
  "AGG UUW/UNLOADED/NO FCCA",
  "AGG UUW/VEHICLE/LOADED FIREARM",
  "AGGRAVATED UNLAWFUL USE OF WEAPON",
  "ARMED HABITUAL CRIMINAL",
  "UNLWFL USE FIREARM PROJECTILE",
  "POSS HANDGUN/MENTAL PAT 5 YEAR",
  "POSSESSION OF STOLEN FIREARM",
  "DEFACING IDENTIFICATION MARKS OF FIREARMS")

# Clean dataset
data <- data |>
  filter(!is.na(DIVERSION_RESULT))

### FIGURES ###
# Figure 11: Diversion Outcomes for Black Men with Gun Possession Charges vs All Other Cases
comparison_summary <- data %>%
  mutate(is_target = RACE == "Black" & GENDER == "Male" & PRIMARY_CHARGE_OFFENSE_TITLE %in% charges_of_interest) %>%
  mutate(group = if_else(is_target, 
                         "Black men with gun possession charges", 
                         "All other cases")) %>%
  count(group, DIVERSION_RESULT) %>%
  group_by(group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    DIVERSION_RESULT = factor(DIVERSION_RESULT, levels = c("Graduated", "Failed")),
    group = factor(group, levels = c("Black men with gun possession charges", "All other cases"))
  )

fig11 <- ggplot(comparison_summary, 
                 aes(x = group, y = percent, fill = DIVERSION_RESULT)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = paste0(round(percent,1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  scale_fill_manual(values = c("Graduated"="#368942", "Failed"="#E27D60")) +
  scale_y_continuous(labels = scales::percent_format(scale=1), limits=c(0,100)) +
  labs(title = "Diversion Outcomes for Black Men with Gun Possession Charges vs All Other Cases",
       x = NULL,
       y = "Percent of Cases",
       fill = "Outcome") +
  theme_minimal(base_size = 14) +
  theme(legend.position="top")

ggsave(filename = file.path(outpath, "Figure_11.png"),
       plot = fig11,
       width = 10, height = 6, dpi = 600, bg = "white")