### SETUP ###
library(tidyverse)
library(lubridate)
library(scales)
library(slider)
library(here)

# Read in Arrests dataset
data <- read_csv(here::here("data", "arrests.csv"))

# Outpath
outpath <- here::here("figures")
if (!dir.exists(outpath)) dir.create(outpath)

# Parameters for analysis
charges_of_interest <- c(
  "AGG UPW/VEHICLE",
  "AGG UPW/VEH/LOADED/FCCA/FOID",
  "AGG UPW/VEH/LOADED/NO FCCA",
  "AGG UPW/VEHICLE/LOADED FIREARM",
  "AGG UPW/VEH/LOADED/NO FOID",
  "AGG UPW/VEHICLE/<21",
  "AGG UPW/VEH/AMMO/NO FCCA",
  "AGG UPW/VEHICLE/NO FOID",
  "AGG UPW/VEHICLE/<21/2+",
  "AGG UPW/VEH/LOADED FIREARM/2+",
  "AGG UPW/VEH/LOADED/NO FCCA/2+",
  "AGG UPW/VEHICLE/NO FOID/2+",
  "AGG UPW/VEH/PREV CONVICTION",
  "AGG UPW/VEHICLE/CM VIOLATION",
  "AGG UPW/VEH/AMMO",
  "AGG UPW/VEHICLE/2+",
  "AGG UUW/VEHICLE/<21",
  "AGG UUW/VEHICLE/NO FOID",
  "AGG UUW/VEHICLE/LOADED/NO FCCA",
  "AGG UUW/UNLOADED/NO FCCA/FOID",
  "AGG UUW/LOADED-NO FCCA-FOID",
  "AGG UUW/VEH/FIR LOADED/NO FOID",
  "AGG UUW/VEH/PREV CONVICTION",
  "AGG UUW/VEH/UNLOADED/AMMO",
  "AGG UUW/VEHICLE/LOADED FIREARM 2+",
  "UUW - WEAPON - AGG./VEH. OR CONCEALED",
  "AGG UUW/VEHICLE/NO FOID/2+",
  "AGG UUW/VEH/UNLOAD/NO FCCA/2+",
  "AGG UNLWFL USE WEAPON/VEH/2ND",
  "AGG UUW/VEHICLE/CM VIOLATION",
  "AGG UUW/VEHICLE/LOADED/NO FCCA 2+",
  "AGG UUW/VEH/CM VIOLATION/2+",
  "AGG UUW/VEHICLE/ORDER PROTECTI",
  "AGG UUW/VEHICLE/<21/2+",
  "AGG UUW/VEH/DELINQ MINOR/2+",
  "AGG UUW/VEH/DELINQUENT MINOR",
  "AGG UUW/VEH/UNLOADED/AMMO/2+",
  "AGG UUW/VEHICLE/LOADED FIREARM",
  "AGG UUW-LOADED-NO FCCA-FOID",
  "POSSESSION OF WEAPON - NARCOTICS ACT",
  "AGG UPW/PERSON/LOADED/NO FCCA",
  "AGG UPW/PERSON/AMMO/NO FCCA",
  "AGG UPW/PERS/LOADED/FCCA/FOID",
  "AGG UPW/PERSON",
  "AGG UPW/PERSON/<21",
  "AGG UPW/PERSON/LOADED/NO FOID",
  "AGG UPW/PERSON/NO FOID",
  "AGG UPW/PERS/PREV CONVICTION",
  "AGG UPW/PERS/LOADED/NO FCCA/2+",
  "AGG UPW/PERSON/<21/2+",
  "AGG UPW/PERSON/LOADED FIREARM",
  "AGG UPW/PERSON/NO FOID/2+",
  "AGG UPW/PERSON/2+",
  "AGG UPW/PERS/LOADED FIREARM/2+",
  "AGG UPW/PERSON/AMMO",
  "DESC AGG UPW/PERSON/CM VIOLATI",
  "AGG UUW/ON PERSON",
  "AGG UUW/PERS/LOAD/NO FCCA/FOID",
  "AGG UUW/PERSON/<21",
  "AGG UUW/PERS/FIR LOADED/FOID",
  "AGG UUW/PERSON/LOADED/NO FCCA",
  "AGG UUW/PERSON/NO FOID",
  "AGG UUW/PERSON/LOADED FIREARM",
  "AGG UUW/PERS/UNLOADED/NO FCCA",
  "AGG UUW/PERS/LOADED FIR/2+",
  "UUW - AGG UUW/PERSON/VEHICLE/PREVIOUS CONVICTION",
  "AGG UUW/PERSON/NO FOID/2+",
  "AGG UUW/PERS/LOADED/NO FCCA/2+",
  "AGG UUW/ON PERSON/2ND",
  "AGG UUW/PERSON/UNLOADED/AMMO",
  "AGG UUW/PERSON/<21/2+",
  "AGG UUW/UNLOADED/NO FCCA/2+",
  "AGG UUW/PERS/UNLOADED/AMMO/2+",
  "AGG UUW/PERSON/DELINQ MINOR",
  "AGG UUW/PERSON/CM VIOLATION",
  "UUW - WEAPON - AGG./PUBLIC ST./ALLEY/LAND",
  "AGG UUW/ LOADED FIREARM - NOT PISTOL, REVOLVER, HANGUN",
  "AGG UUW/ LOADED GUN",
  "AGG UUW/ LOADED PISTOL, REVOLVER, HANDGUN - NO CCL",
  "AGG UUW / UNLOADED, AMMO ACCESS / NOT PISTOL, REVOL, HANDGUN",
  "AGG UUW/UNLOADED, AMMO ACCESS/PISTOL, REVOL, HANDGUN-NO CCL",
  "UUW WHILE ENGAGED IN VIOLATING THE MISD. CANNABIS CONTROL AC",
  "UUW - WEAPON - PUBLIC STREET/ALLEY/LANDS",
  "UUW - WEAPON - CARRY/POSSESS FIREARM/1ST",
  "UUW - WEAPON - CARRY/POSSESS FIREARM/2ND+",
  "UUW - WEAPON - CARRY/POSSESS FIREARM/SCHOOL/PARK",
  "UUW - CARRY/POSSESS FIREARM/1ST",
  "UUW - WEAPON - POSSESS/CARRY /CONCEAL WEAPON",
  "UUW - WEAPON - CARRY /POSSESS CONCEALED WEAPON/2ND")

highlight_color <- "#368942"

# Clean dataset
gun_arrests <- data %>%
  filter(if_any(c(`CHARGE 1 DESCRIPTION`, `CHARGE 2 DESCRIPTION`, `CHARGE 3 DESCRIPTION`),
                ~ . %in% charges_of_interest)) %>%
  mutate(arrest_date_clean = mdy_hms(`ARREST DATE`)) %>%
  filter(arrest_date_clean >= as.Date("2021-01-01"))

### FIGURES ###
# Figure 1: Gun Possession Arrests by Police District in Chicago
district_share <- gun_arrests %>%
  filter(!is.na(DISTRICT)) %>%
  count(DISTRICT) %>%
  mutate(pct = n / sum(n), highlight = DISTRICT == "7") %>%
  arrange(desc(pct))

fig1 <- ggplot(district_share, aes(x = reorder(DISTRICT, pct), y = pct, fill = highlight)) +
  geom_col(width = 0.7) +
  coord_flip() +
  geom_text(aes(label = if_else(DISTRICT == "7", percent(pct, accuracy = 1), "")),
            hjust = -0.1, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c("TRUE" = highlight_color, "FALSE" = "gray75"), guide = "none") +
  labs(title = "Share of Gun Possession Arrests by Police District",
       subtitle = "Chicago, Jan 2021 – Mar 2026",
       x = "Police District", y = "Percent of Gun Possession Arrests") +
  theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "white"))

ggsave(file.path(outpath, "Figure_1.png"), fig1, width = 8, height = 5, bg = "white")

# Figure 2: Gun Possession Arrests by Race in Chicago
race_dist <- gun_arrests %>%
  mutate(RACE_SIMPLE = ifelse(RACE == "BLACK", "Black", "Non-Black")) %>%
  count(RACE_SIMPLE) %>%
  mutate(pct = n / sum(n), highlight = RACE_SIMPLE == "Black")

fig2 <- ggplot(race_dist, aes(x = reorder(RACE_SIMPLE, pct), y = pct, fill = highlight)) +
  geom_col() +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            hjust = -0.1, color = "black", fontface = "bold", size = 4) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c("TRUE" = highlight_color, "FALSE" = "gray75"), guide = "none") +
  labs(title = "Share of Gun Possession Arrests by Race",
       subtitle = "Chicago, Jan 2021 – Mar 2026",
       x = "", y = "Percent of Gun Possession Arrests") +
  theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "white"))

ggsave(file.path(outpath, "Figure_2.png"), fig2, width = 6, height = 4, bg = "white")

# FIGURE 3: Gun Possession Arrests of Young Black Men in Englewood
dist7_comp <- gun_arrests %>%
  filter(DISTRICT == "7") %>%
  mutate(group = ifelse(RACE == "BLACK" & SEX == "MALE" & between(AGE, 18, 40), 
                        "Black men 18–40", "Other")) %>%
  count(group) %>%
  mutate(pct = n / sum(n), highlight = group == "Black men 18–40") %>%
  arrange(desc(pct))

fig3 <- ggplot(dist7_comp, aes(x = reorder(group, pct), y = pct, fill = highlight)) +
  geom_col() +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            hjust = -0.1, color = "black", fontface = "bold", size = 4) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c("TRUE" = highlight_color, "FALSE" = "gray75"), guide = "none") +
  labs(title = "Share of Gun Possession Arrests in District 7,\nYoung Black Men vs. Others",
       subtitle = "District 7, Jan 2021 – Mar 2026",
       x = "", y = "Percent of Gun Possession Arrests") +
  theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "white"))

ggsave(file.path(outpath, "Figure_3.png"), fig3, width = 6, height = 4, bg = "white")

# Figure 4: Monthly Gun Possession Arrests of Young Black Men in Englewood
monthly_trend <- gun_arrests %>%
  filter(DISTRICT == "7", RACE == "BLACK", SEX == "MALE", between(AGE, 18, 40)) %>%
  mutate(month = floor_date(mdy_hms(`ARREST DATE`), "month")) %>%
  count(month) %>%
  arrange(month) %>%
  mutate(rolling_avg = slide_dbl(n, mean, .before = 2, .complete = FALSE))

fig4 <- ggplot(monthly_trend, aes(x = month, y = rolling_avg)) +
  geom_line(color = highlight_color, linewidth = 1.4) +
  geom_point(color = highlight_color) +
  labs(title = "Monthly Gun Possession Arrests of Young Black Men in District 7",
       subtitle = "District 7, Jan 2021 – Mar 2026",
       x = "", y = "Number of Arrests") +
  theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "white"))

ggsave(file.path(outpath, "Figure_4.png"), fig4, width = 8, height = 5, bg = "white")

# Figure 5: Gun Possession Arrests of Young Black Men in Englewood vs. All Other Gun Possession Cases
pie_data <- gun_arrests %>%
  filter(!is.na(DISTRICT), !is.na(RACE), !is.na(SEX), !is.na(AGE)) %>%
  mutate(group = ifelse(DISTRICT == "7" & RACE == "BLACK" & SEX == "MALE" & between(AGE, 18, 40), 
                        "Target Population", "All Others")) %>%
  count(group) %>%
  mutate(pct = n / sum(n), pct_label = paste0(group, " – ", round(pct*100), "%"))

fig5 <- ggplot(pie_data, aes(x = "", y = pct, fill = group)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = pct_label), position = position_stack(vjust = 0.5), 
            color = "black", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Target Population" = highlight_color, "All Others" = "gray75"), 
                    guide = "none") +
  labs(title = "Share of Gun Possession Arrests:\nTarget Population vs All Others",
       subtitle = "Chicago, Jan 2021 – Mar 2026") +
  theme_void(base_size = 13) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0, lineheight = 1.2),
        plot.subtitle = element_text(hjust = 0),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

ggsave(file.path(outpath, "Figure_5.png"), fig5, width = 6, height = 6, bg = "white")

# Figure 6: Distribution of Gun Possession Charge Statutes Among Young Black Men in Englewood
statute_counts <- gun_arrests %>%
  filter(DISTRICT == "7", RACE == "BLACK", SEX == "MALE", between(AGE, 18, 40)) %>%
  select(`CHARGE 1 DESCRIPTION`) %>% 
  pivot_longer(everything(), values_to = "charge") %>%
  filter(!is.na(charge), charge %in% charges_of_interest) %>%
  count(charge, sort = TRUE) %>%
  mutate(pct = n / sum(n)) %>%
  filter(n >= 50)

fig6 <- ggplot(statute_counts, aes(x = reorder(charge, pct), y = pct)) +
  geom_col(fill = highlight_color) +
  geom_text(aes(label = percent(pct, accuracy = 1)), 
            hjust = -0.1, color = "black", fontface = "bold", size = 4) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Most Common Gun Possession Charges Among\nBlack Men Ages 18–40 in District 7",
       subtitle = "Chicago, Jan 2021 – Mar 2026",
       x = "", y = "Percent of Gun Posession Arrest Charges") +
  theme_minimal(base_size = 13) +
  theme(panel.background = element_rect(fill = "white"))

ggsave(file.path(outpath, "Figure_6.png"), fig6, width = 8, height = 5, bg = "white")