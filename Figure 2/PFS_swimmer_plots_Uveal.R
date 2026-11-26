library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)

efficacy <- read_excel("PFS_swimmer.xlsx", sheet = "swimmer") %>%
  filter(diagnosis == "Melanoma - uveal") %>%
  mutate(across(starts_with("date"), as.Date)) %>%
  mutate(
    time_best_response = as.numeric(difftime(date_best_response, date_fmt, units = "days")) / 30.4375,
    time_pfs = as.numeric(difftime(date_pfs, date_fmt, units = "days")) / 30.4375,
    time_os = as.numeric(difftime(date_os, date_fmt, units = "days")) / 30.4375,
    time_last_follow_up = as.numeric(difftime(date_last_follow_up, date_fmt, units = "days")) / 30.4375,
    time_antibiotic_1 = as.numeric(difftime(date_antibiotic_1, date_fmt, units = "days")) / 30.4375,
    
    time_on_treatment = ifelse(
      !is.na(date_off_treatment),
      as.numeric(difftime(date_off_treatment, date_fmt, units = "days")) / 30.4375,
      time_last_follow_up
    ),
    time_off_treatment = ifelse(
      !is.na(date_off_treatment),
      as.numeric(difftime(date_last_follow_up, date_off_treatment, units = "days")) / 30.4375,
      NA
    ),
    progression = ifelse(progression == 1, time_pfs, NA),
    death = ifelse(death == 1, time_os, NA),
    end_treatment_at_last_follow_up = ifelse(
      !is.na(date_off_treatment) &
        date_off_treatment == date_last_follow_up &
        (is.na(death) | death == 0),
      TRUE, FALSE
    )
  )

patient_levels <- efficacy %>%
  arrange(time_os) %>%
  pull(id_patient) %>%
  unique()

efficacy <- efficacy %>%
  mutate(y_pos = as.numeric(factor(id_patient, levels = patient_levels)))

pr_y_offset <- -0.02

events_levels <- c(
  "CR", "PR", "Progression", "Death",
  "On treatment", "Off treatment"
)

ggplot(efficacy) +
  geom_segment(
    aes(x = 0, xend = time_on_treatment,
        y = y_pos, yend = y_pos,
        color = "On treatment"),
    linewidth = 5
  ) +
  geom_segment(
    data = efficacy %>% filter(!is.na(time_off_treatment)),
    aes(x = time_on_treatment, xend = time_last_follow_up,
        y = y_pos, yend = y_pos,
        color = "Off treatment"),
    linewidth = 5
  ) +
  geom_point(
    data = efficacy %>% filter(best_response %in% c("CR", "PR")),
    aes(x = time_best_response,
        y = y_pos + pr_y_offset,
        color = best_response),
    size = 3.6,
    shape = 17
  ) +
  geom_point(
    aes(x = progression,
        y = y_pos,
        color = "Progression"),
    size = 4.5,
    shape = 16,
    na.rm = TRUE
  ) +
  geom_point(
    aes(x = death,
        y = y_pos,
        color = "Death"),
    size = 4.3,
    shape = 15,
    na.rm = TRUE
  ) +
  geom_point(
    aes(x = time_antibiotic_1,
        y = y_pos,
        fill = "Antibiotics"),
    size = 2.8,
    shape = 23,
    color = "black",
    stroke = 0.6,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c(
      "CR" = "#65b03f",
      "PR" = "#657fdb",
      "Progression" = "#f06f59",
      "Death" = "black",
      "On treatment" = "#A9C7EE",
      "Off treatment" = "#b2dfdb"
    ),
    breaks = events_levels
  ) +
  scale_fill_manual(
    values = c("Antibiotics" = "#E78AC3")
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    fill = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  scale_y_continuous(
    breaks = seq_along(patient_levels),
    labels = patient_levels
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 24, 30, 36),
    limits = c(0, 36),
    expand = c(0, 0.2)
  ) +
  labs(
    x = "Months after FMT",
    y = "Patients",
    color = NULL,
    fill = NULL,
    caption = "* Patients ended treatment at time of last follow-up \nPatients typically receive immunotherapy for up to 2 years. Some variability due to scheduling"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.box = "horizontal",
    legend.spacing.x = unit(0.5, "cm"),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0, face = "italic")
  )


efficacy %>%
  filter(id_patient == "101-004") %>%
  select(id_patient, time_on_treatment, time_last_follow_up, time_off_treatment)