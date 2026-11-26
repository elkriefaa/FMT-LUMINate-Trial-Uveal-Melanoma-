library(readxl)
library(survival)
library(forestmodel)
library(survminer)
library(dplyr)
library(ggplot2)

#All Uveal melanoma patients

efficacy <- read_excel("PFS_OS_KM.xlsx", 
                       sheet = "PFS_OS_KM")

efficacy <- efficacy[efficacy$diagnosis == "Melanoma - uveal", ]

str(efficacy)
nrow(efficacy)


###########
###########
### PFS ###
###########
###########

### PFS - Median only ###
survPFS <- survfit(Surv(pfs, progression) ~ 1, data = efficacy)

median_pfs <- summary(survPFS)$table["median"]
median_pfs_ci_lower <- summary(survPFS)$table["0.95LCL"]
median_pfs_ci_upper <- summary(survPFS)$table["0.95UCL"]
cat("Median PFS:", median_pfs, "months\n")
cat("95% CI for Median PFS:", median_pfs_ci_lower, "-", median_pfs_ci_upper, "months\n")

pfs_km <- ggsurvplot(
  survPFS, data = efficacy,
  xlab = "Time since start of therapy (months)",
  xlim = c(0, 12),
  break.time.by = 3,
  ylab = "Progression-Free Survival",
  conf.int = FALSE,
  risk.table = TRUE,
  risk.table.y.text = TRUE,
  risk.table.height = 0.20,  
  tables.theme = theme_cleantable(),  
  ggtheme = theme_classic(),          
  palette = "#ec7063",
  font.main    = 18,
  font.x       = 16,
  font.y       = 16,
  font.tickslab= 12,
  legend.labs = c(""),
  legend.title = "FMT-LUMINate Cohort",
)

pfs_km$table <- pfs_km$table +
  theme(
    plot.margin  = margin(t = 0, r = 0, b = 40, l = 25),
    plot.title   = element_text(margin = margin(b = 0)),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x  = element_blank()
  )

print(pfs_km)



##########
##########
### OS ###
##########
##########

### OS - Median only ###
survOS <- survfit(Surv(os, death) ~ 1, data = efficacy)

median_os <- summary(survOS)$table["median"]
median_os_ci_lower <- summary(survOS)$table["0.95LCL"]
median_os_ci_upper <- summary(survOS)$table["0.95UCL"]
cat("Median OS:", median_os, "months\n")
cat("95% CI for Median OS:", median_os_ci_lower, "-", median_os_ci_upper, "months\n")

os_km <- ggsurvplot(
  survOS, data = efficacy,
  xlab = "Time since start of therapy (months)",
  xlim = c(0, 12),
  break.time.by = 3,
  ylab = "Overall Survival",
  conf.int = FALSE,
  risk.table = TRUE,
  risk.table.y.text = TRUE,
  risk.table.height = 0.20,  
  tables.theme = theme_cleantable(), 
  palette = "#ec7063",
  font.main    = 18,
  font.x       = 16,
  font.y       = 16,
  font.tickslab= 13,
  risk.table.fontsize = 4.8,
  legend.labs = c(""),
  legend.title = "Real World Cohort",
)

os_km$table <- os_km$table +
  theme(
    plot.margin  = margin(t = 0, r = 5, b = 40, l = 30),
    plot.title   = element_text(margin = margin(b = 0), size = 14),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x  = element_blank(),
  )

print(os_km)


# Median follow-up time --- Reverse KM to estimate follow-up time for OS
########
########
########
followup_OS <- survfit(Surv(os, 1 - death) ~ 1, data = efficacy)

# Median follow-up
median_followup_os <- summary(followup_OS)$table["median"]
cat("Median follow-up for OS:", median_followup_os, "months\n")


