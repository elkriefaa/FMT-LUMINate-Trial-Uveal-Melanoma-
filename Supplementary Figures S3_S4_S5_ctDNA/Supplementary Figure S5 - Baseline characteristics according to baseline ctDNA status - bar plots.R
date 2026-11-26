# ============================================================
# FMT-LUMINate
# Clinical characteristics according to baseline ctDNA status
# ============================================================

# 1. PACKAGES ------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)


# 2. DATA IMPORT ---------------------------------------------

df <- read_excel("ctDNA.xlsx")


# 3. DATA PREPARATION ----------------------------------------

df <- df %>%
  mutate(
    tumor_type = recode(
      tumor_type,
      "Non-small cell lung cancer" = "NSCLC"
    ),
    tumor_type = factor(
      tumor_type,
      levels = c("NSCLC", "Cutaneous melanoma", "Uveal melanoma")
    ),
    metastasis_brain = factor(
      metastasis_brain,
      levels = c("Present", "Absent")
    ),
    metastasis_liver = factor(
      metastasis_liver,
      levels = c("Present", "Absent")
    ),
    stage_group = case_when(
      stage %in% c("III", "3", 3) ~ "III",
      stage %in% c("IV", "4", 4) ~ "IV",
      TRUE ~ NA_character_
    ),
    stage_group = factor(
      stage_group,
      levels = c("III", "IV")
    )
  )


# 4. STATISTICAL ANALYSIS AND PLOTTING FUNCTION ---------------

plot_stacked_by_ctDNA <- function(data, category_var, xlab, title) {
  
  # Selection of evaluable patients
  df_plot <- data %>%
    filter(
      !is.na(`ctDNA status baseline`),
      `ctDNA status baseline` %in% c("Positive", "Negative"),
      !is.na(.data[[category_var]]),
      .data[[category_var]] != ""
    ) %>%
    distinct(`Patient ID`, `ctDNA status baseline`, .data[[category_var]])
  
  # Fisher's exact test
  fisher_tab <- table(
    df_plot$`ctDNA status baseline`,
    df_plot[[category_var]]
  )
  
  fisher_p <- fisher.test(fisher_tab)$p.value
  
  # Counts and proportions
  plot_data <- df_plot %>%
    count(`ctDNA status baseline`, .data[[category_var]]) %>%
    group_by(`ctDNA status baseline`) %>%
    mutate(
      total = sum(n),
      percent = n / total,
      label = paste0(n, " (", sprintf("%.1f", percent * 100), "%)")
    ) %>%
    ungroup()
  
  # Stacked bar plot
  p <- ggplot(
    plot_data,
    aes(
      x = `ctDNA status baseline`,
      y = percent,
      fill = .data[[category_var]]
    )
  ) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5,
      fontface = "bold"
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1.12),
      expand = c(0, 0)
    ) +
    annotate(
      "segment",
      x = 1, xend = 2,
      y = 1.04, yend = 1.04
    ) +
    annotate(
      "text",
      x = 1.5,
      y = 1.08,
      label = paste0("p = ", signif(fisher_p, 3)),
      size = 6
    ) +
    labs(
      x = xlab,
      y = "Patients (%)",
      title = title,
      fill = NULL
    ) +
    theme_classic(base_size = 16)
  
  # Stage-specific color scale
  if (category_var == "stage_group") {
    p <- p +
      scale_fill_manual(
        values = c(
          "III" = "#4DAF4A",
          "IV" = "#E41A1C"
        )
      )
  }
  
  return(p)
}


# 5. CLINICAL CHARACTERISTICS PLOTS --------------------------

p_stage <- plot_stacked_by_ctDNA(
  df,
  category_var = "stage_group",
  xlab = "Baseline ctDNA status",
  title = "Stage according to baseline ctDNA status"
)

p_tumor <- plot_stacked_by_ctDNA(
  df,
  category_var = "tumor_type",
  xlab = "Baseline ctDNA status",
  title = "Tumor type according to baseline ctDNA status"
)

p_brain <- plot_stacked_by_ctDNA(
  df,
  category_var = "metastasis_brain",
  xlab = "Baseline ctDNA status",
  title = "Brain metastasis according to baseline ctDNA status"
)

p_liver <- plot_stacked_by_ctDNA(
  df,
  category_var = "metastasis_liver",
  xlab = "Baseline ctDNA status",
  title = "Liver metastasis according to baseline ctDNA status"
)


# 6. FIGURE DISPLAY ------------------------------------------

p_stage
p_tumor
p_brain
p_liver


# 7. FIGURE EXPORT -------------------------------------------

ggsave("baseline_ctDNA_stage.png", p_stage, width = 5, height = 6, dpi = 300)
ggsave("baseline_ctDNA_tumor_type.png", p_tumor, width = 6.5, height = 6, dpi = 300)
ggsave("baseline_ctDNA_brain_mets.png", p_brain, width = 5, height = 6, dpi = 300)
ggsave("baseline_ctDNA_liver_mets.png", p_liver, width = 5, height = 6, dpi = 300)


# ============================================================
# END
# ============================================================