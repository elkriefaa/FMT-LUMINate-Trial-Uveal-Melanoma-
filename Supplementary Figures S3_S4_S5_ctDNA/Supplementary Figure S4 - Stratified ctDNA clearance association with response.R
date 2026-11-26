# ============================================================
# FMT-LUMINate
# Association between ctDNA clearance and clinical response
# stratified by tumor type
# ============================================================

# 1. PACKAGES ------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)


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
      levels = c(
        "NSCLC",
        "Cutaneous melanoma",
        "Uveal melanoma"
      )
    ),
    `ctDNA clearance` = factor(
      trimws(as.character(`ctDNA clearance`)),
      levels = c("No", "Yes")
    ),
    Response = factor(
      trimws(as.character(Response)),
      levels = c("NR", "R")
    )
  )


# 4. SELECTION OF EVALUABLE PATIENTS --------------------------

df_plot <- df %>%
  filter(
    !is.na(tumor_type),
    !is.na(`ctDNA clearance`),
    !is.na(Response)
  ) %>%
  distinct(
    `Patient ID`,
    tumor_type,
    `ctDNA clearance`,
    Response
  )

# Verify patient-level uniqueness
stopifnot(!anyDuplicated(df_plot$`Patient ID`))


# 5. RESPONSE PLOTTING FUNCTION ------------------------------

plot_response_by_clearance <- function(data, tumor) {
  
  df_tumor <- data %>%
    filter(tumor_type == tumor)
  
  # Response counts and proportions
  plot_data <- df_tumor %>%
    count(
      `ctDNA clearance`,
      Response,
      .drop = FALSE
    ) %>%
    complete(
      `ctDNA clearance`,
      Response,
      fill = list(n = 0)
    ) %>%
    group_by(`ctDNA clearance`) %>%
    mutate(
      total = sum(n),
      percent = ifelse(
        total > 0,
        n / total,
        0
      ),
      label = ifelse(
        n > 0,
        paste0(
          n, " (",
          sprintf("%.1f", percent * 100),
          "%)"
        ),
        ""
      )
    ) %>%
    ungroup()
  
  # Stacked bar plot
  p <- ggplot(
    plot_data,
    aes(
      x = `ctDNA clearance`,
      y = percent,
      fill = Response
    )
  ) +
    geom_col(
      width = 0.8
    ) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5,
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = c(
        "NR" = "red",
        "R" = "blue"
      ),
      breaks = c("NR", "R"),
      limits = c("NR", "R"),
      drop = FALSE
    ) +
    scale_x_discrete(
      limits = c("No", "Yes"),
      drop = FALSE
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      expand = c(0, 0)
    ) +
    labs(
      x = "ctDNA clearance",
      y = "Response rate (%)",
      fill = NULL
    ) +
    theme_classic(base_size = 16) +
    theme(
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "right"
    )
  
  return(p)
}


# 6. TUMOR-SPECIFIC RESPONSE PLOTS ----------------------------

p_nsclc <- plot_response_by_clearance(
  df_plot,
  "NSCLC"
)

p_cutaneous <- plot_response_by_clearance(
  df_plot,
  "Cutaneous melanoma"
)

p_uveal <- plot_response_by_clearance(
  df_plot,
  "Uveal melanoma"
)


# 7. FIGURE DISPLAY ------------------------------------------

p_nsclc
p_cutaneous
p_uveal


# 8. FIGURE EXPORT -------------------------------------------

ggsave(
  "ctDNA_clearance_response_NSCLC.png",
  p_nsclc,
  width = 5,
  height = 5,
  dpi = 300
)

ggsave(
  "ctDNA_clearance_response_cutaneous.png",
  p_cutaneous,
  width = 5,
  height = 5,
  dpi = 300
)

ggsave(
  "ctDNA_clearance_response_uveal.png",
  p_uveal,
  width = 5,
  height = 5,
  dpi = 300
)


# 9. PATIENT COUNT VERIFICATION ------------------------------

df_plot %>%
  count(
    tumor_type,
    `ctDNA clearance`,
    Response,
    .drop = FALSE
  )

cat(
  "\nTotal evaluable patients:",
  nrow(df_plot),
  "\n"
)


# ============================================================
# END
# ============================================================