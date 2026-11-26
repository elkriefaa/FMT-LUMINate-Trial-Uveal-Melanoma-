# ============================================================
# FMT-LUMINate
# Baseline ctDNA positivity according to tumor type
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
    )
  )

# Include patients with available baseline ctDNA results
df_baseline <- df %>%
  filter(
    `ctDNA status baseline` %in% c("Positive", "Negative"),
    !is.na(tumor_type)
  ) %>%
  distinct(
    `Patient ID`,
    tumor_type,
    `ctDNA status baseline`
  ) %>%
  mutate(
    `ctDNA status baseline` = factor(
      `ctDNA status baseline`,
      levels = c("Negative", "Positive")
    )
  )


# 4. STATISTICAL ANALYSIS ------------------------------------

fisher_tab_baseline <- table(
  df_baseline$tumor_type,
  df_baseline$`ctDNA status baseline`
)

fisher_p_baseline <- fisher.test(fisher_tab_baseline)$p.value


# 5. COUNTS AND PROPORTIONS ----------------------------------

plot_data3 <- df_baseline %>%
  count(
    tumor_type,
    `ctDNA status baseline`,
    .drop = FALSE
  ) %>%
  group_by(tumor_type) %>%
  mutate(
    total = sum(n),
    percent = n / total,
    label = ifelse(
      n == 0,
      "",
      paste0(n, " (", sprintf("%.1f", percent * 100), "%)")
    )
  ) %>%
  ungroup()


# 6. BASELINE ctDNA POSITIVITY PLOT ---------------------------

p_baseline_positivity <- ggplot(
  plot_data3,
  aes(
    x = tumor_type,
    y = percent,
    fill = `ctDNA status baseline`
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
  scale_fill_manual(
    values = c(
      "Negative" = "#F8766D",
      "Positive" = "#00BA38"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.12),
    expand = c(0, 0)
  ) +
  annotate(
    "segment",
    x = 1, xend = 3,
    y = 1.04, yend = 1.04
  ) +
  annotate(
    "text",
    x = 2,
    y = 1.08,
    label = paste0(
      "p = ", signif(fisher_p_baseline, 3)
    ),
    size = 6
  ) +
  labs(
    x = "Tumor type",
    y = "Patients (%)",
    title = "Baseline ctDNA positivity according to tumor type",
    fill = "Baseline ctDNA"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 12)
  )


# 7. FIGURE DISPLAY AND EXPORT -------------------------------

p_baseline_positivity

ggsave(
  "baseline_ctDNA_positivity_by_tumor_type.png",
  p_baseline_positivity,
  width = 8,
  height = 6,
  dpi = 300
)


# 8. CONTINGENCY TABLE ---------------------------------------

fisher_tab_baseline


# ============================================================
# END
# ============================================================