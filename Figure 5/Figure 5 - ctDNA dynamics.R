# ============================================================
# FMT-LUMINate
# Association between ctDNA dynamics and clinical response
# ============================================================

# 1. PACKAGES ------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)


# 2. DATA IMPORT ---------------------------------------------

df <- read_excel("ctDNA.xlsx")


# 3. RESPONSE ANALYSIS AND PLOTTING FUNCTION ------------------

plot_bar_fisher <- function(data, xvar, xlab, title) {
  
  # Patient selection
  df_plot <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      .data[[xvar]] != "",
      !is.na(Response),
      Response %in% c("R", "NR")
    ) %>%
    distinct(`Patient ID`, .data[[xvar]], Response)
  
  # Fisher's exact test
  fisher_tab <- table(df_plot[[xvar]], df_plot$Response)
  fisher_p <- fisher.test(fisher_tab)$p.value
  
  # Response counts and proportions
  plot_data <- df_plot %>%
    count(.data[[xvar]], Response) %>%
    group_by(.data[[xvar]]) %>%
    mutate(
      total = sum(n),
      percent = n / total,
      label = paste0(n, " (", sprintf("%.1f", percent * 100), "%)")
    ) %>%
    ungroup()
  
  # Stacked bar plot
  ggplot(plot_data, aes(x = .data[[xvar]], y = percent, fill = Response)) +
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
    scale_fill_manual(values = c("NR" = "red", "R" = "blue")) +
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
      y = "Response rate (%)",
      title = title,
      fill = NULL
    ) +
    theme_classic(base_size = 16)
}


# 4. BASELINE ctDNA STATUS AND RESPONSE -----------------------

p1 <- plot_bar_fisher(
  df,
  xvar = "ctDNA status baseline",
  xlab = "ctDNA status at baseline",
  title = "Baseline ctDNA status and response"
)

p1


# 5. ctDNA CLEARANCE AND RESPONSE -----------------------------

p2 <- plot_bar_fisher(
  df,
  xvar = "ctDNA clearance",
  xlab = "ctDNA clearance",
  title = "ctDNA clearance and response"
)

p2


# 6. FIGURE EXPORT -------------------------------------------

ggsave("ctDNA_baseline_response.png", p1, width = 5, height = 6, dpi = 300)
ggsave("ctDNA_clearance_response.png", p2, width = 5, height = 6, dpi = 300)


# ============================================================
# END
# ============================================================