# 1. FIRST install and load required packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(ggplot2)
library(dplyr)

# 2. Prepare the data
plot_data <- data.frame(
  Predictor = c(
    "Disorders of lipid metabolism in parents",
    "Disorders of lipid metabolism in grandparents",
    "Other nervous system conditions in parents",
    "Hypertension in parents",
    "Menopausal disorders in parents",
    "Joint disorders in individuals",
    "Dementia and other cognitive disorders in grandparents",
    "Esophageal disorders in parents",
    "Thyroid disorders in grandparents",
    "Cataract in grandparents",
    "Gout and other crystal arthropathies in grandparents",
    "Esophageal disorders in grandparents",
    "Personality disorders in individuals",
    "Substance-use disorders in individuals",
    "Mood and anxiety disorders in individuals",
    "Attention deficit and related disorders in individuals",
    "Adjustment disorders in individuals",
    "Psychotic disorder in parents",
    "Developmental disorders in individuals",
    "Alcohol and substance-use disorders in parents"
  ),
  Odds_ratio = c(
    0.75, 0.78, 0.78, 0.82, 0.83, 1.85, 1.88, 1.92, 1.95, 1.95,
    1.96, 1.96, 2.49, 2.31, 1.83, 1.50, 1.41, 1.40, 1.25, 1.06
  )
)

# 3. Process and order the data
plot_data <- plot_data %>%
  arrange(Odds_ratio) %>%
  mutate(Predictor = factor(Predictor, levels = Predictor))

# 4. Create the plot with automatic scaling
odds_plot <- ggplot(plot_data, aes(x = Odds_ratio, y = Predictor)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = Odds_ratio < 1), size = 3) +
  geom_segment(aes(xend = 1, yend = Predictor, color = Odds_ratio < 1),
               linewidth = 0.8, alpha = 0.7) +
  scale_x_log10(
    breaks = c(0.7, 0.8, 0.9, 1, 1.5, 2, 2.5),
    limits = c(0.7, 2.6)
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8"), guide = "none") +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Association of Predictors with Outcome"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    plot.margin = margin(1, 1, 1, 3, "cm")
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Odds_ratio),
        x = ifelse(Odds_ratio < 1, Odds_ratio - 0.03, Odds_ratio + 0.03)),
    size = 3.5, vjust = 0.5
  )

# 5. Display and save the plot
print(odds_plot)

# Save with explicit ggplot2 call to ensure function is found
ggplot2::ggsave("odds_ratio_plot_final.png", 
                plot = odds_plot, 
                width = 10, 
                height = 8, 
                dpi = 300)
