#figure1

read_model_data <- function(filename, model_label) {
  read_excel(paste0(data_path, filename)) %>%
    rename(
      Estimate = odds_ratio,
      CI_lower = conf.low,
      CI_upper = conf.high
    ) %>%
    mutate(
      Model = model_label,
      Significance = ifelse(CI_lower > 1 | CI_upper < 1, "Significant", "Not Significant"),
      Direction = ifelse(Estimate > 1, "Positive", "Negative")
    )
}

read_meaninglessness <- function(filename, model_label) {
  read_excel(paste0(data_path, filename)) %>%
    mutate(
      Model = model_label,
      Significance = ifelse(CI_lower > 0 | CI_upper < 0, "Significant", "Not Significant"),
      Direction = ifelse(Estimate > 0, "Positive", "Negative")
    )
}
dep1 <- read_model_data("odds_ratios_model1_from_text.xlsx", "Model 1")
dep2 <- read_model_data("odds_ratios_model2_from_text.xlsx", "Model 2")
dep3 <- read_model_data("odds_ratios_model3_from_text.xlsx", "Model 3")
dep_all <- bind_rows(dep1, dep2, dep3)

all_vars <- unique(dep_all$Variable)
all_models <- unique(dep_all$Model)
var_model_grid <- expand.grid(Variable = all_vars, Model = all_models)

dep_all_full <- var_model_grid %>%
  left_join(dep_all, by = c("Variable", "Model")) %>%
  mutate(
    Variable = factor(Variable, levels = rev(all_vars))
  )
mean1 <- read_meaninglessness("meaninglessness_model1.xlsx", "Model 1")
mean2 <- read_meaninglessness("meaninglessness_model2.xlsx", "Model 2")
mean3 <- read_meaninglessness("meaninglessness_model3.xlsx", "Model 3")
mean_all <- bind_rows(mean1, mean2, mean3)
mean_all$Variable <- factor(mean_all$Variable, levels = rev(unique(mean_all$Variable)))
direction_colors <- c("Positive" = "#d7191c", "Negative" = "#2c7bb6")  # Lancet红蓝
shape_values <- c("Significant" = 16, "Not Significant" = 1)  # 实心 vs 空心

plot_depression <- ggplot(dep_all_full, aes(x = Estimate, y = Variable,
                                            color = Direction, shape = Significance)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                 height = 0.2, color = "gray50", na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_manual(values = direction_colors, na.translate = FALSE) +
  scale_shape_manual(values = shape_values, na.translate = FALSE) +
  scale_x_continuous(trans = "log10", breaks = c(0.5, 1, 2)) +
  facet_grid(. ~ Model) +
  labs(
    title = "A. Depression: Fixed Effects Across Models",
    x = "Odds Ratio (95% CI)",
    y = NULL,
    color = "Direction",
    shape = "Significance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0)
  )

plot_meaninglessness <- ggplot(mean_all, aes(x = Estimate, y = Variable,
                                             color = Direction, shape = Significance)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "gray50") +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = direction_colors) +
  scale_shape_manual(values = shape_values) +
  facet_grid(. ~ Model) +
  labs(
    title = " B. Meaninglessness: Fixed Effects Across Models",
    x = "Estimate (95% CI)",
    y = NULL,
    color = "Direction",
    shape = "Significance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0)
  )

#figure1 based on model_interaction_fladder RESULTS
predicted_fladder <- ggpredict(model_interaction_fladder, terms = c("NSP_weighted", "fladder"))
ggplot(predicted_fladder, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    title = "Interaction Effect: NSP and Fladder on Depression Risk",
    x = "Community SES (NSP_weighted)",
    y = "Predicted Probability of Depression",
    color = "Fladder (Subjective SES)"
  ) +
  theme_minimal()