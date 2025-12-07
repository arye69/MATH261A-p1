plot(PGA2022$driveSG, PGA2022$avgScore)
plot(PGA2022$puttsSG, PGA2022$avgScore)


library(dplyr)
summary <- PGA2022 |>
  group_by(tournament) |>
  summarize(Player_Count = n_distinct(playerName))

print(sum(summary[,2]))

plot(PGA2022$avgDriveDist, log(PGA2022$Money))

n_distinct(PGA2022$playerName)







pga_cleaned2 <- PGA2022 |>
  group_by(playerName) |>
  summarise(
    avg_puttsSG = mean(puttsSG, na.rm = TRUE),   # Mean Strokes Gained Putting
    avg_driveSG = mean(driveSG, na.rm = TRUE),    # Mean Strokes Gained Off the Tee
    avg_money = mean(Money, na.rm = TRUE)      # Mean Scoring Average
  ) |>
  ungroup()

plot(pga_cleaned$avg_driveSG, pga_cleaned$avg_Score)
reg <- lm(avg_Score ~ avg_driveSG, data = pga_cleaned)
summary(reg)
plot(reg2)

reg2 <- lm(avg_Score ~ avg_puttsSG, data = pga_cleaned)
summary(reg2)

abline(a = 70.26907, b = -0.69627)




library(ggplot2)
ggplot(pga_cleaned, aes(x = avg_driveSG, y = avg_Score)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_abline(
    intercept = 70.26907,  # Line crosses y-axis at y=100
    slope = -0.69627,        # For each unit increase in x, y increases by 5
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  )


plot(pga_cleaned2$avg_driveSG, pga_cleaned2$avg_money)




pga_agg <- PGA2022 |>
  group_by(playerName) |>
  summarise(
    avg_Score = mean(avgScore, na.rm = TRUE),
    avg_puttsSG = mean(puttsSG, na.rm = TRUE),
    avg_driveSG = mean(driveSG, na.rm = TRUE),
    Tournaments_Played = n()
  )

pga_agg <- pga_agg[-c(77, 81),]

hist(pga_agg$Tournaments_Played)

library(kableExtra)
library(dplyr)
library(tidyverse)
library(knitr)
library(GGally)
library(here)


scatter_plot <- ggpairs(pga_agg,
                        columns = c("avg_puttsSG", "avg_driveSG", "avg_Score"),
                        upper = list(continuous = "cor"),
                        title = "Figure 1: Scatterplot Matrix of Aggregated Variables"
) +
  theme_bw()
print(scatter_plot)



summary_table <- pga_agg |>
  select(avg_Score, avg_puttsSG, avg_driveSG) |>
  summarise(across(everything(), list(Mean = mean, SD = sd, Min = min, Max = max)))


summary_stats <- pga_agg |>
  summarise(
    Variable = c("avg_Score", "avg_puttsSG", "avg_driveSG"),
    Mean = c(mean(avg_Score), mean(avg_puttsSG), mean(avg_driveSG)),
    SD = c(sd(avg_Score), sd(avg_puttsSG), sd(avg_driveSG)),
    Min = c(min(avg_Score), min(avg_puttsSG), min(avg_driveSG)),
    Max = c(max(avg_Score), max(avg_puttsSG), max(avg_driveSG))
  )

# Create the formatted table
summary_stats |>
  kbl(caption = "Table 1: Summary Statistics of Key Variables (n = 278 golfers)",
      digits = 2,
      col.names = c("Variable", "Mean", "SD", "Min", "Max"),
      align = c('l', 'c', 'c', 'c', 'c')) |>
  kable_classic(full_width = FALSE) |>
  footnote(general = "Data aggregated from 19 PGA Tour tournaments in 2022. SD = Standard Deviation.",
           general_title = "Note:") |>
  row_spec(1:3, background = "#F9F9F9")


plot_putting <- pga_agg |>
  ggplot(aes(x = avg_puttsSG, y = avg_Score)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(
    title = "Strokes Gained: Putting vs Scoring Average",
    x = "Strokes Gained Putting (Average)",
    y = "Scoring Average",
    caption = "Lower scores indicate better performance"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
print(plot_putting)

library(stargazer)
regp <- lm(avg_Score ~ avg_puttsSG, data = pga_agg)
summary(regd)
stargazer(regp)


stargazer(regp,
          type = "latex",
          title = "Regression Results: Putting Performance and Scoring Average",
          dep.var.labels = "Scoring Average",
          covariate.labels = "Strokes Gained: Putting",
          align = TRUE,
          ci = TRUE,  # Show confidence intervals instead of standard errors
          single.row = TRUE,  # Put coefficients and SEs on same line
          notes = "Data aggregated from 19 PGA Tour tournaments in 2022.",
          notes.append = FALSE,
          header = FALSE)

screenreg(regp, 
          custom.model.names = "Putting Model",
          custom.coef.names = c("Intercept", "Strokes Gained: Putting"))

par(mfrow = c(2, 2))
plot(regp, which = 1:2)
plot(regd, which = 1:2)



library(ggplot2)
library(gridExtra)
library(dplyr)

# Extract residuals and fitted values
residuals_data <- data.frame(
  fitted = c(fitted(regp), fitted(regd)),
  residuals = c(residuals(regp), residuals(regd)),
  model = rep(c("Model P", "Model D"), each = length(residuals(regp)))
)

# Create compact residual plots
resid_plot <- ggplot(residuals_data, aes(x = fitted, y = residuals)) +
  geom_point(size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.3) +
  facet_wrap(~ model, ncol = 2, scales = "free") +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 6),
    strip.text = element_text(size = 7, margin = margin(1,0,1,0)),
    panel.spacing = unit(0.2, "cm"),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
  ) +
  labs(x = "Fitted Values", y = "Residuals")

# Create compact QQ-plots
qq_data <- data.frame(
  theoretical = c(qqnorm(residuals(regp), plot.it = FALSE)$x,
                  qqnorm(residuals(regd), plot.it = FALSE)$x),
  sample = c(sort(residuals(regp)), sort(residuals(regd))),
  model = rep(c("Model P", "Model D"), each = length(residuals(regp)))
)

qq_plot <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
  geom_point(size = 0.5, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.3) +
  facet_wrap(~ model, ncol = 2, scales = "free") +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 6),
    strip.text = element_text(size = 7, margin = margin(1,0,1,0)),
    panel.spacing = unit(0.2, "cm"),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
  ) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Arrange plots vertically
grid.arrange(resid_plot, qq_plot, ncol = 1, heights = c(1, 1))

qqnorm(residuals(regp), main = "Q-Q Plot of Residuals")
qqline(residuals(regp), col = "red")

library(ggplot2)
putting_diagnostics <- pga_agg %>%
  mutate(
    fitted = fitted(regp),
    residuals = residuals(regp),
    sqrt_abs_resid = sqrt(abs(residuals))
  )

# Plot 1: Residuals vs Fitted
p1 <- ggplot(putting_diagnostics, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth = 0.8) +
  geom_smooth(se = F, color = "black", method = "loess", formula = y ~ x) +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values (Predicted Scoring Average)",
    y = "Residuals",
    caption = "Zero reference line in red, smooth line in black"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 6))

# Plot 2: Q-Q Plot
p2 <- ggplot(putting_diagnostics, aes(sample = residuals)) +
  stat_qq_line(color = "darkred", linewidth = 0.8, linetype = "dashed") +
  stat_qq(color = "steelblue", alpha = 0.6, size = 2) +
  labs(
    title = "Normal Q-Q Plot of Residuals",
    subtitle = "Checking for normality of errors",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 10)
  )

p1

# Combine plots side by side
p1 + p2 +
  plot_annotation(
    title = "Model 1 Diagnostic Plots: Putting Performance Regression",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )
