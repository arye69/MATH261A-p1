# Plot 1
plot_putting <- pga_agg |>
  ggplot(aes(x = avg_puttsSG, y = avg_Score)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(
    title = "Strokes Gained: Putting vs Scoring Average",
    x = "Strokes Gained Putting (Average)",
    y = "Scoring Average",
    caption = "Lower scoring average and higher strokes gained indicate better performance"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 6))
print(plot_putting)

# Plot 2
plot_putting <- pga_agg |>
  ggplot(aes(x = avg_driveSG, y = avg_Score)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(
    title = "Strokes Gained: Driving vs Scoring Average",
    x = "Strokes Gained Driving (Average)",
    y = "Scoring Average",
    caption = "Lower scoring average and higher strokes gained indicate better performance"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 6))
print(plot_putting)

# Plot 3
plot_putting_fitted <- pga_agg |>
  ggplot(aes(x = avg_puttsSG, y = avg_Score)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", fill = "lightpink", linewidth = 0.8) +
  labs(
    title = "Strokes Gained: Putting vs Scoring Average (fitted)",
    x = "Strokes Gained Putting (Average)",
    y = "Scoring Average",
    caption = "Lower scoring average and higher strokes gained indicate better performance"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 6))

# Plot 4
plot_driving_fitted <- pga_agg |>
  ggplot(aes(x = avg_driveSG, y = avg_Score)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", fill = "lightpink", linewidth = 0.8) +
  labs(
    title = "Strokes Gained: Driving vs Scoring Average (fitted)",
    x = "Strokes Gained Driving (Average)",
    y = "Scoring Average",
    caption = "Lower scoring average and higher strokes gained indicate better performance"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 6))
