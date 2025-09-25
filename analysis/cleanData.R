# Clean data
pga_agg <- PGA2022 |>
  group_by(playerName) |>
  summarise(
    avg_Score = mean(avgScore, na.rm = TRUE),
    avg_puttsSG = mean(puttsSG, na.rm = TRUE),
    avg_driveSG = mean(driveSG, na.rm = TRUE),
    Tournaments_Played = n()
  )

pga_agg <- pga_agg[-c(77, 81),]

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
  kbl(caption = "Summary Statistics of Key Variables (n = 278 golfers)",
      digits = 2,
      col.names = c("Variable", "Mean", "SD", "Min", "Max"),
      align = c('l', 'c', 'c', 'c', 'c'),
      booktabs = TRUE) |>
  kable_styling(full_width = FALSE, 
                latex_options = c("hold_position", "scale_down"))