# Putting regression
regp <- lm(avg_Score ~ avg_puttsSG, data = pga_agg)
sump <- summary(regp)

stargazer(regp,
          title = "Regression of scoring average on putting performance",
          font.size = "footnotesize",
          header = FALSE)
# Resids / QQ plot
plot(regp, which = 1)
plot(regp, which = 2)


# Driving regression
regd <- lm(avg_Score ~ avg_driveSG, data = pga_agg)
sumd <- summary(regd)

stargazer(regd,
          title = "Regression of scoring average on driving performance",
          font.size = "footnotesize",
          header = FALSE)
# Resids / QQ plot
plot(regd, which = 1)
plot(regd, which = 2)