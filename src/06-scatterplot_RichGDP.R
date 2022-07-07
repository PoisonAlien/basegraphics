# Pretty Scatter plots in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating scatter plot from https://www.datawrapper.de/charts/

#data = read.table("06-scatterplot_data.csv", sep = ",", header = TRUE)
data = data.table::fread(input = "../data/06-scatterplot_data.csv", data.table = FALSE)

data$population = as.numeric(data$population)
#data$bub_size = data$population/max(data$population, na.rm = TRUE)
cols = c("#34495e", "#ff5252", "#34ace0", "#ffb142", "#218c74", "#84817a")
cols = adjustcolor(col = cols, alpha.f = 0.6)
names(cols) = names(table(data$regions))

title_main = "The richer, the healthier"
title_sub = "GDP per person adjusted for differences in purchasing power (in 2011 international dollars) and life expectancy in years for selected countries, 2018.\nThe bigger a circle, the more people live in a country."

pdf(file = "06-scatterplot.pdf", height = 3.5, width = 5, bg = "white")

layout(mat = matrix(data = c(1:2), nrow = 2), heights = c(2, 6))

par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
text(x = 0, y = 0.8, labels = title_main, cex = 1, font = 2, adj = 0)
text(x = 0, y = 0.6, labels = title_sub, cex = 0.6, font = 3, adj = 0)

legend(x = .1, y = 0.4, legend = names(cols), col = cols, pch = 19, ncol = 3, bty = "n", cex = 0.7)



data$logGDP = log10(data$`GDP per capita`)

par(mar = c(2, 2, 0, 1))
plot(NA, xlim = range(pretty(data$logGDP)), ylim = c(50, 90), xlab = NA, ylab = NA, frame.plot = FALSE, axes = FALSE)
axl_lb = round(10^pretty(data$logGDP))
axl_lb = ifelse(test = axl_lb > 1000, yes = paste0(round(axl_lb/1000), "k"), no = round(axl_lb))

axis(side = 1, at = pretty(data$logGDP), labels = axl_lb, col.axis = "#7f8c8d", tick = FALSE, col = "gray", cex.axis = 0.8, line = -.5)
axis(side = 2, at = seq(50, 100, 10), col.axis = "#7f8c8d", tick = FALSE, col = "gray", cex.axis = 0.8, las = 2, line = -0.5)
abline(h = seq(50, 100, 10), v = pretty(data$logGDP), col = "gray90")

text(x = rev(pretty(data$logGDP))[1], y = 50, labels = "GDP per capita", col = "#34495e", adj = 1, cex = 0.7, font = 2)
text(x = pretty(data$logGDP)[1], y = 90, labels = "Life Expectancy", col = "#34495e", adj = 0, cex = 0.7, font = 2)

symbols(
  x = data$logGDP,
  y = data$`Life expectancy`,
  circles  = sqrt(data$population / pi),
  inches = 0.18, add = TRUE, bg = cols[data$regions], fg = cols[data$regions]
)

bot_cntrs = data[data$country %in% c("South Africa", "China", "Afghanistan", "Lesotho", "United Arab Emirates"),]
text(x = bot_cntrs$logGDP, y = bot_cntrs$`Life expectancy`, labels = bot_cntrs$country, pos = 1, cex = 0.6, col = "#2c2c54")
top_cntrs = data[data$country %in% c("Liberia", "Timor-Leste", "Nicaragua", "Costa Rica"),]
text(x = top_cntrs$logGDP, y = top_cntrs$`Life expectancy`, labels = top_cntrs$country, pos = 3, cex = 0.6, col = "#2c2c54")

dev.off()