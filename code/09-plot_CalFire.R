# Widlfiles in California
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating plot from https://www.buzzfeednews.com/article/peteraldhous/california-wildfires-people-climate
# Original ggplot source: https://buzzfeednews.github.io/2018-07-wildfire-trends/

#download.file(url = "https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_frap.csv", destfile = "data/09-calfire_frap.csv")
data = read.delim(file = "data/09-calfire_frap.csv", sep = ",")

date_lvls = seq.Date(as.Date("1950/01/01"), as.Date("2017/12/31"), 1)
md = names(table(substr(date_lvls, 6, 10)))

data$reporting_date = factor(data$alarm_date, levels = as.character.Date(date_lvls), ordered = TRUE)
data$reporting_date = as.Date(data$reporting_date)
data = data[order(data$reporting_date),]
data$md = substr(x = data$reporting_date, 6, 10)
data$md = factor(x = data$md, levels = md, ordered = TRUE)

#layout(mat = matrix(data = c(1:2), nrow = 2), heights = c(0.75, 6))

par(mar = c(1, 3, 3, 1), family = "mono")
plot(NA, pch = NA, axes = FALSE, ylim = c(1950,2017), xlim = c(1, 366))
abline(h = 1950:2017, v = c(0, cumsum(table(substr(md, 1, 2)))), col = "gray90")
abline(h = seq(1950, 2010, 10), col = adjustcolor("#34495e", 0.5), lwd = 1)
symb_cols = adjustcolor(col = "#e74c3c", alpha.f = 0.6)
symbols(
  x = data$md,
  y = data$year_, add = TRUE,
  circles = sqrt(data$gis_acres / pi),
  inches = 0.12, bty = "n", pch = 19, bg = symb_cols, fg = symb_cols
)
axis(side = 1, at = cumsum(table(substr(md, 1, 2))), labels = month.abb, tick = FALSE, col.axis = "#34495e", line = -1.2, cex.axis = 0.7)
axis(side = 2, at = seq(1950, 2010, 10), las = 2, tick = FALSE, line = -1, col.axis = "#34495e")

title("Big fires have gotten more common in California", adj = 0, line = 2, family = "Helvetica", col.main = "#2C3A47")
title("Each fire is a dot that is scaled by the area that ultimately burned, centered on the date on which\nthe alarm was sounded", adj = 0, line = 0, font.main = 1, cex.main = 0.8, family = "Helvetica", col.main = "#2C3A47")
