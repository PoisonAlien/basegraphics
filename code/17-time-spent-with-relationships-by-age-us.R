data = read.delim("data/17-time-spent-with-relationships-by-age-us.csv", sep = ",")
colnames(data) = c("Entity", "Code", "Year", "alone", "frinds", "children", "parents", "partner", "coworkers")

cols = hcl.colors(n = 6, palette = "Dark 3")

par(mar = c(2, 2, 4, 3))
plot(NA, xlim = c(15, 85), ylim = c(0, 8.4), xlab = NA, ylab = NA, axes = FALSE)
grid()
for(i in 4:ncol(data)){
  points(x = data$Year, y = data[,i]/60, type = "l", col = cols[i-3])
  points(x = data$Year, y = data[,i]/60, col = cols[i-3], cex = 0.3, pch = 19)
  text(x = 86, y = data[nrow(data),i]/60, labels = colnames(data)[i], pos = 4, xpd = TRUE, cex = 0.6)
}

ttl = "Who Americans Spend Their Time With, by Age"
subttl = "\nAverage time spent with others is measured in minutes per day,and shown by \nthe age of the respondent.This is based on averages from surveys \nbetween 2009 and 2019."

title(main = ttl, adj = 0, cex.main = 1, line = 3, col.main = "#2c3e50", family = "serif")
title(main = subttl, adj = 0, cex.main = 0.7, line = 0.9, font.main = 1, col.main = "#7f8c8d", family = "serif")

mtext(text = axTicks(side = 1), side = 1, at = axTicks(side = 1), line = 0.2, col = "#636e72", family = "mono")
mtext(text = "Age in years", side = 1, line = 0.8, las = 1, at = 70, adj = 0, cex = 0.6, col= "#3c6382")

mtext(text = axTicks(side = 2), side = 2, at = axTicks(side = 2), line = 0, las = 2, col = "#636e72", family = "mono")
mtext(text = "Time spent in hours", side = 2, line = 0.8, las = 3, at = 6, adj = 0, cex = 0.6, col= "#3c6382")
