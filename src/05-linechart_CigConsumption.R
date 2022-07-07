# Pretty Line chart in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating line chart from https://www.datawrapper.de/charts/

data = read.table("../data/05-linechart_data.csv", sep = ",", header = TRUE)
data[data == "null"] = NA
data = apply(data, 2, as.numeric)
data = as.data.frame(data)
rownames(data) = data$country

max_count = max(data[,2:ncol(data)], na.rm = TRUE)

title_main = "The rise and fall of cigarette consumption in developed countries"
title_sub = "Sales of cigarettes per adult per day, in selected countries.\nFigures include manufactured cigarettes, as well as an estimated\nnumber of hand-rolled cigarettes,per adult (ages 15+) per day."

layout(mat = matrix(data = c(1:2), nrow = 2), heights = c(2, 6))

par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
text(x = 0, y = 0.8, labels = title_main, cex = 1, font = 2, adj = 0)
text(x = 0, y = 0.35, labels = title_sub, cex = 1, font = 3, adj = 0)

par(mar = c(3, 2, 0, 1))
plot(NA, xlim = range(data$country), ylim = c(0, max_count), xlab = NA, ylab = NA, frame.plot = FALSE, axes = FALSE)
axis(side = 2, at = seq(0, 12, 2), labels = seq(0, 12, 2), line = 0, col.axis = "gray", cex.axis = 1, las = 2, tick = FALSE)
abline(h = seq(0, 12, 2), lwd = 1, col = "gray90")

text(x = data$country[1], y = 10, labels = "Sold cigarettes\nper day per adult", cex = 0.7, xpd = TRUE, adj = 0)

for(i in 2:ncol(data)){
  points(x = data$country, y = data[,i], pch = "", type = "l", col = "#7f8c8d")  
}

hghlt = c("United.States", "Germany", "France")
hghlt_cols = c("#c0392b", "#16a085", "#2980b9")
for(hghlt_idx in seq_along(hghlt)){
  hghlt_col_idx = which(colnames(data) == hghlt[hghlt_idx])
  points(x = data$country, y = data[,hghlt_col_idx], pch = "", type = "l", col = hghlt_cols[hghlt_idx], lwd = 2)
}

text(x = data$country[1], y = c(6:4), labels = hghlt, col = hghlt_cols, adj = 0)

axis(side = 1, at = pretty(data$country), labels = pretty(data$country), line = 0, tick = TRUE, col.axis = "#7f8c8d", col = "#7f8c8d")