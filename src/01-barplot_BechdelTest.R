# Pretty barplots in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating barplot from https://www.datawrapper.de/charts/stacked-bar-chart


data = read.table(file = "../data/01-barplot_data.csv", sep = ",", header = TRUE)
colnames(data) = c("Decade", "3 of 3", "2 of 3", "1 of 3", "0 of 3")
rownames(data) = data$Decade
data$Decade = NULL
data = t(data)

#A simple barplot
barplot(data, horiz = T, las = 2)

#Now lets pimp it up
## Choose some color codes
colorcodes = c("#1abc9c", "#2980b9", "#d35400", "#c0392b")

pdf(file = "barplot.pdf", width = 7, height = 4, bg = "white")

#Adjust the margins (bottom, left, top, and right)
par(mar = c(4, 3, 3, 1))

b_idx = barplot(
  data,
  horiz = TRUE,
  las = 2,
  col = colorcodes,
  border = NA,
  axes = FALSE,
  names.arg = rep(NA, ncol(data))
)
mtext(text = colnames(data), side = 2, at = b_idx, las = 2, cex = 0.85, col = "#2c3e50")
abline(v = seq(0, 100, 50), col = "gray90", lty = 1)
axis(
  side = 3,
  at = seq(0, 100, 50),
  labels = paste0(seq(0, 100, 50), "%"),
  tick = FALSE,
  col.axis = "gray",
  line = -1,
  lwd = 0, cex.axis = 0.75
)

mainttl = "Percentage of films that pass the Bechdel test"
subttl = "Dataset includes 7,924 films.\nDataset has many more ratings for films released in recent decades, so earlier decades are likely to be less accurately represented.\nSome ratings given to films have been contested and may not be accurate."
subttl2 = "Source: Bechdel Test Website"
title(main = mainttl, line = 2, cex.main = 0.9, col = "#2c3e50")
title(
  main = NA,
  sub = subttl,
  line = 3,
  col.sub = "#7f8c8d",
  adj = 1,
  font = 3, cex = 0.3, cex.sub = 0.5
)
title(
  main = NA,
  sub = subttl2,
  line = 1.5,
  col.sub = "#7f8c8d",
  adj = 1,
  font = 3, cex = 0.3, cex.sub = 0.5
)

legend(
  x = -1, y = -0.5,
  legend = rownames(data),
  border = NA,
  ncol = nrow(data),
  col = colorcodes,
  pch = 15,
  bty = "n", xpd = TRUE, title = "criteria passed", cex = 0.9, adj = 0)

for(i in 1:ncol(data)){
  text(
    y = b_idx[i],
    x = cumsum(data[, i]),
    labels = paste0(data[, i], "%"),
    cex = .6, adj = 1, col = "#ecf0f1"
  )
}

dev.off()
