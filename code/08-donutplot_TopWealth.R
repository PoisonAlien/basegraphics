# Pretty donut plots in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating donut plot from https://www.datawrapper.de/charts/

data = read.table("../data/08-donutplot_data.csv", sep = ",", header = TRUE)

#pdf(file = "08-donutplot.pdf", height = 4, width = 5, bg = "white")

lo =layout(mat = matrix(data = c(1, 1, 1, 2:7), nrow = 3, ncol = 3, byrow = TRUE), heights = c(1.7, 3, 3))

par(mar = c(0, 0, 0, 0))
title_main = "Who gets which slice of the pie?"
title_sub = "How much of the wealth of a country do the poorest 50% own?\nHow much do the top 10% own?"

plot(NA, xlim = c(0, 1), ylim = c(0, 1), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
text(x = 0, y = 0.9, labels = title_main, cex = 1.5, font = 2, adj = 0)
text(x = 0, y = 0.6, labels = title_sub, cex = 1.2, font = 3, adj = 0)
legend(x = .1, y = 0.4, legend = c("bottom 50%", "next 40%", "top 10%"), col = c("#b33939", "#f7f1e3", "#40407a"), pch = 15, ncol = 3, bty = "n", cex = 1.4)

par(mar = c(2, 2, 2, 2))

for (i in 2:ncol(data)) {
  pie(
    data[, i],
    radius = 1,
    clockwise = TRUE,
    labels = paste0(round(data[, i]), "%"), border = "white",
    col = c("#b33939", "#f7f1e3", "#40407a")
  )
  points(x = 0, y = 0, pch = 19, cex = 10, col = "white", lwd = 0)
  title(main = colnames(data)[i], adj = 0)
}

#dev.off()
