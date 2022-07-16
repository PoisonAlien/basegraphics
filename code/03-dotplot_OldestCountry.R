# Pretty Dot plot in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating split-barplot from https://www.datawrapper.de/charts/

data = read.table("../data/03-dotplot_data.csv", sep = ",", header = TRUE)
data$Country[1:3] = c("Monaco", "Japan", "Germany")
xlims = range(data[,2:4])

pdf(file = "03-dotplot.pdf", width = 6, height = 5, bg = "white")
par(mar = c(3, 5, 4, 1))
plot(
  NA,
  xlim = xlims,
  ylim = c(1, nrow(data)),
  frame.plot = FALSE,
  axes = FALSE,
  xlab = NA,
  ylab = NA
)

abline(h = 1:nrow(data), col = "gray90")
abline(v = pretty(xlims), col = "gray90")
axis(side = 3, at = pretty(xlims), labels = pretty(xlims), col = "gray90", tick = FALSE, line = -1, cex.axis = 0.75, col.axis = "gray50")

for(i in 1:nrow(data)){
  combined = rev(data[,"Combined"])[i]
  male = rev(data[,"Male"])[i]
  female = rev(data[,"Female"])[i]
  
  rect(xleft = male, ybottom = i-0.2, xright = female, ytop = i+0.2, col = "#bdc3c7", border = NA)
  points(x = combined, y = i, col = "#34495e", pch = 19, cex = 1.5)
  points(x = male, y = i, col = "#60a3bc", pch = 19, cex = 1.5)
  points(x = female, y = i, col = "#e58e26", pch = 19, cex = 1.5)
}

legend(
  x = 32, y = -0.5,
  legend = c("Combined", "Male", "Female"),
  border = NA,
  ncol = 3,
  col = c("#34495e", "#60a3bc", "#e58e26"),
  pch = 19,
  bty = "n", xpd = TRUE, cex = 0.9, adj = 0)

axis(
  side = 2,
  at = 1:nrow(data),
  labels = rev(data$Country),
  tick = FALSE,
  cex.axis = 0.8, las = 2, line = -1, col = "#34495e"
)

title(
  main = "Germany is the third-oldest country in the world",
  line = 3,
  adj = 0,
  xpd = TRUE,
  cex.main = 1,
  col.main = "#2c3e50"
)

title(
  main = "Median age in the three countries with the oldest population and selected other countries, in years",
  line = 2,
  adj = 0,
  xpd = TRUE,
  cex.main = 0.6,
  col.main = "#2c3e50"
)

dev.off()