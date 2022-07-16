# Area diagrams in R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Data source: https://app.datawrapper.de/chart/wsfqj/visualize


data = read.table(file = "../data/07-areachart_USmigration.csv", header = TRUE, sep = ",")

#Maximum
max_pop = max(rowSums(data[,2:ncol(data)]))

layout(mat = matrix(data = c(1:2), nrow = 2), heights = c(0.75, 6))

par(family = "mono")
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
title_main = "Migration to the US by world region, 1820-2009"
title_main2 = "The numbers are recorded by decade\nFor example, the numbers recorded for 1905 tell us the number of immigrants between 1900 and 1910."

title(main = title_main, cex.main = 1, line = -1, family = "Helvetica", col.main = "#2C3A47", adj = 0)
title(main = title_main2, cex.main = .75, line = -2.5, family = "Helvetica", col.main = "#2C3A47", adj = 0, font.main = 1)


par(mar = c(1, 1, 1, 3))
plot(
  NA,
  xlim = range(data$Region.and.country.of.last.residence.1),
  ylim = c(0, max_pop),
  xlab = NA,
  ylab = NA,
  frame.plot = FALSE,
  axes = FALSE
)

abline(h = pretty(c(0, max_pop)), col = "gray90")
axis(side = 4, at = pretty(c(0, max_pop)), labels = paste0(seq(0, 12, 2), "M"), las = 2, tick = FALSE, line = .1, col.axis = "#34495e", cex.axis = 0.8)
axis(side = 1, at = seq(1820, 2000, 10), tick = FALSE, cex.axis = 0.8, col.axis = "#34495e", line = -1.5)

rect(xleft = 1914, ybottom = 0, xright = 1918, ytop = max_pop, col = "#95a5a6", border = NA)
text(x = 1916, y = max_pop, labels = "WW1", cex = 0.75, pos = 3, xpd = TRUE, font = 4, adj = 1)
rect(xleft = 1939, ybottom = 0, xright = 1945, ytop = max_pop, col = "#95a5a6", border = NA)
text(x = 1942, y = max_pop, labels = "WW2", cex = 0.75, pos = 3, xpd = TRUE, font = 4, adj = 1)

cols = c("white", "#1d81a2", "#004765", "#3a96b8", "#48adc0", "#329a9b", "#2b8589", 
         "#257085", "#005d71", 
         "#b4241c", "#cd3d2e", "#dc464b", "#e65340", 
         "#ffa126", "#ffca76", "#ffe59c", "#fffbb1", "#ffdc6b", "#ffbb7f", 
         "#009a69", "#003f65", "#181818", "black")
#cols = adjustcolor(col = cols, alpha.f = 0.7)

for(i in 22:2){
  if(i > 2){
    polygon(
      c(
        data[1, "Region.and.country.of.last.residence.1"],
        data$Region.and.country.of.last.residence.1,
        data[nrow(data), "Region.and.country.of.last.residence.1"]
      ),
      c(0, rowSums(data[, 2:i, drop = FALSE]), 0),
      col = cols[i], border = cols[i]
    )
    
  }else{
    polygon(data$Region.and.country.of.last.residence.1,
            data[,i],
            col = cols[i],  border = cols[i])
  }
}

to_hghlt = c("Germany", "United.Kingdom", "Austria.Hungary","Ireland", "Philippines", "Italy", "Russia", "India", "China", "Mexico", "Central.America")

for(h in to_hghlt){
  i = which(colnames(data) == h)
  max_idx = which(data[,i] == max(data[,i]))
  y_point = data[,i] + rowSums(data[,2:(i-1), drop = FALSE])
  text(x = data[max_idx,1], y = y_point[max_idx], labels = colnames(data)[i], cex = 0.55, xpd = TRUE, font = 2, family = "mono", col = "#192a56")
}

text(x = 1900, y = 1e6, label = "EUROPE", col = "white", cex = 1.2, font = 2)
text(x = 1990, y = 2e6, label = "ASIA", col = "white", cex = 1.2, adj = 0.8, font = 2)
text(x = 1990, y = 6e6, label = "AMERICA", col = "#2c3e50", cex = 1.2, adj = 0.7, font = 2)