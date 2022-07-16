# Pretty Arrow plot in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating arrow plot from https://www.datawrapper.de/charts/

data = read.table("../data/04-arrowplot_data.csv", sep = ",", header = TRUE)
colnames(data) = c("Country", "y2000", "y2020", "group")
data$diff = data$y2020 - data$y2000

pan_ratio = unname(unlist(lapply(split(data, ~group), nrow)))

asia = split(data, ~group)[[1]]
eu = split(data, ~group)[[2]]

title_txt = "Most countries have a higher share of women in their national parliaments than twenty years ago"
subtitle_txt = "Proportion of seats held by women in national parliaments, 2000 and 2020,\nin selected countries in Europe, Central Asia, East Asia and the Pacific"

lo = layout(matrix(1:3, ncol = 1), heights = c(2, pan_ratio))
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
text(x = 0, y = 0.8, labels = title_txt, cex = 1, font = 2, adj = 0)
text(x = 0, y = 0.35, labels = subtitle_txt, cex = 1, font = 3, adj = 0)

par(mar = c(2, 7, 1, 1))

lapply(split(data, ~group), function(dat){
  plot(
    NA,
    xlim = c(0, 50),
    ylim = c(1, nrow(dat)),
    frame.plot = FALSE,
    axes = FALSE,
    xlab = NA,
    ylab = NA
  )
  abline(h = 1:nrow(dat), v = c(0, 25, 50), col = "gray90", lwd = 0.6, outer = FALSE)
  abline(v = 0, col = "gray")
  rect(xleft = dat$y2000, ybottom = 1:nrow(dat), xright = dat$y2020, ytop = 1:nrow(dat), lwd = 1.5, col = ifelse(test = dat$diff > 0, yes = "#3498db", no = "#eb2f06"), border = ifelse(test = dat$diff > 0, yes = "#3498db", no = "#eb2f06"))
  for(i in 1:nrow(dat)){
    if(dat$diff[i] > 0){
      points(x = dat$y2020[i], y = i, pch = ">", col = "#3498db")
      text(x = dat$y2020[i], y = i, labels = paste0(dat$y2020[i], "%"), col = "#0c2461", pos = 4, cex = 0.9, xpd = TRUE)
    }else{
      points(x = dat$y2020[i], y = i, pch = "<", col = "#eb2f06")
      text(x = dat$y2020[i], y = i, labels = paste0(dat$y2000[i], "%"), pos = 2, col = "#eb2f06", cex = 0.9, xpd = TRUE)
    }
  }
  
  axis(side = 2, at = 1:nrow(dat), labels = dat$Country, las = 2, tick = FALSE, line = -0.75, col = "#2c3e50", cex.axis = 1)
  axis(side = 1, at = c(0, 25, 50), labels = paste0(c(0, 25, 50), "%"), tick = FALSE, line = -1, col.axis = "gray50", cex.axis = 1)
  
  title(x = "topright", main = dat$group[1], adj = 1, col.main = "#2c3e50", cex.main = 0.9)
})

bottom_anno = split(data, ~group)[[2]]
bottom_anno = unlist(bottom_anno[nrow(bottom_anno), c(2, 3)])
axis(side = 3, at = bottom_anno, labels = c("2000", "2020"), tick = FALSE, line = -1, col.axis = "#7f8c8d", cex.axis = 0.75)
