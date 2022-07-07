# Pretty split-barplots in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating split-barplot from https://www.datawrapper.de/charts/

data = read.table("../data/02-splitbarplot_data.csv", sep = ",", header = TRUE)
max_count = max(data[,c("male", "female")]) #Maximum value

pdf(file = "02-splitbarplot.pdf", width = 5, height = 4, bg = "white")
par(mar = c(2, 3, 2, 1))
plot(
  NA,
  xlim = c(-max_count, max_count),
  ylim = c(1, nrow(data)),
  frame.plot = FALSE,
  axes = FALSE,
  xlab = NA,
  ylab = NA
)

axis(
  side = 2,
  at = 1:nrow(data)+0.4,
  labels = rev(data$age.span),
  tick = FALSE,
  cex.axis = 0.8, las = 2, line = -1, col = "#34495e"
)

for(i in 1:nrow(data)){
  row_idx = rev(1:nrow(data))[i] #Plot the data from bottom to top
  
  rect(xleft = -max_count, ybottom = i, xright = -10, ytop = i+0.9, col = "#ecf0f1", border = NA)
  
  rect(
    xleft = -data[row_idx, "male"],
    xright = -10,
    ybottom = i,
    ytop = i + 0.9, col = "#2980b9", border = NA,
  )
  pretty_pop_male = ifelse(test = data[row_idx, 2] < 1e6,
                             yes = paste0(round(data[row_idx, 2] / 1000, 1), "k"),
                             no = paste0(round(data[row_idx, 2] / 1e6, 1), "m"))
  
  if(data[row_idx, 2] < 1e6){
    text(
      x = -data[row_idx, "male"],
      y = i + 0.4,
      labels = pretty_pop_male,
      pos = 2,
      outer = TRUE,
      xpd = TRUE, cex = 0.7, col = "black"
    )
  }else{
    text(
      x = -500,
      y = i + 0.4,
      labels = pretty_pop_male,
      pos = 2,
      outer = TRUE,
      xpd = TRUE, cex = 0.7, col = "white"
    )
  }
  
  
  
  rect(xleft = -10, ybottom = i, xright = max_count, ytop = i+0.9, col = "#ecf0f1", border = NA)
  rect(
    xleft = data[row_idx, "female"],
    xright = 10,
    ybottom = i,
    ytop = i + 0.9, col = "#c0392b", border = NA
  )
  pretty_pop_female = ifelse(test = data[row_idx, 3] < 1e6,
                           yes = paste0(round(data[row_idx, 3] / 1000, 1), "k"),
                           no = paste0(round(data[row_idx, 3] / 1e6, 1), "m"))
  
  if(data[row_idx, 2] < 1e6){
    text(
      x = data[row_idx, "female"],
      y = i + 0.4,
      labels = pretty_pop_male,
      pos = 4,
      outer = TRUE,
      xpd = TRUE, cex = 0.7, col = "black"
    )
  }else{
    text(
      x = 500,
      y = i + 0.4,
      labels = pretty_pop_female,
      pos = 4,
      outer = TRUE,
      xpd = TRUE, cex = 0.7, col = "white"
    )
  }
}

text(x = -max_count, y = nrow(data)+0.45, labels = "male", adj = 0, cex = 0.8, font = 1)
text(x = max_count, y = nrow(data)+0.45, labels = "female", adj = 1, cex = 0.8, font = 1)

title(main = "Population projection for Germany, 2020", line = 1, adj = 0, col.main = "#2c3e50")
title(main = NA, sub = "Projection from 2015\nbased on continued trend with higher immigration
", line = 1, font.sub = 3, cex.sub = 0.6, adj = 1, col.sub = "#7f8c8d")

dev.off()