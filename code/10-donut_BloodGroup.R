# Blood group distribution across the world
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating plot from https://www.visualcapitalist.com/visualizing-the-most-widespread-blood-types-in-every-country/

data = read.delim(file = "data/10-donut_BloodGroup.tsv")
data2 = read.delim(file = "data/10-donut_BloodGroupCountry.tsv")


cols = hcl.colors(n = 8, palette = "Viridis", alpha = 0.8)
cols = c("#c23616", "#f0932b", "#f9ca24", "#6ab04c", "#40739e", "#192a56", "#ff9ff3", "purple")

# lomat = matrix(data = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,
#                         6,6,6,7,7,7,8,8,8,9,9,9,10,10,10,
#                         11,11,11,0,12,12,12,0,13,13,13,0,14,14,14,
#                         0,0,15,15,15,0,16,16,16,0,17,17,17,0,0),
#                         nrow = 4, byrow = TRUE)

lomat = matrix(data = c(rep(1, 5), 2:11, rep(12, 5)), nrow = 4, ncol = 5, byrow = TRUE)

lo = layout(mat = lomat, heights = c(0.5, 3, 3, 1))

par(mar = c(0, 0, 0, 0), bg = "#ecf0f1")
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE)
title(
  main = "Distribution of Blood types by country",
  line = -3,
  cex.main = 2.3,
  col.main = "white",
  xpd = TRUE,
  outer = TRUE,
  family = "Helvetica", col.main = "#222f3e"
)
# text(
#   x = 0.5,
#   y = 0.3,
#   label = "Distribution of Blood types by country",
#   cex = 2,
#   font = 2,
#   col = "white", xpd = TRUE, family = "Helvetica"
# )


par(mar = c(1, 1, 1, 1))

for(i in 1:nrow(data2)){
  pie(
    as.numeric(data2[i, 2:9]),
    clockwise = TRUE,
    col = cols,
    border = cols,
    labels = paste0(as.numeric(data2[i, 2:ncol(data)])[1:4], "%"), radius = 1.05, col.lab = "white", xpd = TRUE, cex = 0.8
  ) #
  points(x = 0, y = 0, pch = 19, cex = 8, col = "#ecf0f1", lwd = 0)
  #legend(x = "bottomright", legend = data[i,"Region"], bty = "n", cex.title = 1.2, adj = 0)
  text(
    x = 0,
    y = -2,
    label = paste0(data2[i, "Country"], "\n", data2[i, "Pop"]),
    family = "mono",
    cex = 1.5,
    xpd = TRUE,
    font = 2,
    col = "#535c68"
  )
}

plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE)
legend(
  x = "center",
  legend = c("O+", "A+", "B+", "AB+", "O-", "A-", "B-", "AB-"),
  col = cols,
  ncol = 8,
  pch = 15, bty = "n", cex = 1.7
)

# for(i in 1:nrow(data)){
#   pie(
#     as.numeric(data[i, 2:ncol(data)]),
#     clockwise = TRUE,
#     col = cols,
#     border = cols,
#     labels = paste0(as.numeric(data[i, 2:ncol(data)]), "%"), radius = 1.05, col.font = cols, xpd = TRUE, cex = 0.8
#   ) #
#   points(x = 0, y = 0, pch = 19, cex = 8, col = "white", lwd = 0)
#   #legend(x = "bottomright", legend = data[i,"Region"], bty = "n", cex.title = 1.2, adj = 0)
#   text(x = 0, y = -2, label = data[i,"Region"], family = "mono", cex= 1.4, xpd= TRUE, font = 2)
# }


