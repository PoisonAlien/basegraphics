lo_mat = matrix(data = c(1, 1, 2, 2, 3, 4), ncol = 2, byrow = TRUE)
lo = layout(mat = lo_mat, widths = c(4, 0.6), heights = c(1, 0.8, 4))

title1 = "WHERE ARE THE CLEAN-ENERGY TECHNOLOGIES"
title2 = "M A N U F A C T U R E D?"
title3 = "As the market for low-emission solutions expands, China dominates the production of
clean energy technologies and their components."

par(mar = c(0, 0, 0, 0), bg = "#2d3436", col = "white")
plot(NA, xlim = c(0, 1), ylim = c(0.1, 1), axes = FALSE)
text(x = 0.5, y = 0.9, label = title1, font = 2, cex = 1.2)
text(x = 0.5, y = 0.7, label = title2, font = 2, cex = 1.4)
text(x = 0.5, y = 0.3, label = title3, xpd = TRUE, cex = 1.1)

par(mar = c(0, 0, 0, 0), bg = "#2d3436", col = "white")
plot(NA, xlim = c(0, 1), ylim = c(0.1, 0.6), axes = FALSE)
country_cols = c("#c0392b", "#e67e22", "#16a085", "#3498db", "#95a5a6")
names(country_cols) = c("China", "AsiaPacific", "Europe", "NorthAmerica", "Other")

legend(x = 0, y = 0.5, legend = names(country_cols),
       col = country_cols, pch = 15, ncol = 5, cex = 1,
       border = NA, xpd = TRUE, box.col = "white", pt.cex = 1.8)

data = read.delim("data/18-cleanEnergy_manufacturer.tsv")
data$cat = paste(data$source, data$element, sep = "_")
n_cat = rev(table(data$cat)[unique(data$cat)])

y_bottoms = seq(0.1, 16.1, 1)
y_tops = seq(0.9, 16.9, 1)


par(mar = c(2, 4, 0.5, 0), bg = "#2d3436", col = "white")
plot(NA, xlim = c(0, 100), ylim = c(0, length(n_cat)), xlab = NA, ylab = NA, axes = FALSE)

for(i in seq_len(length(n_cat))){
  d = data[data$cat == names(n_cat)[i],]
  d_cumsum = cumsum(d$pct)
  x_right = cumsum(d$pct)
  x_left = c(0, d_cumsum[1:length(d_cumsum) - 1])
  rect(
    xleft = x_left,
    ybottom = y_bottoms[i],
    xright = x_right,
    ytop = y_tops[i],
    col = country_cols[d$Country], border = "#34495e"
  )
  text(x = 0, y = y_bottoms[i]+0.25, label = unique(d$element), cex = 0.7, pos = 2, xpd = NA)
}

at_lines = c()
for(s in unique(data$source)){
  at_lines = c(at_lines, length(unique(data[data$source == s, "element"])))
}

rect(xleft = -100, ybottom = cumsum(rev(at_lines))-0.03, xright = 100, ytop = cumsum(rev(at_lines)), border = NA, col = "white", xpd = TRUE)
rect(xleft = 0, ybottom = 0, xright = 0.1, ytop = 17, border = NA, col = "white", lwd = 2)
rect(xleft = -100, ybottom = 0, xright = 100, ytop = 0+0.03, border = NA, col = "white", lwd = 2, xpd = NA)

abline(v = 65, lty = 2)
text(x = 65, y = 17.5, label = "China's avergae: 65%", col = country_cols["China"], xpd = NA, cex = 0.7, adj = -0.1)
text(x = 50, y = 18.5, label = "Shares of manufacturing capacity by region, 2021", col = "white", xpd = NA, cex = 0.8, adj = 0.5, font = 2)
text(x = 0, y = 17.5, label = "Component", col = "white", xpd = NA, cex = 0.8, adj = 0.1, font = 4, pos = 2)
text(x = 105, y = 17.5, label = "Technology", col = "white", xpd = NA, cex = 0.8, adj = 0.1, font = 4, pos = 4)
axis(side = 1, at = seq(0, 100, 20), line = 0, col = "white", col.ticks = "white", col.axis = "white")

par(mar = c(2, 0, 0.5, 0), bg = "#2d3436", col = "white")
plot(NA, xlim = c(0, 1), ylim = c(0, length(n_cat)), xlab = NA, ylab = NA, axes = FALSE)

lbs = c("Electrolyzers", "Heat Pumps", "Fuel cell\nTrucks", "EV", "Solar",
        "Onshore\nWind", "Offshore\nWind")
text(x = 0, y = c(0.25, 1.5, 3, 6, 9.5, 12.5, 15.5), labels = lbs, adj = 1, xpd = NA, cex = 0.7, pos = 4, font = 2)
rect(xleft = -200, ybottom = cumsum(rev(at_lines))-0.03, xright = 100, ytop = cumsum(rev(at_lines)), border = NA, col = "white", xpd = TRUE)
