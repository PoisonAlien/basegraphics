data = read.csv(file = 'data/19-barcodechart_USagegroup.csv')
rownames(data) = data$name
data$name = NULL
data = apply(data, 1, function(x) x/sum(x))

par(mar = c(2.4, 4, 2, 1), col = '#2c3e50', family = "serif")

plot(NA, ylim = c(0, nrow(data)+0.5), xlim = c(0, 0.2), axes = FALSE, xlab = NA, ylab = NA)
abline(v = seq(0, 0.3, 0.05), col = 'gray70', lwd = 0.2)
abline(h = 1:nrow(data), col = 'gray80', lwd = 0.2, lty = 2)
lapply(1:nrow(data), function(idx){
  d = data[idx,]
  points(x = d, y = rep(idx, length(d)), pch = '|')
})
axiscol = grDevices::adjustcolor(col = '#2c3e50', alpha.f = 0.9)
axis(side = 2, at = 1:nrow(data), labels = c('<10', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '>80'), las = 2, tick = FALSE, col.axis = axiscol, cex.axis = 0.7)
axis(side = 1, at = seq(0, 0.3, 0.05), labels = seq(0, 0.3, 0.05) * 100, tick = FALSE, line = -1, col.axis = axiscol, cex.axis = 0.7)
mtext(text = 'Population (%)', side = 1, line = 1, adj = 1, cex = 0.6)
mtext(text = 'Age (years)', side = 2, line = 3, adj = 0, cex = 0.6, las = 2, at = nrow(data)+1.2, padj = 3)

title(main = "Percentage of each age class in every state of USA", cex.main = 0.85, adj = 0, col.main = "#2c3e50")
