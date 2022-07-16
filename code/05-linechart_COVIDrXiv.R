# Pretty Line chart in base R
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Visualizing COVID-19 biorxiv/medrxiv articles

arxiv_data = read.delim(file = "../data/COVID_rxiv_2020-12-18.tsv")
arxiv_data$rel_date = as.Date.character(arxiv_data$rel_date)
date_lvls = seq.Date(as.Date(arxiv_data[1,"rel_date"]), as.Date(arxiv_data[nrow(arxiv_data),"rel_date"]), 1)
arxiv_data$date = factor(x = arxiv_data$rel_date, levels = as.character.Date(date_lvls), ordered = TRUE )

biorxiv = data.frame(n_articles = unlist(lapply(split(arxiv_data[arxiv_data$rel_site %in% "bioRxiv",], ~date), nrow)))
medrxiv = data.frame(n_articles = unlist(lapply(split(arxiv_data[arxiv_data$rel_site %in% "medRxiv",], ~date), nrow)))

data = merge(biorxiv, medrxiv, by = "row.names", suffixes = c("_biorxiv", "_medrxiv"))
data$n_articles = rowSums(data[,c("n_articles_biorxiv", "n_articles_medrxiv")])
rownames(data) = data$Row.names
#data = data.frame(n_articles = unlist(lapply(split(arxiv_data, ~date), nrow)))
data$ym = substr(x = rownames(data), start = 1, stop = 7)
data$yr = substr(x = rownames(data), start = 1, stop = 4)

month_lines = cumsum(lapply(split(data, ~ym), nrow))
year_lines = cumsum(lapply(split(data, ~yr), nrow))
#
heatcols = colorRampPalette(c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"))(180)

par(mar = c(3, 3, 4, 1), family = "mono")
plot(NA, xlim = c(0, nrow(data)), ylim = c(0, 170), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
abline(v = month_lines, col = "gray90", lty = 1, lwd = 0.6)
abline(v = year_lines[c("2020", "2021")], col = "#2c3e50", lwd = 0.9)
abline(h = seq(0, 150, 25), col = "gray90", lwd = 0.9, lty = 2)

axis(side = 1, at = month_lines, labels = substr(x = names(month_lines), 6, 7), las = 2, tick = FALSE, cex.axis = 0.6, line = -0.75, font.axis = 3, col.axis = "#95a5a6")
axis(side = 2, at = seq(0, 150, 25), labels = seq(0, 150, 25), las = 2, tick = FALSE, line = -0.5, col.axis = "#95a5a6")
axis(side = 3, at = year_lines[c("2020", "2021")], labels = c("2020", "2021"), tick = FALSE, line = -1, col.axis = "#8e44ad")

pnt_col = "#95a5a6"
# points(
#   x = 1:nrow(data),
#   y = data$n_articles,
#   pch = 20,
#   col = adjustcolor(col = pnt_col, alpha.f = .5), cex = 0.9
# )
#lines(smooth.spline(x = 1:nrow(data), y = data$n_articles), col = pnt_col, lwd = 3)

#e67e22
biorxiv_col = "#e67e22"
points(
  x = 1:nrow(data),
  y = data$n_articles_biorxiv,
  pch = 20,
  col = adjustcolor(col = biorxiv_col, alpha.f = 0.2), cex = 0.9
)
lines(smooth.spline(x = 1:nrow(data), y = data$n_articles_biorxiv), col = biorxiv_col, lwd = 3)

medrxiv_col = "#27ae60"
points(
  x = 1:nrow(data),
  y = data$n_articles_medrxiv,
  pch = 20,
  col = adjustcolor(col = medrxiv_col, alpha.f = 0.2), cex = 0.9
)
lines(smooth.spline(x = 1:nrow(data), y = data$n_articles_medrxiv), col = medrxiv_col, lwd = 3)

#mtext(text = "Month", side = 1, line = 1.5, font = 2)
#mtext(text = "# of articles", side = 2, line = 2, font = 2)

title(main = "Frequency of COVID19 articles in bioRxiv and medRxiv", adj = 0, line = 2, col.main = "#34495e", cex.main = 1)

legend(x = "topleft", legend = c("bioRxiv", "medRxiv"), col = c(biorxiv_col, medrxiv_col), bty = "n", lwd = 2)