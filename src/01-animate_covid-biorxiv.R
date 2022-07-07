#!/usr/bin/env Rscript
# Source code: https://github.com/PoisonAlien/COVID_rXiv
#
# MIT License
# Copyright (c) 2020 Anand Mayakonda <anandmt3@gmail.com>
#
# Code for parsing meta-data from COVID related articles from biorxiv and medrxiv.

library(data.table)
library(jsonlite)

#api = "https://api.biorxiv.org/covid19/"
#Get top 30 most recent and parse the total number of articles
#max_articles = jsonlite::fromJSON(txt = paste0(api, "0"))
#max = max_articles$messages[,"total"]
#message("Total number of articles: ", max)
#API returns 30 entries per query
#batches = seq(0, max, 30)

# meta_data = parallel::mclapply(batches, function(batch){
#   jsonlite::fromJSON(txt = paste0(api, batch))[[2]][,c("rel_doi", "rel_site", "rel_date", "type", "category", "rel_num_authors")]
# }, mc.cores = 4)
# 
# meta_data = data.table::rbindlist(l = meta_data, use.names = TRUE, fill = TRUE)
# if(nrow(meta_data) != max){
#   warning("Some entries are missing!")
# }
# 
# #Save to file
# data.table::fwrite(x = meta_data, file = "COVID_rxiv_2020-12-18.tsv.gz", sep = "\t")

#Start from here if you have the file downloaded already

meta_data = data.table::fread(input = "COVID_rxiv_2020-12-18.tsv.gz")

meta_data = meta_data[order(rel_date, decreasing = FALSE)]
meta_data$ym = substr(meta_data$rel_date, 1, 7)

date_lvls = seq.Date(as.Date(meta_data[1,rel_date]), as.Date(meta_data[nrow(meta_data),rel_date]), 1)
meta_data[, pub_date := as.character.Date(rel_date)]
meta_data = meta_data[,pub_date := factor(x = meta_data$pub_date, levels = as.character.Date(date_lvls), ordered = TRUE)]

by_site = as.data.frame(table(meta_data$pub_date, meta_data$rel_site))
data.table::setDT(by_site)
colnames(by_site)[1] = "pub_date"
by_site = data.table::dcast(data = by_site, pub_date ~ Var2, value.var = "Freq", fill = 0)

by_site[, total := bioRxiv + medRxiv]
by_site[, bioRxiv_cumsum := cumsum(bioRxiv)]
by_site[, medRxiv_cumsum := cumsum(medRxiv)]
by_site[, total_cumsum := cumsum(total)]
by_site = by_site[order(pub_date)]

by_site$Month = substr(by_site$pub_date, 1, 7)
by_site$xax = 1:nrow(by_site)
data.table::setDF(x = by_site)


dir.create(path = "./pngs_bysrc/", showWarnings = FALSE, recursive = TRUE)
xlabs = unlist(lapply(split(by_site, as.factor(by_site$Month)), function(x){min(x$xax)}))

for(idx in seq_len(nrow(by_site))){
  data = by_site[1:idx,, drop = FALSE]
  ylims = c(0, max(data[,"total_cumsum"], na.rm = TRUE)+5)
  xlims = c(0, max(data[,"xax"])+5)
  png(filename = paste0("pngs_bysrc/", by_site[idx, "pub_date"], ".png"), width = 8, height = 5.5, units = "in", bg = "white", res = 100)
  #png("~/Downloads/test.png", width = 8, height = 5.5, units = "in", bg = "white", res = 100)
  #Set some pretty background color and change default font font
  par(bg = "#dfe4ea", family = "mono")
  plot(x = data$xax, y = data$total_cumsum, type = 'l', axes = FALSE, xlab = NA, ylab = NA, xlim = xlims, ylim = ylims, lwd = 1.2, col = "#2f3542")
  text(x = by_site[idx, "xax"], y = by_site[idx, "total_cumsum"], labels = paste0("combined [", by_site[idx, "total_cumsum"], "]"), pos = 3, xpd = TRUE, col = "#2f3542")
  
  points(x = data$xax, y = data$bioRxiv_cumsum, type = 'l', lwd = 1.2, col = "#e74c3c")
  text(x = by_site[idx, "xax"], y = by_site[idx, "bioRxiv_cumsum"], labels = paste0("bioRxiv [", by_site[idx, "bioRxiv_cumsum"], "]"), pos = 3, xpd = TRUE, col = "#e74c3c")
  
  points(x = data$xax, y = data$medRxiv_cumsum, type = 'l', lwd = 1.2, col = "#3498db")
  text(x = by_site[idx, "xax"], y = by_site[idx, "medRxiv_cumsum"], labels = paste0("medRxiv [", by_site[idx, "medRxiv_cumsum"], "]"), pos = 3, xpd = TRUE, col = "#3498db")
  
  text(x = by_site[idx, "xax"], y = by_site[idx, "cumN"], labels = by_site[idx, "N"], pos = 1, xpd = TRUE)
  axis(side = 2, at = pretty(ylims), lwd = 0, las = 2, col = "#34495e", col.axis = "#34495e")
  axis(side = 1, at = xlabs, labels = names(xlabs), las = 2, col = "#34495e", col.axis = "#34495e")
  title(main = paste0(by_site[idx, "pub_date"]), adj = 0, line = 2, font.main = 3, col.main = "#c0392b")
  title(main = paste0("bioRxiv = ", by_site[idx, "bioRxiv"], ", medRxiv = ", by_site[idx, "medRxiv"], ", total = ", by_site[idx, "total"]), adj = 0, line = 1, font.main = 3, col.main = "#34495e")
  
  par(fig = c(0.05,0.55, 0.45, 1), new = TRUE)  
  
  if(max(data$total) > 10){
    b = boxplot(total ~ Month, data = data, axes = FALSE, xlab = NA, ylab = NA, outcex = 0.1, col = "#00b894", xlim = c(1, length(table(data$Month))))  
    axis(side = 2, at = pretty(data$total), las = 2, col = "#34495e", col.axis = "#34495e", lwd = 0, line = -1, cex.axis = 0.7)
    axis(side = 1, at = 1:length(unique(data$Month)), las = 2, col = "#34495e", col.axis = "#34495e", labels = unique(data$Month), las=  2, cex.axis = 0.7, lwd = 0, line = -1)
  }
  
  dev.off()
}


#Output should be a GIF: `covid_rxiv.gif`
system(command = "convert -loop 0 -delay 3 pngs_bysrc/*.png covid_rxiv.gif")
