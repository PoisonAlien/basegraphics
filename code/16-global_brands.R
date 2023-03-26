x = read.csv(file = "data/16-global_brands.csv")
single_entries = names(which(table(x$name) == 1))
x = x[!x$name %in% single_entries,]


mod_data = lapply(split(x, x$name), function(d){
  brand = unique(d$name)
  category = unique(d$category)
  d_brand = lapply(1:(nrow(d)-1), function(idx){
    val_idx = d[idx, "value"]
    val_idx_next = d[idx+1, "value"]

    date_idx = d[idx, "date"]
    date_idx_next = d[idx+1, "date"]

    seq_date = seq.Date(from = as.Date(date_idx), to = as.Date(date_idx_next), by = 'month')

    data.frame(date = seq_date, value = round(seq(from = val_idx, to = val_idx_next, length.out = length(seq_date))))
  })

  d_brand = do.call(what = "rbind", d_brand)
  d_brand$brand = brand
  d_brand$category = category
  d_brand
})

colpal = c("#f39c12", "#f1c40f", "#3498db", "#ecf0f1", "#95a5a6", "#2c3e50",
           "#9b59b6", "#e74c3c", "#bdc3c7", "#e67e22", "#c0392b", "#27ae60",
           "#d35400", "#7f8c8d", "#16a085", "#8e44ad", "#1abc9c", "#34495e",
           "#2980b9", "#2ecc71", "#9AECDB", "#F8EFBA")

mod_data = do.call(what = "rbind", mod_data)
mod_data$date = as.Date(as.character(mod_data$date))
names(colpal) = names(table(mod_data$category))


fnames = lapply(split(mod_data, mod_data$date), function(pd){
  pd = pd[!duplicated(pd$brand),]
  fname = NA
  if(nrow(pd) >= 12){
    pd = pd[order(pd$value, decreasing = T),][1:12,]

    pd$value = ifelse(test = is.na(pd$value), yes = 0, no = pd$value)

    fname = tempfile(pattern = "gb", fileext = ".png")
    png(filename = fname, width = 750, height = 655, bg = "white", res = 120)
    par(mar = c(0, 6, 0, 0))

    plot(NA, xlim = c(0, max(pd$value)), ylim = c(0, 13), frame.plot = FALSE, axes = FALSE, xlab =NA, ylab = NA)
    #main rect
    rect(xleft = 0, ybottom = seq(0.1, 11.1, by = 1), ytop = seq(0.9, 11.9, by = 1), xright = rev(pd$value), col = "#fdcb6e", border = "#fdcb6e")
    #add value to the end of rect
    text(x = rev(pd$value), y = seq(0.45, 11.45, by = 1), labels = rev(pd$value), adj = 1, cex = 0.6, col = "#636e72")
    #Add category to the beginning
    text(x = 0, y = seq(0.45, 11.45, by = 1), labels = rev(pd$category), adj = 0, cex = 0.6, col = "#636e72")
    #add brand name as row names
    text(x = -0.5, y = seq(0.45, 11.45, by = 1), labels = rev(pd$brand), adj = 1.1, xpd = TRUE, cex = 0.9, col = "#34495e")
    #add top y axis labs
    text(x = pretty(c(0, pd$value)), y = 12, labels = pretty(c(0, pd$value)), pos = 3, cex = 0.5)
    rect(xleft = pretty(c(0, pd$value)), ybottom = 0, xright = pretty(c(0, pd$value)), ytop = 12, col = "maroon", border = "white")
    #add year in progress
    legend(x = "bottomright", legend = unique(substr(pd$date, 1, 4)), col = "#2c3e50", bty = "n", text.font = 2, cex = 1.5)
    dev.off()

  }
  fname
})

pngs = unlist(fnames, use.names = FALSE)
cmd = paste0("convert -loop 0 -delay 8 ", paste(pngs, collapse = ' '), " globalbrands.gif")
sys.log = system(command = cmd, intern = TRUE)
