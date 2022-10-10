data = read.delim(file = "data/14-barplot_BerlinMarathon2022.tsv", header = FALSE)

data$secs = apply(X = data, MARGIN = 1, FUN = function(x){
  sapply(strsplit(x = x, split = ":"), function(x) {
    as.numeric(x[1]) * 3600 + as.numeric(x[2]) * 60 + as.numeric(x[3])
  })
})
data$mins = data$secs / 60
data$cut = cut(x = data$mins, breaks = seq(90, 480, 1))
hcol = adjustcolor(col = '#d35400', alpha.f = 0.4)

par(mar = c(3, 2, 3, 1), family = "mono")
plot.new()
plot.window(xlim = c(0, 390), ylim = range(table(data$cut)))
abline(v = seq(0, 390, 30), col = "#ecf0f1")
points(table(data$cut), type = 'h', col = hcol)
#axis(side = 1, at = seq(0, 390, 30), labels = seq(90, 480, 30)/60, las = 2, col.axis = "#34495e", tick = FALSE, )
text(
  x = seq(0, 390, 30),
  y = -50,
  labels = c(
    "1:30",
    "2:00",
    "2:30",
    "3:00",
    "3:30",
    "4:00",
    "4:30",
    "5:00",
    "5:30",
    "6:00",
    "6:30",
    "7:00",
    "7:30",
    "8:00"
  ),
  srt = -45,
  xpd = TRUE,
  cex = 0.8, col = "#2c3e50"
)

kipchoge_slot_idx = which(names(table(data$cut)) == "(121,122]")
assefa_slot_idx = which(names(table(data$cut)) == "(135,136]")

median_time = median(data$secs) #245.7 i.e: 04:05:42
median_time_idx = which(names(table(data$cut)) == "(245,246]")

rect(xleft = kipchoge_slot_idx, ybottom = 0, xright = kipchoge_slot_idx, ytop = 100, border = "#34495e", lwd = 1.2)
text(x = kipchoge_slot_idx, y = 90, labels = "KIPCHOGE\n2:01:09", cex = 0.7, pos = 3, font = 2)

rect(xleft = assefa_slot_idx, ybottom = 0, xright = assefa_slot_idx, ytop = 200, border = "#95a5a6", lwd = 1.2)
text(x = assefa_slot_idx, y = 190, labels = "ASSEFA\n2:15:37", cex = 0.7, pos = 3, font = 2)

rect(xleft = median_time_idx, ybottom = 0, xright = median_time_idx, ytop = 90, border = "#34495e", lwd = 1.2)
text(x = median_time_idx, y = 90, labels = "MEDIAN\n04:05:42", cex = 0.7, pos = 3, font = 2)

mtext(text = "TIME [H:MM]", side = 1, line = 2)
mtext(text = "NUMBER OF ATHLETES", side = 2, line = 0.5)

title(main = "BMW BERLIN-MARATHON 2022: FINISH TIMES", col.main = "#2c3e50")
