# The 20 Fastest Growing Jobs in the Next Decade
# MIT License
# Copyright (c) 2022 Anand Mayakonda <anandmt3@gmail.com>
# Recreating plot from https://www.visualcapitalist.com/the-20-fastest-growing-jobs-in-the-next-decade/

data = read.delim(file = "data/11-bubblechart_furturejobs2030.tsv")

cols = c("#16a085", "#2980b9", "#f39c12", "#8e44ad", "#e74c3c")
cols = adjustcolor(col = cols, alpha.f = 0.6)
names(cols) = names(table(data$categ))

par(mar = c(1, 4, 4, 4), family = "sans", bg = "#ecf0f1")
plot(NA, xlim = range(pretty(data$median_ann_wage)), ylim = c(-40, 80), axes = FALSE, xlab = NA, ylab = NA)
grid(col = "gray80")

symbols(
  x = data$median_ann_wage,
  y = data$pct_empl_change,
  circles = sqrt(abs(data$num_empl_change / pi)),
  add = TRUE, inches = 0.9, bg = cols[data$categ], fg = "#ecf0f1"
)

#rect(xleft = seq(45, 105, 15)*1000, ybottom = 75, xright = seq(60, 120, 15)*1000, ytop = 80, col = cols, border = cols)
rect(xleft = seq(70, 110, 10)*1000, ybottom = 75, xright = seq(80, 120, 10)*1000, ytop = 80, col = cols, border = cols)
text(x = seq(75, 115, 10)*1000, y = 85, labels = c("Computer&\nmath", "Health\nrerlated", "Office\nsupport", "Other", "Production"), xpd = TRUE, adj = 0.5, family = "sans", cex = 0.65)

if("berryFunctions" %in% installed.packages()){
  berryFunctions::roundedRect(xleft = 68*1000, ybottom = 72, xright = 122*1000, ytop = 90, xpd = FALSE, lwd = 1.4)
}else{
  rect(xleft = 40*1000, ybottom = 72, xright = 122*1000, ytop = 90, xpd = FALSE, lwd = 1.4)
}

axis(side = 2, at = seq(0, 80, 20), tick = FALSE, las = 2, labels = paste0(seq(0, 80, 20), "%"), col.axis = "#34495e", line = -0.75, cex.axis = 0.8)
mtext(text = "Employment change %\n2020-2030", side = 2, at = 60, line = 2, col = "#2c3e50", cex = 0.9)

axis(side = 4, at = seq(-20, -40, -20), tick = FALSE, las = 2, labels = paste0(seq(-20, -40, -20), "%"), col.axis = "#34495e", cex.axis = 0.8, line = -0.75)
rect(xleft = 20000, ybottom = 0, xright = 120000, ytop = 0)
text(x = pretty(data$median_ann_wage), y = 0, labels = paste0(pretty(data$median_ann_wage)/1000, "K"), pos = 3, col = "#34495e", xpd = TRUE, cex = 0.8)
text(x = 120000, y = -3, label = "Median annual wage", family = "mono", cex = 0.8, font = 2, adj = 1, col = "#2c3e50")


#Annotate some bubbles manually
text(x = 30000,  y = 50, label = ("Home health &\npersonal care aids"), cex = 0.6, col = "#34495e")
text(x = 118000,  y = 65, label = ("Nurse\npractitioners"), cex = 0.6, col = "#34495e")
text(x = 95000,  y = 40, label = ("Statisticians"), cex = 0.6, adj = 1, col = "#34495e")
text(x = 60000,  y = 42, label = ("PT assistants"), cex = 0.6, col = "#34495e")
text(x = 60000,  y = 42, label = ("PT assistants"), cex = 0.6, col = "#34495e")
text(x = 56230,  y = 63, label = ("Wind turbine &\nservice technicians"), cex = 0.6, col = "#34495e")

rect(xleft = 85700, ybottom = 18, xright = 85700, ytop = 25.5, border = "#2980B999")
text(x = 85700,  y = 14, label = ("Genetic\ncounselors"), cex = 0.6, col = "#34495e")

rect(xleft = 70000, ybottom = 20, xright = 70000, ytop = 29.6, border = "#2980B999")
rect(xleft = 70000, ybottom = 29.6, xright = 74560, ytop = 29.6, border = "#2980B999")
text(x = 70000,  y = 17, label = ("Epidemiologists"), cex = 0.6, col = "#34495e")

text(x = 65110,  y = -30, label = ("Executive secretaries &\n admin assistants"), cex = 0.6, col = "#34495e")
text(x = 46500,  y = -29, label = ("Watch&\nclock repairers"), cex = 0.6, col = "#34495e")
text(x = 105000,  y = -38, label = ("Nuclear power\nreactor operator"), cex = 0.6, col = "#34495e")

rect(xleft = 22140, ybottom = -20.1, xright = 29140, ytop = -20.1, border = "#8E44AD99")
text(x = 22140,  y = -22, label = ("Florists"), cex = 0.6, col = "#34495e")

text(x = 31630,  y = -34, label = ("Cutters&\ntrimmers"), cex = 0.6, col = "#34495e")

title(main = "The 20 Fastest Growing and Declining Jobs", adj = 0, line = 2.5, cex.main = 1.4, col.main = "#2c3e50")
title(main = "OVER THE NEXT DECADE", adj = 0, line = 1.5, cex.main = 0.9, col.main = "#2c3e50")
