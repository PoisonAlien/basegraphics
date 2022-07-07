
data = read.table(file = "../data/07-areachart.csv", header = TRUE, sep = ",")
par(mar = c(3, 2, 0, 1))
plot(
  NA,
  xlim = range(data$Region.and.country.of.last.residence.1),
  ylim = c(0, max(rowSums(data[,2:ncol(data)]))),
  xlab = NA,
  ylab = NA,
  frame.plot = FALSE,
  axes = FALSE
)

cols = c("#fa8231", "#778ca3", "#0fb9b1", "#a5b1c2", "#a55eea", "#3867d6", 
         "#eb3b5a", "#2bcbba", "#20bf6b", "#45aaf2", "#4b6584", "#d1d8e0", 
         "#fc5c65", "#26de81", "#fd9644", "#8854d0", "#f7b731", "#fed330", 
         "#2d98da", "#4b7bec", "#474787", "#aaa69d")
cols = adjustcolor(col = cols, alpha.f = 0.2)

for(i in 2:20){
  if(i > 2){
    polygon(data$Region.and.country.of.last.residence.1,
            data[,i] + data[,i-1],
            col = cols[i], border = cols[i])
  }else{
    polygon(data$Region.and.country.of.last.residence.1,
            data[,i],
            col = cols[i], border = cols[i])
  }
}