data = read.delim(file = "data/15-pieanimate_browserWars_compileData.tsv", header = TRUE)
data$Total = round(data$Total, 2)
data$browser = gsub(pattern = "Other", replacement = "others", x = data$browser)

#Order by months
data = lapply(split(data, data$year), function(x){
  x[order(factor(x$month, levels = month.name)),]
})
data = do.call(what = rbind, data)
data$month = as.character(data$month)

browsercols = c("#16a085", "#2980b9", "#f39c12", "#8e44ad", "#e74c3c", "gray") #hcl.colors(n = 6, palette = "Dark2")
names(browsercols) = c("chrome", "explorer", "firefox", "opera", "safari", "others")

#pie() with an added argument labcol
pie2 = function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE,
                 init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45,
                 col = NULL, border = NULL, lty = NULL, main = NULL, labcol = "black", ...)
{
  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive.")
  if (is.null(labels))
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L])
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col))
    col <- if (is.null(density))
      c("white", "lightblue", "mistyrose", "lightcyan",
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col))
    col <- rep_len(col, nx)
  if (!is.null(border))
    border <- rep_len(border, nx)
  if (!is.null(lty))
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    density <- rep_len(density, nx)
  twopi <- if (clockwise)
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i],
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE,
           adj = ifelse(P$x < 0, 1, 0), col = labcol[i], ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}

dataspl = split(data, data$date_obs)

pngs = lapply(dataspl, function(x){
  fname = tempfile(fileext = '.png')
  png(filename = fname, res = 110)
  par(mar = c(2, 4, 3, 4),  family = "mono") #bg = "#ecf0f1"
  pie2(
    x = x$Total,
    labcol = rep("#2c3e50", nrow(x)), #browsercols[x$browser],
    col = browsercols[x$browser],
    radius = 1,
    labels = paste0(x$browser, "\n[", x$Total, "%]"),
    xpd = TRUE,
    outer = TRUE, cex = 1, border = browsercols[x$browser], font = 2
  )
  points(x = 0, y = 0, pch = 19, cex = 18, col = "white", lwd = 0)
  text(x = 0, y = -.1, label = x$year[1], adj = 0.5, cex = 2, col = "#c0392b", font = 2)
  text(x = 0, y = .1, label = x$month[1], adj = 0.5, cex = 1.2, col = "#e74c3c", font = 2)

  title(main = "WEB BROWSERS MARKET SHARE\n2002-2022", adj = 0.5, cex.main = 1.5, line = 0, col.main = "#2c3e50")
  dev.off()
  fname
})

pngs = unlist(pngs, use.names = FALSE)
cmd = paste0("convert -loop 0 -delay 15 ", paste(pngs, collapse = ' '), " pianimate.gif")
sys.log = system(command = cmd, intern = TRUE)
