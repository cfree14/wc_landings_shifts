

my_theme <- theme(
  axis.text=element_text(size=8),
  axis.title=element_text(size=10),
  legend.text=element_text(size=8),
  legend.title=element_text(size=10),
  plot.title=element_text(size=11),
  # Gridlines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"))
