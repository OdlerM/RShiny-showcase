create_plot <- function(scale_max, attr_vals, attr_labels) {
  plot.new()
  plot.window(xlim = 1.5 * c(-scale_max, scale_max), 
              ylim = 1.5 * c(-scale_max, scale_max), asp=1)
  par(mar=c(0,0,0,0))
  for (step in 1:scale_max) draw.circle(0, 0, step, border = "lightgray")
  points(0, 0, cex=0.6, col="black", pch=19)
  attr_num = length(attr_vals)
  thetas = seq(0, 2*pi, len=attr_num+1)
  attr_vals[length(attr_vals) + 1] = attr_vals[1]
  polygon(attr_vals * cos(thetas), attr_vals * sin(thetas), 
          col=rgb(0.68,0.85,0.9, 0.3), border="lightblue", lwd=2)
  polygon(scale_max * cos(thetas), scale_max * sin(thetas), 
          border="black", lwd=1)
  
  for (ind in 1:attr_num) {
    points(attr_vals[ind] * cos(thetas[ind]),
           attr_vals[ind] * sin(thetas[ind]), col="lightblue", pch=19)
    text(1.3 * scale_max * cos(thetas[ind]),
         1.3 * scale_max * sin(thetas[ind]),
         attr_labels[ind], adj=0.5, cex = 2)
         
  } 
}

attr_vals <- c(1, 2, 3, 5, 6)
attr_labels <- c("Ahoj", "Ja", "Som", "Bohus", "Brav")
create_plot(8, attr_vals, attr_labels)

zabi_sa <- function(vect) {
  plot(vect)
  points(1, 1, col="red", pch=19)
}

vect <- 1:7
zabi_sa(vect)
