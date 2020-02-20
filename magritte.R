
rm(list = ls())
library(ggplot2)

# Define a function that takes as inputs the number of sides of the polygon, 
# the color of the lines, and a "Magritte" option.

circle <- function(faces = 50, color = 'steelblue', magritte = FALSE, fontsize = 5) {
  
  # Parameters
  max = faces / 2
  shift <- (max + 1) / (3 * pi)
  
  # Raw plot
  x <- seq(0, max, 1)
  y <- x
  data <- data.frame(x, y)
  plot <- ggplot(data = data)
  
  # Add lines
  for (i in seq(1, (max / 2), 1)) {
    plot <- plot + geom_segment(x = 0, y = (max / 2) + 1 - i + shift, xend = i, yend = 0 + shift, 
                                color = color, alpha = 0.2) #bottom-left
    plot <- plot + geom_segment(x = 0, y = (max / 2) - 1 + i, xend = i, yend = max, 
                                color = color, alpha = 0.2) #top-left
    plot <- plot + geom_segment(x = (max / 2) - 1 + i - shift, y = 0 + shift, xend = max - shift, yend = i + shift, 
                                color = color, alpha = 0.2) #bottom-right
    plot <- plot + geom_segment(x = max - shift, y = (max / 2) - 1 + i , xend = max - i - shift, yend = max, 
                                color = color, alpha = 0.2) #top-right
  }
  
  # Clean layout
  plot <- plot + scale_y_continuous(limits = c(shift, max)) +
    scale_x_continuous(limits = c(0, max - shift)) +
    theme(panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    theme(axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid")) 
  
  # Magritte option
  if (magritte == TRUE) {
    plot <- plot + annotate("text", x = ((max - shift) / 2), y = shift + ((max - shift) / 2), label = "Ceci n'est pas un cercle.", 
           size = fontsize, color = color)
  }
  
  plot
  
}

# A black icosagon:
circle(faces = 20, color = 'black', magritte = FALSE)

# A steelblue pentacontagon:
circle(faces = 50, color = 'steelblue', magritte = FALSE)

# A steelblue hectogon with the Magritte option on:
circle(faces = 100, color = 'steelblue', magritte = TRUE, fontsize = 5)
