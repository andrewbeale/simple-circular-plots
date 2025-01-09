##Simple circle plots to present circadian phase, and Watson-Williams statistical test to differentiate two conditions. 
##More complicated plots and tests are available! Read Circular Data Analysis from the NCSS manual for more information
##This code has been developed by Andrew Beale in 2024 to enable the simple graphical representation of circadian phase.

##Setup

#Packages - 3 are standard, circular is something you may not have already
if (!require("circular", quietly = TRUE))
  install.packages("circular")
library(circular)
library(ggplot2)
library(dplyr)
library(tidyr)

#Write function to convert time (hours) to angle in degrees for use in the circular ggplot
time_to_degrees <- function(time) {
  (time / 24) * 360  # Convert hours to degrees (0 to 360)
}

##Import data - phase in h, preferably confined to between 0 and 24, where 0 is a set point you define e.g. time of medium addition

#Either enter data manually..
data <- data.frame(
  condition = c(rep("A", 5), rep("B", 5)),
  phase = c(20.16829452,	19.36571658,	20.65550798,	21.00584795,	21.78896104,	8.638333976,	9.866050808,	8.514401623,	9.404041237,	9.678321678)
)  

#or import from a csv which has a similar format table
data <- read.csv("table.csv")

#Calculate means and SEM for each condition
summary_data <- data %>%
  group_by(condition) %>%
  summarise(
    Mean = mean(phase, na.rm = T),
    SEM = sd(phase, na.rm = T) / sqrt(n())
  )

#apply a correction to allow points to be plotted on the circular plot, which has limits of 0 and 24. Apply this correction AFTER calculation condition mean and SEM
data$phase[data$phase > 24] <- data$phase[data$phase > 24] - 24

##The plot

#Define colors for the conditions
colours <- c("A" = "#000000", "B" = "#777777")

##Plot. 
#Here a simple 1D plot is wrapped to form a circle. Points are plotted as well as condition mean and condition SEM on the x-axis.
#The x-axis value is taken from phase in h (converted to phase in degrees by the above function), conditions are differentiated by colour and or shape.
#y is set at an arbitrary radius for visual purposes - trial and error will allow you to size the plot correctly
#coord_polar() wraps the x-axis to a circle - take care to define the start and direction of the wrapping
#NB, if an SEM value takes the band across the 0/24 boundary, you have to plot the SEM segments for either side of the boundary manually and separately. 
#E.g x = time_to_degrees(Mean - SEM), xend = time_to_degrees(24) AND x = time_to_degrees(0), xend = time_to_degrees(SEM - (24 - Mean))

ggplot(data, aes(x = time_to_degrees(phase), y = 0.31, color = condition)) +
  
  #An outer circle is plotted as the "x" axis to sit under points
  geom_segment(data = summary_data, aes(x = time_to_degrees(0), xend = time_to_degrees(24), 
                                        y = 0.31, yend = 0.31), color = "#eeeeee", linewidth = 0.8) +
  
  #Mean is a truncated segment radiating from the center
  geom_segment(data = summary_data, aes(x = time_to_degrees(Mean), xend = time_to_degrees(Mean),
                                        y = 0.25, yend = 0.35, color = condition), linewidth = 0.5) +
  
  #SEM is a shaded thick bar
  geom_segment(data = summary_data, aes(x = time_to_degrees(Mean - SEM), xend = time_to_degrees(Mean + SEM), y = 0.31, yend = 0.31), linewidth = 3, alpha = 0.5) +
  
  #Plot the individual data points
  geom_point(size = 0.5) +
  
  #Color the points, mean lines and SEM shading based on condition
  scale_color_manual(values = colours) +
  
  #Custom polar coordinates (start at top, 0 hours, clockwise)
  coord_polar(start = 0, direction = 1) + 
  
  #Plot x "axis" labels around a central filled circle with spokes
  geom_point(aes(x = 0, y = 0), shape = 16, colour = "black", size = 4.5) +
  
  #Customise x-axis breaks for hours (0, 6, 12, 18) with degree positions
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),  #i.e. 0, 6, 12, 18 hours in degrees
    labels = c("0", "6", "12", "18"),
    limits = c(0, 360)  # Ensures proper wrapping around the full circle
  ) +
  
  #Manually add axis tick marks at 0, 6, 12, 18 hours
  geom_segment(aes(x = 0, xend = 0, y = 0.05, yend = 0.09), color = "black", linewidth = 0.3) +
  geom_segment(aes(x = 90, xend = 90, y = 0.05, yend = 0.09), color = "black", linewidth = 0.3) +
  geom_segment(aes(x = 180, xend = 180, y = 0.05, yend = 0.09), color = "black", linewidth = 0.3) +
  geom_segment(aes(x = 270, xend = 270, y = 0.05, yend = 0.09), color = "black", linewidth = 0.3) +
  
  #Manually place the axis labels using annotate() at the correct angles
  annotate("text", x = 0, y = 0.15, label = "0/24", size = 2.45, color = "black") +
  annotate("text", x = 90, y = 0.15, label = "6", size = 2.45, color = "black") +
  annotate("text", x = 180, y = 0.15, label = "12", size = 2.45,  color = "black") +
  annotate("text", x = 270, y = 0.16, label = "18", size = 2.45, color = "black") +
  
  #Remove the default axis and grid lines
  theme_void() +
  
  #Remove legend
  theme(legend.position = "none")

#Save the plot using ggsave to a location of your choice - adjust width and height iteratively to get the plot you want!
ggsave("phase_plot.pdf", width = 1, height = 1, dpi = 300,)


##Statistical test
#This test is effectively a circular t-test, therefore it only works with two groups
x <- circular(time_to_degrees(data$phase), units="degrees")
data2 <- data %>%
  mutate(x) %>%
  select(!phase)

watson.williams.test(x ~ condition, data = data2)