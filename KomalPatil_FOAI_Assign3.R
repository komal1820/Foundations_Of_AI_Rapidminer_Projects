#Topic: Assignment 3
#Name: Komal Patil
#Date: 10/10/2023

###TASK 1
install.packages("tidyverse")
library(tidyverse)

# Load the Cars93 data
Cars93 <- as_tibble(MASS::Cars93)

## a) Create a scatter plot using qplot
qplot(x = EngineSize, y = Price, data = Cars93,main = "Scatterplot between Price and EngineSize")
# a) After conducting the analysis, it is evident that there exists a moderately positive linear relationship
#between Engine Size and Price. The correlation between these variables suggests that, on average, as Engine Size increases, there is a corresponding increase in Price. 

## b)Create a scatter plot using ggplot
ggplot(data = Cars93, aes(x = EngineSize, y = Price)) +
  geom_point()+ labs(title = "Scatterplot between Price and EngineSize")

## c)Scatter plot with color and shape mappings
ggplot(data = Cars93, aes(x = EngineSize, y = Price, color = Type, shape = DriveTrain)) +
  geom_point() + labs(title = "Scatterplot between Price and EngineSize")
  theme_minimal()  # You can add this for a cleaner look
#a)A majority of small-type vehicles do tend to fall into the lower price and engine size category 
 # (between 1 and 2). Even though they are a little more expensive than small-type vehicles, compact-type
 #vehicles also fall into the lower price and engine size range. It's interesting to note that compact-type
 #automobiles have higher price ranges and larger engines than small-type vehicles.
#b)In comparison to other drive train types, a sizable fraction of front-wheel-drive vehicles are seen in the 
#lower price and engine size range.
  
## d) Boxplot showing Price on the y-axis and AirBags on the x-axis
ggplot(data = Cars93, aes(x = AirBags, y = Price)) +
  geom_boxplot() +labs(title = "Boxplot between Price and Airbags")
  theme_minimal()
 #a)Yes, vehicles with driver and passenger airbags cost more on average than those with a driver-only airbag.
#Airbags are available at the lowest median price for non-vehicles.More the number of airbags more is the price.

###TASK 2

# Load the required library
install.packages("maps")
library(maps)

# Reset the graphics device
dev.off()

# Load the required libraries
library(maps)
library(ggplot2)

# Create data frame for map data (US states)
states <- map_data("state")

# Make a copy of the data frame to manipulate
arrests <- USArrests

# Convert everything to lower case
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

# Merge the map data with the arrests data based on region
choro <- merge(states, arrests, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]

# Plot a map, filling in the states based on assault rate
qplot(long, lat, data = choro, group = group, fill = assault, geom = "polygon") +
  labs(title = "Heatmap of Assault Rates")

# Task 03

summer_winter_olympics <- read.csv("summer_winter_olympics.csv")
view(summer_winter_olympics)
str(summer_winter_olympics)
head(summer_winter_olympics)


# do histogram of summer games (total)
# a)
library(ggplot2)
a1 <- ggplot(summer_winter_olympics, aes(x=X..Summer)) + geom_histogram(bins = 15, fill = "skyblue") + labs(title = "Histogram of total summer games", x = "Summer games")+
  theme(plot.title = element_text(size=11))
a1

# b)
# do histogram of winter games (total)
b1 <- ggplot(summer_winter_olympics, aes(x= X..Winter)) + geom_histogram(bins = 15, fill = "skyblue") + labs(title = "Histogram of total winter games", x = "Winter games")+
  theme(plot.title = element_text(size=11))
b1

# c)
#put above two histograms on one page
library(gridExtra) 
grid.arrange(a1,b1, ncol=2)

# d)
# do two histograms on one page: total summer, total winter medals won

library(ggplot2)
plot1 <- ggplot(summer_winter_olympics, aes(x=Total)) + geom_histogram(bins = 15, fill = "red") + labs(title = "Histogram of total summer medals won", x = "Summer")+
  theme(plot.title = element_text(size=11))
plot1

# do histogram of total winter medals won

plot2 <- ggplot(summer_winter_olympics, aes(x= Total.1)) + geom_histogram(bins = 15, fill = "red") + labs(title = "Histogram of total winter medals won", x = "Winter")+
  theme(plot.title = element_text(size=11))
plot2

#put above two histograms on one page
library(gridExtra) 
grid.arrange(plot1,plot2, ncol=2)

# e)

# Create a Scatter plot for summer medals won and winter medals won
ggplot(summer_winter_olympics, aes( x = Total, y = Total.1)) + geom_point() + labs(title = "Scatter plot for summer medals won and winter medals won", x = "Summer", y = "Winter")

# There is a moderate positive linear relationship between summer medals won and winter medals won.


# f) 
# Create a Scatter plot for total summer games and winter games
library(ggplot2)
ggplot(summer_winter_olympics, aes(x=X..Summer, y = X..Winter)) + geom_point() + labs(title = "Scatter plot for total summer games and total winter games", x = "Summer games", y = "Winter games")+
  theme(plot.title = element_text(size=11))

# g) 
# Create histograms for each types of medals, by season 
# summer

library(ggplot2)
G1 <- ggplot(summer_winter_olympics, aes(x=X)) + geom_histogram(bins = 10, fill = "pink") + labs(title = "Summer medals won 1", x = "Summer")+
  theme(plot.title = element_text(size=07))

G2 <- ggplot(summer_winter_olympics, aes(x= X.1)) + geom_histogram(bins = 10, fill = "pink") + labs(title = "Summer medals won 2", x = "Summer")+
  theme(plot.title = element_text(size=07))

G3 <- ggplot(summer_winter_olympics, aes(x= X.2)) + geom_histogram(bins = 10, fill = "pink") + labs(title = "Summer medals won 3", x = "Summer")+
  theme(plot.title = element_text(size=07))

# Winter

G4 <- ggplot(summer_winter_olympics, aes(x= X.3)) + geom_histogram(bins = 10, fill = "darkblue") + labs(title = "Winter medals won 1", x = "Winter")+
  theme(plot.title = element_text(size=07))

G5 <- ggplot(summer_winter_olympics, aes(x= X.4)) + geom_histogram(bins = 10, fill = "darkblue") + labs(title = "Winter medals won 2", x = "Winter")+
  theme(plot.title = element_text(size=07))

G6 <- ggplot(summer_winter_olympics, aes(x= X.5)) + geom_histogram(bins = 10, fill = "darkblue") + labs(title = "Winter medals won 3", x = "Winter")+
  theme(plot.title = element_text(size=07))


#put above two histograms on one page
library(gridExtra) 
grid.arrange(G1,G2,G3,G4,G5,G6 ,ncol=3)

# h) 
# summer

library(ggplot2)
H1 <- ggplot(summer_winter_olympics, aes(x=X)) + geom_histogram(bins = 20, fill = "yellow") + labs(title = "Summer medals won 1", x = "Summer")+
  theme(plot.title = element_text(size=07))

H2 <- ggplot(summer_winter_olympics, aes(x= X.1)) + geom_histogram(bins = 20, fill = "yellow") + labs(title = "Summer medals won 2", x = "Summer")+
  theme(plot.title = element_text(size=07))

H3 <- ggplot(summer_winter_olympics, aes(x= X.2)) + geom_histogram(bins = 20, fill = "yellow") + labs(title = "Summer medals won 3", x = "Summer")+
  theme(plot.title = element_text(size=07))

# Winter

H4 <- ggplot(summer_winter_olympics, aes(x= X.3)) + geom_histogram(bins = 20, fill = "blue") + labs(title = "Winter medals won 1", x = "Winter")+
  theme(plot.title = element_text(size=07))

H5 <- ggplot(summer_winter_olympics, aes(x= X.4)) + geom_histogram(bins = 20, fill = "blue") + labs(title = "Winter medals won 2", x = "Winter")+
  theme(plot.title = element_text(size=07))

H6 <- ggplot(summer_winter_olympics, aes(x= X.5)) + geom_histogram(bins = 20, fill = "blue") + labs(title = "Winter medals won 3", x = "Winter")+
  theme(plot.title = element_text(size=07))


#put above two histograms on one page
library(gridExtra) 
grid.arrange(H1,H2,H3,H4,H5,H6 ,ncol=3)

# i)
# histogram for total games for both summer and winter
ggplot(summer_winter_olympics, aes(x= X..Games)) + geom_histogram(bins = 20, fill = "green") + labs(title = "Games for both summer and winter", x = "Games")


library(ggplot2)
# i)b)
# Assuming you have a data frame named summer_winter_olympics
# Filter the data to include only rows where Combined.total is greater than 300
filtered_data <- subset(summer_winter_olympics, Combined.total > 300)

# Create a bar plot for the filtered data
ggplot(filtered_data, aes(x = Team..IOC.code., y = Combined.total)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Team (IOC Code)", y = "Combined Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Filter the data to include only rows where Combined.total is less than 300
filtered_data <- subset(summer_winter_olympics, Combined.total < 300)

# Create a bar plot for the filtered data
ggplot(filtered_data, aes(x = Team..IOC.code., y = Combined.total)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Team (IOC Code)", y = "Combined Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
#From the above observation we can see that most of the countries have earned the medals less than 300.


