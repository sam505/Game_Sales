# install required packages
install.packages("leaps")
library(leaps)
library("ggplot2")

# Load dataset
gameData_df <- read.csv("vgsales.csv")

# statistical summary of dataset columns
summary(gameData_df)

# NAs change from string 
gameData_na <- gameData_df
gameData_na[gameData_na=="N/A"] = NA

# calculate total missing values
miss <- function(x){sum(is.na(x))}
apply(gameData_na, 2, miss)

# remove missing values
gameData_clean <- na.omit(gameData_na)
apply(gameData_clean, 2, miss)

# set the correct type of features
gameData_clean$Platform <- as.factor(gameData_clean$Platform)
gameData_clean$Genre <- as.factor(gameData_clean$Genre)
gameData_clean$Year <- as.numeric(gameData_clean$Year)

# create platform table and visuaize the stats
plataformFreq <- as.data.frame(sort(table(gameData_clean$Platform), decreasing = TRUE))
ggplot(plataformFreq, aes(x=Var1, y=Freq, fill=Var1))+
  ggtitle("Barplot of Total Number of Games per Platforms")+
  xlab("Platform")+
  ylab("Count")+
  geom_bar(stat="identity")

# create genre table and visualize genre column
genreFreq <- as.data.frame(sort(table(gameData_clean$Genre), decreasing = TRUE))
ggplot(genreFreq, aes(x=Var1, y=Freq, fill=Var1))+
  ggtitle("Barplot of Total Number of Games per Genre")+
  xlab("Genre")+
  ylab("Count")+
  geom_bar(stat="identity")

# Plot leading sales
ggplot(
  subset(gameData_clean, Platform %in% c('GBA', 'PC', 'Wii', 'X360', 'PS2', 'PS3') &
           Genre %in% c('Action', 'Sports', 'Misc', 'Role-playing', 'Shooter', 'Adventure')),
  aes(x=Genre, y=Global_Sales, fill=Genre))+
  ggtitle("Multiple barplot for global sales per year separated by platforms")+
  geom_bar(stat="identity")+
  labs(x='Genre', y='Global Sales')+
  facet_wrap('Platform')

