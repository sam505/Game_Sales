# install required packages
install.packages("leaps")
install.packages("dplyr")
install.packages("randomForest")
install.packages("gplots")
library(leaps)
library("ggplot2")
library(dplyr)
library(randomForest)
library(gplots)

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

# create platform table and visualize the stats
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
  ggtitle("Barplots of Global Sales per Genre separated by Platforms")+
  geom_bar(stat="identity")+
  labs(x='Genre', y='Global Sales')+
  facet_wrap('Platform')

# Plot Total Number of Sales per Year
Year_sales <- gameData_clean %>% group_by(Year) %>% summarise(
  sum_global_sales = sum(Global_Sales), .groups = 'drop')
ggplot(Year_sales, aes(x = Year, y = sum_global_sales, group = 1)) + 
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="top") + 
  labs(title = "Total Global Sales By Year", y = "Global Sales", x= "Year")

# Plot Publishers stats in relation to global sales
publisherFreq <- as.data.frame(sort(table(gameData_clean$Publisher), increasing = TRUE))
ggplot(subset(publisherFreq, Freq >= 50), aes(x=Var1, y=Freq, fill=Var1))+
  ggtitle("Barplot of Publisher Number of Sales")+
  xlab("Publisher")+
  ylab("Number of Sales")+
  coord_flip()+
  geom_bar(stat="identity")

# Plot platform per Publisher Global sales 
ggplot(
  subset(gameData_clean,
         Publisher %in% c("Electronic Arts", "Activision", "Namco Bandai Games", "Ubisoft", "Konami Digital Entertainment", "Nintendo") &
           Platform %in% c("DS", "PS2", "PS3", "Wii", "X360", "PSP", "PS", "PC", "XB")),
  aes(x=Platform, y=Global_Sales, fill=Platform))+
  ggtitle("Barplots of Publisher's  Number ofGlobal Sales per Genre")+
  geom_bar(stat="identity")+
  labs(x='Genre', y='Global Sales')+
  facet_wrap('Publisher')

# set the correct type of features
gameData_clean$Platform <- as.factor(gameData_clean$Platform)
gameData_clean$Genre <- as.factor(gameData_clean$Genre)
gameData_clean$Year <- as.numeric(gameData_clean$Year)

#Set variable to look at in regression 
selected.var <- c(2, 3, 4, 5, 6, 11)

#Set up Regression Partitions
set.seed(1)

# Split dataset into train and test sets
gameTrain.index <-sample(rownames(gameData_clean), dim(gameData_clean)[1]*0.6) # 60%
gameTrain <- gameData_clean[gameTrain.index,selected.var]

gameValid.index<-setdiff(rownames(gameData_clean),gameTrain.index) # 40%
gameValid <-gameData_clean[gameValid.index, selected.var]

str(gameTrain)
str(gameValid)

# Linear Regression
linearReg <- lm(formula = Global_Sales ~ Genre + Publisher + Year + Platform, data = gameTrain)
summary(linearReg)

# Binary column of sales greater or less than the mean
gameTrainLogit <- gameTrain
salesMean <- mean(gameTrainLogit$Global_Sales)
salesMean
gameTrainLogit$Sales <- ifelse(gameTrainLogit$Global_Sales > salesMean, 1, 0)

gameValidLogit <- gameValid
salesMean <- mean(gameValidLogit$Global_Sales)
salesMean
gameValidLogit$Sales <- ifelse(gameValidLogit$Global_Sales > salesMean, 1, 0)

# Logistic Regression
logisticReg <- glm(formula = Sales ~ Genre + Publisher + Year + Platform, data = gameTrainLogit)
summary(logisticReg)

# K-means Clustering
km <- kmeans(gameTrainLogit, centers = 2)
km

# Random Forest
randForest1 <- randomForest(Sales ~ Genre + Platform + Year, data = gameTrainLogit)
randForest1

# Make predictions using Logistic Regression Model
log_predict <- predict(logisticReg, newdata = gameValidLogit,type = "response")
log_predict <- ifelse(log_predict > 0.5,1,0)

pr <- prediction(logisticReg, gameValidLogit$Sales)perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(gameValidLogit$Survived,log_predict)
