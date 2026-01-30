install.packages(c("gridExtra", "dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(dplyr)

data<- read.csv("/Users/nisht/Documents/Nishtha/Documents/Duke/Fall 1/Data Science/Project/final_data.csv", header=TRUE)
source("/Users/nisht/Documents/Nishtha/Documents/Duke/Fall 1/Data Science/DataAnalyticsFunctions.R")
installpkg("plotrix")
library(plotrix)

summary(data)
str(data)
#none of the variables are constant for all users hence relevant for our analysis
#user id is just a unique identifier and shouldnt be used for modelling 

#NULL VALUE CHECK
# Count of missing values per column
colSums(is.na(data))
#Since no column has any Nulls we do not have to tackle any issue

#histograms
country_counts <- table(data$country)
# Country
barp(country_counts,
     main = "Distribution of Country",
     xlab = "Country",
     ylab = "Count",
     col = "lightblue",
     names.arg = names(country_counts))

# Gender
gender_counts <- table(data$gender)
barp(gender_counts,
     main = "Distribution of Gender",
     xlab = "Gender",
     ylab = "Count",
     col = "lightblue",
     names.arg = names(gender_counts))

# Subscription Type
subs_counts <- table(data$subscription_type)
barp(subs_counts,
     main = "Distribution of Subscription Type",
     xlab = "Gender",
     ylab = "Count",
     col = "lightblue",
     names.arg = names(subs_counts))
# Boxplots for numeric features
p1 <- ggplot(data, aes(y = listening_time)) + geom_boxplot(fill = "lightblue") + ggtitle("Listening Time")
p2 <- ggplot(data, aes(y = songs_played_per_day)) + geom_boxplot(fill = "lightgreen") + ggtitle("Songs/Day")
p3 <- ggplot(data, aes(y = skip_rate)) + geom_boxplot(fill = "lightpink") + ggtitle("Skip Rate")

# Arrange in one row
grid.arrange(p1, p2, p3, ncol = 3)

p4 <- ggplot(data, aes(y = age)) + geom_boxplot(fill = "lightblue") + ggtitle("age")
p5 <- ggplot(data, aes(y = ads_listened_per_week)) + geom_boxplot(fill = "lightgreen") + ggtitle("Ads/Week")
p6<-ggplot(data, aes(y = offline_listening)) + geom_boxplot(fill = "lightpink") + ggtitle("Offline Listening")
# Arrange in one row
grid.arrange(p4, p5,p6, ncol = 3)

#Most users listen to relatively few ads per week (small IQR near the bottom).
#But many users are flagged as outliers with very high ads/week (10–50).
#This long tail suggests heavy skewness → most users hear a handful of ads, but a small group hears a lot.
hist(data$ads_listened_per_week,
     breaks = 5,
     col = "skyblue",
     main = "Histogram of Ads Listened Per Week",
     xlab = "Ads per Week",
     ylab = "Number of Users")
ggplot(data, aes(x = subscription_type, y = ads_listened_per_week, fill = subscription_type)) +
  geom_boxplot() +
  labs(title = "Ads per Week by Subscription Type",
       x = "Subscription Type", y = "Ads per Week") +
  theme_minimal()

#Extra checks
# Age should be reasonable (10–90)
data %>% filter(age < 10 | age > 90)



# Skip rate must be between 0 and 1
data %>% filter(skip_rate < 0 | skip_rate > 1)

# Listening time, songs per day, ads per week, offline listening must be >= 0
data %>% filter(listening_time < 0 |
                  songs_played_per_day < 0 |
                  ads_listened_per_week < 0 |
                  offline_listening < 0)


#Offline Listening by Subs Type
ggplot(data, aes(x = subscription_type, fill = factor(offline_listening))) +
  geom_bar(position = "dodge") +
  labs(title = "Offline Listening by Subscription Type",
       x = "Subscription Type",
       y = "Number of Users",
       fill = "Offline Listening\n(0 = No, 1 = Yes)") +
  theme_minimal()


