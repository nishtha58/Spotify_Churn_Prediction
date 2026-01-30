## Data Preparation

# Data Cleaning 
install.packages(c("gridExtra", "dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(dplyr)

#Dataset available in repository
data<- read.csv("/Users/nisht/Documents/Nishtha/Documents/Duke/Fall 1/Data Science/Project/spotify_churn_dataset.csv", header=TRUE)
source("/Users/nisht/Documents/Nishtha/Documents/Duke/Fall 1/Data Science/DataAnalyticsFunctions.R")
installpkg("plotrix")
library(plotrix)

data <- read.csv("final_data.csv")

summary(data)
str(data)
#none of the variables are constant for all users hence relevant for our analysis
#user id is just a unique identifier and shouldnt be used for modelling 

#NULL VALUE CHECK
# Count of missing values per column
colSums(is.na(data))
#Since no column has any Nulls we do not have to tackle any issue


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


# Visualization  exploration and summary statistics of data:

spotify <- read.csv("final_data.csv")

overall_churn_rate <- spotify %>% 
  summarize(churn_rate = mean(is_churned))

churn_by_sub_type <- spotify %>% 
  group_by(subscription_type) %>% 
  summarize(churn_rate = mean(is_churned)) %>% 
  arrange(desc(churn_rate))

churn_by_dev_type <- spotify %>% 
  group_by(device_type) %>% 
  summarize(churn_rate = mean(is_churned)) %>% 
  arrange(desc(churn_rate))

summary_stats <- summary(spotify[, c("listening_time", "songs_played_per_day", "skip_rate")])

ggplot(spotify, aes(x = factor(is_churned), y = listening_time)) + 
  geom_boxplot(fill = "darkgreen") +
  labs(x = "Churned", y = "Listening Time (hrs/week)", 
       title = "Listening Time by Churn Status") +
  scale_x_discrete(labels = c("0" = "Stayed", "1" = "Churned"))


ggplot(spotify, aes(x = country, fill = factor(is_churned))) +
  geom_bar(position = "fill") +
  labs(x = "Country", y = "Churn Rate", fill = "Churned",
       title = "Churn Proportions by Country") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("0" = "darkgreen", "1" = "black"),
                    labels = c("Stayed", "Churned"))


spotify %>%
  mutate(age_group = cut(age, breaks = seq(10, 70, 10), include.lowest = TRUE)) %>%
  group_by(age_group) %>%
  summarise(churn_rate = mean(is_churned)) %>%
  ggplot(aes(x = age_group, y = churn_rate)) +
  geom_col(fill = "darkgreen") +
  labs(x = "Age Group", y = "Churn Rate",
       title = "Churn Rate by Age Group") +
  scale_y_continuous(labels = scales::percent)


## Clustering 
install.packages(c("gridExtra", "dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(dplyr)
installpkg("class")
library(class)
library(ggplot2)
install.packages("fmsb")
library(fmsb)

data<- read.csv("/Users/nisht/Documents/Nishtha/Documents/Duke/Fall 1/Data Science/Project/final_data.csv", header=TRUE)
source("/Users/nisht/Documents/Nishtha/Documents/Duke/Fall 1/Data Science/DataAnalyticsFunctions.R")
installpkg("plotrix")
library(plotrix)

summary(data)
#Clustering 
#removing user_id and target variable 
spotify_numeric <- data[, sapply(data, is.numeric)]
spotify_numeric <- subset(spotify_numeric, select = -c(user_id, is_churned))
x <- as.matrix(spotify_numeric)
x.scaled <- scale(x)

#elbow method to pick Optimal k for clustering 
wss <- sapply(1:10, function(k) {
  kmeans(x.scaled, centers = k, nstart = 25)$tot.withinss
})
plot(1:10, wss, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster sum of squares",
     main = "Elbow Method for Optimal k")

# Running k-means 
set.seed(123)
n.clusters <- kmeans(x.scaled, centers = 2, nstart = 25)

# Attach cluster results to your data
data$cluster <- n.clusters$cluster

# Summarize mean of numeric columns by cluster
cluster_means <- aggregate(. ~ cluster, data = data[, c(names(spotify_numeric), "cluster")], FUN = mean)

# Count how many songs per cluster
cluster_counts <- aggregate(rep(1, nrow(data)) ~ n.clusters$cluster, FUN = sum)
names(cluster_counts) <- c("cluster", "count")

list(
  cluster_means = cluster_means,
  cluster_counts = cluster_counts
)

#cluster size
ggplot(data, aes(x = factor(cluster), fill = factor(cluster))) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Cluster Sizes (User Distribution)",
    x = "Cluster",
    y = "Number of Users"
  ) +scale_fill_brewer(palette = "Paired")+
  theme_minimal()

#Radar plot for cluster summary
cluster_means <- aggregate(. ~ cluster, data = data[, c(names(spotify_numeric), "cluster")], FUN = mean)
rownames(cluster_means) <- paste0("Cluster_", cluster_means$cluster)
cluster_means$cluster <- NULL
maxs <- apply(cluster_means, 2, max)
mins <- apply(cluster_means, 2, min)
radar_df <- rbind(maxs, mins, cluster_means)

radarchart(radar_df, axistype = 1, title = "Cluster Behavioral Profiles")
legend("topright", legend = rownames(cluster_means), bty = "n")

## Listening Time v/s Skip Rate by Cluster
ggplot(data, aes(x = listening_time, y = skip_rate, color = factor(cluster))) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Listening Time vs Skip Rate by Cluster",
    x = "Listening Time (minutes per day)",
    y = "Skip Rate"
  ) +
  scale_color_brewer(palette = "Spectral") + 
  theme_minimal()
##cluster v/s subscription type
ggplot(data, aes(x = factor(cluster), fill = subscription_type)) +
  geom_bar(position = "fill") +
  labs(
    title = "Subscription Composition per Cluster",
    x = "Cluster",
    y = "Proportion of Users",
    fill = "Subscription Type"
  ) +
  scale_fill_brewer(palette = "Pastel1") + 
  theme_minimal()

#Break-up By countries
top_countries <- names(sort(table(data$country), decreasing = TRUE))[1:10]

ggplot(data %>%
         filter(country %in% top_countries) %>%
         group_by(country, cluster) %>%
         summarise(n = n(), .groups = "drop"),
       aes(x = reorder(country, n), y = n, fill = factor(cluster))) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Top 10 Countries by Cluster Composition",
    x = "Country",
    y = "Number of Users",
    fill = "Cluster"
  )  + 
  theme_minimal()


#Churn Rate v/s Clusuter
churn_summary <- data %>%
  group_by(cluster) %>%
  summarise(churn_rate = mean(is_churned), n = n())

ggplot(churn_summary, aes(x = factor(cluster), y = churn_rate, fill = factor(cluster))) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Churn Rate by Cluster",
    x = "Cluster",
    y = "Proportion of Users Churned"
  ) +
  theme_minimal()

#Listening Time v/s Age by Cluster
ggplot(data, aes(x = age, y = listening_time, color = factor(cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Listening Time vs Age by Cluster", x = "Age", y = "Listening Time (minutes)")


#Cluster 1 Analysis ( these are our student and free users , they churn more )
cluster2 <- subset(data, cluster == 2)
cluster1 <- subset(data, cluster == 1)
library(ggplot2)

ggplot(cluster1, aes(x = listening_time, y = skip_rate, color = factor(is_churned))) +
  geom_point(alpha = 0.6, size = 1.8) +
  scale_color_manual(values = c("steelblue", "tomato"), labels = c("Active", "Churned"), name = "Churn Status") +
  labs(
    title = "Cluster 1: Skip Rate vs Listening Time",
    x = "Listening Time (minutes per day)",
    y = "Skip Rate"
  ) +
  theme_minimal()

#poor recommendation fit or low playlist relevance
#Churn driven by content dissatisfaction and declining engagement. These users skip frequently, and leave before forming a habit.

cluster1$is_churned_f <- factor(cluster1$is_churned, levels = c(0,1), labels = c("Active","Churned"))

ggplot(cluster1, aes(x = user_id, y = skip_rate, color = is_churned_f)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_brewer(palette = "Set1", name = "Churn Status") +
  labs(
    title = "Cluster 1 – Skip Rate per User by Churn Status",
    x = "User ID",
    y = "Skip Rate"
  ) +
  theme_minimal()


#Cluster 2 Analysis ( these are premium and family users, they pay more and churn less) 

ggplot(cluster2, aes(x = listening_time, y = skip_rate, color = factor(is_churned))) +
  geom_point(alpha = 0.6, size = 1.8) +
  scale_color_manual(values = c("steelblue", "tomato"), labels = c("Active", "Churned"), name = "Churn Status") +
  labs(
    title = "Cluster 2: Skip Rate vs Listening Time",
    x = "Listening Time (minutes per day)",
    y = "Skip Rate"
  ) +
  theme_minimal()

cluster2$is_churned_f <- factor(cluster2$is_churned, levels = c(0,1), labels = c("Active","Churned"))

ggplot(cluster2, aes(x = user_id, y = age, color = is_churned_f)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_brewer(palette = "Set1", name = "Churn Status") +
  labs(
    title = "Cluster 2 – Skip Rate per User by Churn Status",
    x = "User ID",
    y = "Skip Rate"
  ) +
  theme_minimal()
#This tell us users in cluster 2 also do skip more and churn more when they listen less but not as much of a problem as in CLuster 1

device_summary <- cluster2 %>%
  group_by(device_type) %>%
  summarise(
    n = n(),
    churn_rate = mean(is_churned)
  ) %>%
  arrange(desc(churn_rate))

print(device_summary)


ggplot(device_summary, aes(x = reorder(device_type, -churn_rate), y = churn_rate, fill = device_type)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  geom_text(aes(label = scales::percent(churn_rate, accuracy = 0.1)), vjust = -0.4) +
  labs(
    title = "Cluster 2 – Churn Rate by Device Type",
    x = "Device Type",
    y = "Churn Rate"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

country_summary <- cluster2 %>%
  group_by(country) %>%
  summarise(
    n = n(),
    churn_rate = mean(is_churned)
  ) %>%
  arrange(desc(churn_rate))

print(head(country_summary, 10))

top_countries <- country_summary %>%
  top_n(10, n)

ggplot(top_countries, aes(x = reorder(country, churn_rate), y = churn_rate, fill = country)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  geom_text(aes(label = scales::percent(churn_rate, accuracy = 0.1)), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Cluster 2 – Churn Rate by Country (Top 10)",
    x = "Country",
    y = "Churn Rate"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  theme_minimal()



device_country_summary <- cluster2 %>%
  group_by(country, device_type) %>%
  summarise(
    n = n(),
    churn_rate = mean(is_churned)
  ) %>%
  arrange(desc(churn_rate))

# Example heatmap
ggplot(device_country_summary, aes(x = device_type, y = country, fill = churn_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "skyblue", high = "tomato", name = "Churn Rate") +
  labs(
    title = "Cluster 2 – Churn Rate by Device and Country",
    x = "Device Type", y = "Country"
  ) +
  theme_minimal()



## Modeling 
### Spotify Churn Prediction – Unified Modeling Script
### Includes: Null Baseline, RMSE, AUC, LogLoss, Accuracy
### 10-Fold Cross-Validation Framework

suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(caret)
  library(tree)
  library(randomForest)
  library(nnet)
  library(pROC)
  library(recipes)
  library(themis)
  library(glmnet)
  library(ggplot2)
  library(reshape2)
})
if (file.exists("DataAnalyticsFunctions.R")) source("DataAnalyticsFunctions.R")
set.seed(42)

spotify <- read.csv("/Users/my_mac/Desktop/Duke_DataBus/project_2/final_data.csv")
cat("Initial data shape:", dim(spotify), "\n")
cat("Churn rate:", round(mean(spotify$is_churned == 1), 3), "\n")


if ("user_id" %in% names(spotify)) spotify <- spotify %>% select(-user_id)

# Impute missing numeric values
impute_mean <- function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)
num_cols <- sapply(spotify, is.numeric)
spotify[, num_cols] <- lapply(spotify[, num_cols], impute_mean)

# Convert target to numeric
spotify$is_churned <- as.numeric(spotify$is_churned)

# Train/test split
train_idx <- createDataPartition(spotify$is_churned, p = 0.8, list = FALSE)
train_data <- spotify[train_idx, ]
test_data  <- spotify[-train_idx, ]

# Cast character predictors to factor
cat_cols <- names(train_data)[sapply(train_data, is.character)]
if (length(cat_cols) > 0) {
  train_data[, cat_cols] <- lapply(train_data[, cat_cols], as.factor)
  test_data[, cat_cols]  <- lapply(test_data[, cat_cols], as.factor)
}

train_data$is_churned <- as.factor(train_data$is_churned)
rec <- recipe(is_churned ~ ., data = train_data) %>%
  step_smotenc(is_churned, over_ratio = 1)
rec_p <- prep(rec, training = train_data, retain = TRUE)
train_bal <- bake(rec_p, new_data = train_data)
cat("Balanced class counts:\n")
print(table(train_bal$is_churned))

evaluate_model <- function(actual, prob_pred, model_name = "Model") {
  # ensure actual is 0/1 numeric
  if (is.factor(actual)) {
    actual_num <- as.numeric(as.character(actual))
  } else {
    actual_num <- actual
  }
  eps <- 1e-15
  prob <- pmin(pmax(prob_pred, eps), 1 - eps)
  auc_val <- as.numeric(roc(actual_num, prob)$auc)
  logloss_val <- -mean(actual_num * log(prob) + (1 - actual_num) * log(1 - prob))
  rmse_val <- sqrt(mean((actual_num - prob)^2))
  pred_label <- ifelse(prob > 0.5, 1, 0)
  acc_val <- mean(pred_label == actual_num)
  data.frame(Model = model_name,
             AUC = auc_val,
             LogLoss = logloss_val,
             RMSE = rmse_val,
             Accuracy = acc_val)
}

k <- 10
data_cv <- train_bal
n <- nrow(data_cv)
foldid <- sample(rep(1:k, length.out = n))
cv_results <- data.frame()
cat("\nStarting", k, "fold cross-validation...\n\n")
for (i in 1:k) {
  cat("Fold", i, "of", k, "...\n")
  train_fold <- data_cv[foldid != i, ]
  test_fold  <- data_cv[foldid == i, ]
  # --- :one: Null Baseline ---
  mean_churn <- mean(as.numeric(as.character(train_fold$is_churned)))
  null_pred <- rep(mean_churn, nrow(test_fold))
  res_null <- evaluate_model(as.numeric(as.character(test_fold$is_churned)),
                             null_pred, "Null")
  # --- :two: Logistic Regression ---
  lr_mod <- glm(is_churned ~ ., data = train_fold, family = "binomial")
  lr_prob <- predict(lr_mod, newdata = test_fold, type = "response")
  res_lr <- evaluate_model(test_fold$is_churned, lr_prob, "Logistic")
  # --- :three: Logistic Regression + Interactions ---
  lri_mod <- glm(is_churned ~ .^2, data = train_fold, family = "binomial")
  lri_prob <- predict(lri_mod, newdata = test_fold, type = "response")
  res_lri <- evaluate_model(test_fold$is_churned, lri_prob, "Logistic+Int")
  # --- :four: Classification Tree ---
  tree_mod <- tree(is_churned ~ ., data = train_fold)
  tree_prob <- predict(tree_mod, newdata = test_fold, type = "vector")[,2]
  res_tree <- evaluate_model(test_fold$is_churned, tree_prob, "Tree")
  # --- :five: Random Forest ---
  rf_mod <- randomForest(is_churned ~ ., data = train_fold,
                         ntree = 200, mtry = floor(sqrt(ncol(train_fold)-1)))
  rf_prob <- predict(rf_mod, newdata = test_fold, type = "prob")[,2]
  res_rf <- evaluate_model(test_fold$is_churned, rf_prob, "RandomForest")
  # --- :six: Neural Network ---
  dummy_mod <- dummyVars(is_churned ~ ., data = train_fold)
  train_x <- scale(predict(dummy_mod, newdata = train_fold))
  test_x  <- scale(predict(dummy_mod, newdata = test_fold))
  train_y <- as.numeric(as.character(train_fold$is_churned))
  test_y  <- as.numeric(as.character(test_fold$is_churned))
  nn_mod <- nnet(x = train_x, y = train_y, size = 8, decay = 0.01,
                 maxit = 800, trace = FALSE)
  nn_prob <- predict(nn_mod, test_x, type = "raw")
  res_nn <- evaluate_model(test_y, nn_prob, "NeuralNet")
  # --- :seven: LASSO ---
  X_train <- model.matrix(is_churned ~ ., data = train_fold)[, -1]
  y_train <- as.numeric(as.character(train_fold$is_churned))
  X_test  <- model.matrix(is_churned ~ ., data = test_fold)[, -1]
  lasso_cv <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)
  lasso_prob <- predict(lasso_cv, newx = X_test, s = "lambda.min", type = "response")
  res_lasso <- evaluate_model(test_fold$is_churned, lasso_prob, "LASSO")
  # Combine results from this fold
  cv_results <- rbind(cv_results,
                      rbind(res_null, res_lr, res_lri, res_tree, res_rf, res_nn, res_lasso))
}
cat("\n:white_check_mark: Cross-validation complete.\n")


cv_summary <- cv_results %>%
  group_by(Model) %>%
  summarise(
    Mean_AUC = mean(AUC, na.rm = TRUE),
    Mean_LogLoss = mean(LogLoss, na.rm = TRUE),
    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_AUC))
cat("\nAverage Performance Across 10 Folds:\n")
print(cv_summary)


theme_cv <- theme_minimal(base_size = 12) + theme(legend.position = "none")
ggplot(cv_results, aes(x = Model, y = AUC, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  theme_cv +
  labs(title = "10-Fold Cross-Validation: AUC by Model",
       x = NULL, y = "AUC")
ggplot(cv_results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  theme_cv +
  labs(title = "10-Fold Cross-Validation: RMSE by Model",
       x = NULL, y = "RMSE")
ggplot(cv_results, aes(x = Model, y = LogLoss, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  theme_cv +
  labs(title = "10-Fold Cross-Validation: LogLoss by Model",
       x = NULL, y = "Log-Loss")
ggplot(cv_results, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  theme_cv +
  labs(title = "10-Fold Cross-Validation: Accuracy by Model",
       x = NULL, y = "Accuracy")


X_full <- model.matrix(is_churned ~ ., data = train_bal)[, -1]
y_full <- as.numeric(as.character(train_bal$is_churned))
lasso_full <- cv.glmnet(X_full, y_full, family = "binomial", alpha = 1)
coef_lasso <- coef(lasso_full, s = "lambda.min")

# Convert coefficients to a tidy data frame
lasso_vars <- data.frame(
  Variable = rownames(coef_lasso),
  Coefficient = as.vector(coef_lasso)
)

# Keep only non-zero coefficients and sort by absolute importance
important_vars <- lasso_vars %>%
  filter(Coefficient != 0) %>%
  arrange(desc(abs(Coefficient)))
cat("\nTop 15 Most Important Variables from LASSO:\n")
print(head(important_vars, 15))
# Visualize top coefficients
ggplot(important_vars[1:min(15, nrow(important_vars)), ],
       aes(x = reorder(Variable, abs(Coefficient)), y = abs(Coefficient))) +
  geom_col(fill = "#2E86C1") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(title = "Top 15 Most Important Variables (LASSO Logistic Regression)",
       x = "Variable", y = "Absolute Coefficient")

