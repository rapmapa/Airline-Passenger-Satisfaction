library(dplyr)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(markdown)
library(readr)
library(magrittr)
library(fastDummies)
library(caret)
library(caTools)
library(ROCR)
library(scales)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(randomForest)
library(tidyr)
library(ggpubr)
library(cluster)
library(factoextra)
library(cowplot)
library(gridExtra)
library(reshape2)
library(fmsb)
library(missMDA)

df = read.csv("airline_passenger_satisfaction.csv",header = T)
df = data.frame(df)

is.na(df)

head(df,3)

summary = as.data.frame(skim(df))
summary

# we know that less than 30% is maximum missing values that are allowed to drop
# since the arrival delay column contains 0.3% null values, we can drop them

df = df %>% na.omit()

colors <- c("red", "blue", "green", "orange", "purple", "brown", "pink", "gray", "olivedrab", "cyan", "magenta", "yellow", "darkred", "darkblue", "darkgreen", "coral", "violet", "beige", "lavender", "turquoise", "lightpink", "lightgray", "olivedrab1")

df$Satisfaction <- ifelse(df$Satisfaction == "Neutral or Dissatisfied", 0, 1)

# Satisfaction by Gender
ggplot(df, aes(x=Gender, fill=Satisfaction)) + 
  geom_bar(position="dodge") +
  labs(x="Gender", y="Count", fill="Satisfaction") +
  ggtitle("Satisfaction by Gender")

# Satisfaction by Customer Type
ggplot(df, aes(x=Customer.Type, fill=Satisfaction)) + 
  geom_bar(position="dodge") +
  labs(x="Gender", y="Count", fill="Satisfaction") +
  ggtitle("Satisfaction by Customer Type")

# create a stacked bar chart
ggplot(df, aes(x=Gender, fill=factor(Satisfaction))) +
  geom_bar()

# Fit decision tree
tree_model <- rpart(Satisfaction ~ Departure.and.Arrival.Time.Convenience + Ease.of.Online.Booking + 
                      Check.in.Service + Online.Boarding + Gate.Location + 
                      On.board.Service + Seat.Comfort + Leg.Room.Service + 
                      Cleanliness + Food.and.Drink + In.flight.Service + 
                      In.flight.Wifi.Service + In.flight.Entertainment + 
                      Baggage.Handling, data = df)

# Print decision tree
print(tree_model)

rpart.plot(tree_model)

prp(tree_model)

# Extract variable importance values
importance <- varImp(tree_model)

# Create a bar chart to rank columns based on importance
ggplot(importance, aes(x=Overall, y=reorder(row.names(importance), Overall))) +
  geom_bar(stat="identity") +
  ylab("Variable") + xlab("Importance") +
  ggtitle("Variable Importance")

# Create a binary outcome variable (1 for satisfied, 0 for dissatisfied/neutral)
df$Satisfaction_bin <- ifelse(df$Satisfaction == "Satisfied", 1, 0)

# Fit a logistic regression model
model <- glm(Satisfaction_bin ~ Gender + Age + Customer.Type + Type.of.Travel + Class + 
               Flight.Distance + Departure.Delay + Arrival.Delay + 
               Departure.and.Arrival.Time.Convenience + Ease.of.Online.Booking + 
               Check.in.Service + Online.Boarding + Gate.Location + 
               On.board.Service + Seat.Comfort + Leg.Room.Service + 
               Cleanliness + Food.and.Drink + In.flight.Service + 
               In.flight.Wifi.Service + In.flight.Entertainment + 
               Baggage.Handling, data = df, family = "binomial")

# Print the model summary
summary(model)

# Extract the model coefficients with 95% confidence intervals
confint(model)

# Extract the model coefficients
coef(model)

# Produce diagnostic plots for the model
plot(model)

# Calculate the cumulative share of satisfaction ratings and customers
df <- df %>% arrange(Gate.Location)
x <- df$Gate.Location
n <- length(x)
S <- sum(x)
x <- sort(x)
i <- cumsum(x)
p <- i / S
q <- seq(1/n, 1, length=n)

# Create a data frame with the cumulative shares
df_l <- data.frame(cum_satisfaction = p, cum_customers = q)

# Plot the Lorenz curve
ggplot(df_l, aes(x = cum_customers, y = cum_satisfaction)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ggtitle("Lorenz Curve") +
  xlab("Cumulative Share of Customers") +
  ylab("Cumulative Share of Satisfaction Ratings")

# apply the function to each column of the dataframe
gini_index <- sapply(df[, c("Departure.and.Arrival.Time.Convenience", "Ease.of.Online.Booking", "Check.in.Service", "Online.Boarding", "Gate.Location", "On.board.Service", "Seat.Comfort", "Leg.Room.Service", "Cleanliness", "Food.and.Drink", "In.flight.Service", "In.flight.Wifi.Service", "In.flight.Entertainment", "Baggage.Handling")], gini)

# print the results
print(gini_index)

df$Gender <- factor(df$Gender)
df$Customer.Type <- factor(df$Customer.Type)
df$Type.of.Travel <- factor(df$Type.of.Travel)
df$Class <- factor(df$Class)
df$Satisfaction <- factor(df$Satisfaction)

set.seed(123)  # for reproducibility
split <- sample.split(df$Satisfaction, SplitRatio = 0.7)  # 70% training, 30% testing
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

model <- glm(Satisfaction ~ Gender + Age + Customer.Type + Type.of.Travel + Class + Flight.Distance + 
               Departure.Delay + Arrival.Delay + Departure.and.Arrival.Time.Convenience + 
               Ease.of.Online.Booking + Check.in.Service + Online.Boarding + Gate.Location + 
               On.board.Service + Seat.Comfort + Leg.Room.Service + Cleanliness + Food.and.Drink + 
               In.flight.Service + In.flight.Wifi.Service + In.flight.Entertainment + Baggage.Handling, 
             data = train, family = binomial)
summary(model)  # view the summary of the model

pred <- predict(model, newdata = test, type = "response")
pred_class <- ifelse(pred > 0.5, "Satisfied", "Dissatisfied")
table(pred_class, test$Satisfaction)  # view the confusion matrix

#

model <- glm(Satisfaction ~ ., data = train, family = binomial)
summary(model)

library(pROC)
roc_obj <- roc(test$Satisfaction, pred)
plot(roc_obj)

sapply(df, is.numeric)

df$Gender <- as.numeric(df$Gender)
df$Customer.Type <- as.numeric(df$Customer.Type)
df$ Type.of.Travel <- as.numeric(df$ Type.of.Travel)
df$Class <- as.numeric(df$Class)
df$Satisfaction <- as.numeric(df$NonNumeSatisfactionricColumn)


df$Gender <- factor(df$Gender)
df$Customer.Type <- factor(df$Customer.Type)
df$Type.of.Travel <- factor(df$Type.of.Travel)
df$Class <- factor(df$Class)

df <- df[, -c(1, 22)]

df_scaled <- scale(df[, -c(2:5)])

pca <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

fviz_eig(pca, addlabels = TRUE)

pca_var <- fviz_pca_var(pca, col.var = "contrib") + theme_minimal()
pca_var

pca_ind <- fviz_pca_ind(pca, geom = "point", col.ind = df$Departure.and.Arrival.Time.Convenience)
pca_ind

num_vars <- df[, c(3, 7:20)]
num_vars <- na.omit(num_vars)

num_vars_scale <- scale(num_vars)

pca <- prcomp(num_vars_scale, center = TRUE, scale. = TRUE)

library(ggplot2)

# select only columns with rating data
cols <- c("Departure.and.Arrival.Time.Convenience", "Ease.of.Online.Booking", "Check.in.Service", 
          "Online.Boarding", "Gate.Location", "On.board.Service", "Seat.Comfort", "Leg.Room.Service", 
          "Cleanliness", "Food.and.Drink", "In.flight.Service", "In.flight.Wifi.Service", 
          "In.flight.Entertainment", "Baggage.Handling")
df_ratings <- df[, cols]

# get means for each rating column
means <- colMeans(df_ratings)

# create data frame for plot
df_plot <- data.frame(category = names(means), rating = means)

df_plot <- df_plot[order(df_plot$rating, decreasing = TRUE), ]


# create plot
ggplot(df_plot, aes(x = category, y = rating)) + 
  geom_point(size = 3) + 
  geom_path() + 
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) + 
  labs(x = "", y = "Rating", title = "Airline Passenger Ratings") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create example data
df <- data.frame(
  "Departure.and.Arrival.Time.Convenience" = c(5, 3, 2, 4, 5),
  "Ease.of.Online.Booking" = c(4, 3, 5, 2, 4),
  "Check.in.Service" = c(3, 4, 2, 5, 3),
  "Online.Boarding" = c(4, 3, 2, 4, 5),
  "Gate.Location" = c(3, 2, 4, 5, 3),
  "On.board.Service" = c(4, 5, 2, 3, 4),
  "Seat.Comfort" = c(3, 4, 5, 2, 3),
  "Leg.Room.Service" = c(2, 3, 4, 5, 2),
  "Cleanliness" = c(4, 5, 3, 2, 4),
  "Food.and.Drink" = c(3, 2, 4, 5, 3),
  "In.flight.Service" = c(5, 4, 3, 2, 5),
  "In.flight.Wifi.Service" = c(2, 3, 4, 5, 2),
  "In.flight.Entertainment" = c(3, 4, 2, 5, 4),
  "Baggage.Handling" = c(4, 2, 3, 5, 4)
)

# select only columns with rating data
cols <- c("Departure.and.Arrival.Time.Convenience", "Ease.of.Online.Booking", "Check.in.Service", 
          "Online.Boarding", "Gate.Location", "On.board.Service", "Seat.Comfort", "Leg.Room.Service", 
          "Cleanliness", "Food.and.Drink", "In.flight.Service", "In.flight.Wifi.Service", 
          "In.flight.Entertainment", "Baggage.Handling")
df_ratings <- df[, cols]

# get means for each rating column and sort by mean
means <- sort(colMeans(df_ratings))

# create data frame for plot with sorted means
df_plot <- data.frame(category = names(means), rating = means)

# create plot with categories in order of mean rating
ggplot(df_plot, aes(x = reorder(category, rating), y = rating)) + 
  geom_point(size = 3) + 
  geom_path() + 
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) + 
  labs(x = "", y = "Rating", title = "Airline Passenger Ratings") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# select only relevant columns
df_bag <- df[, c("Class", "Flight.Distance")]

# create plot
ggplot(df_bag, aes(x = Class, y = Flight.Distance, fill = Class)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, alpha = 0.5) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(x = "Class", y = "Flight Distance", title = "Bag Plot of Flight Distance by Class") +
  theme_minimal()

correlation_matrix <- cor(df[,c("Age", "Flight.Distance", "Departure.Delay", "Arrival.Delay", "Departure.and.Arrival.Time.Convenience", "Ease.of.Online.Booking", "Check.in.Service", "Online.Boarding", "Gate.Location", "On.board.Service", "Seat.Comfort", "Leg.Room.Service", "Cleanliness", "Food.and.Drink", "In.flight.Service", "In.flight.Wifi.Service", "In.flight.Entertainment", "Baggage.Handling")])

print(round(correlation_matrix, 2))

# Load the required library
library(corrplot)

# Create a correlation matrix
correlation_matrix <- cor(df[,c("Age", "Flight.Distance", "Departure.Delay", "Arrival.Delay", "Departure.and.Arrival.Time.Convenience", "Ease.of.Online.Booking", "Check.in.Service", "Online.Boarding", "Gate.Location", "On.board.Service", "Seat.Comfort", "Leg.Room.Service", "Cleanliness", "Food.and.Drink", "In.flight.Service", "In.flight.Wifi.Service", "In.flight.Entertainment", "Baggage.Handling")])

# Print the correlation matrix
print(round(correlation_matrix, 2))

library(FactoMineR)
library(dplyr)
df_numeric <- df %>% select_if(is.numeric)
pca <- PCA(df_numeric, graph = FALSE)
library(factoextra)
fviz_pca_ind(pca, col.ind = df$Satisfaction)

library(missMDA)

# Assuming that your data frame is called "df" and only contains numeric columns
df_numeric <- df[, sapply(df, is.numeric)]

# Impute missing values using MCA
df_imputed <- imputePCA(df_numeric)

# Perform PCA on the imputed data
pca <- PCA(df_imputed$completeObs, graph = FALSE)

# Plot the results
plot.PCA(pca)

# load required package
library(aplpack)

# create a subset of the data containing only the columns with rating values
rating_cols <- c("Departure.and.Arrival.Time.Convenience", "Ease.of.Online.Booking",
                 "Check.in.Service", "Online.Boarding", "Gate.Location", "On.board.Service",
                 "Seat.Comfort", "Leg.Room.Service", "Cleanliness", "Food.and.Drink",
                 "In.flight.Service", "In.flight.Wifi.Service", "In.flight.Entertainment",
                 "Baggage.Handling")

rating_data <- df[rating_cols]

# create a bagplot of the rating data
bagplot(rating_data)

# Load required packages
library(ineq)
library(ggplot2)

# Create a data frame with just the Online Boarding column
entertainment <- df$In.flight.Entertainment

# Calculate the Gini index of Age
gini_entertainment <- ineq::Gini(entertainment)

# Calculate the Lorenz curve of Age
lorenz_entertainment <- ineq::Lc(entertainment)

# Plot the Lorenz curve of Age
plot(lorenz_entertainment, lwd = 2, col = "blue", main = "Lorenz Curve of In-flight Entertainment")

# Add the line of perfect equality
abline(0, 1, lwd = 2, col = "red")

# Add the Gini index to the plot
text(0.5, 0.2, paste0("Gini index = ", round(gini_entertainment, 2)), cex = 1.2)

# Load required libraries
library(tidyverse)
library(caret)

# Read in the data
df <- read.csv("path/to/your/file.csv")

# Convert the "Satisfaction" column to a factor with 3 levels
df$Satisfaction <- factor(df$Satisfaction, levels = c("Neutral", "Dissatisfied", "Satisfied"))

# Remove the ID column since it's not needed for the PCA
df <- select(df, -ID)

# Standardize the data
df_std <- preProcess(df, method = c("center", "scale")) %>% predict(df)

# Perform PCA
pca <- prcomp(df_std, scale = TRUE)

# Create a scree plot to determine the number of components to retain
screeplot(pca, type = "l")

# Create a biplot of the first two principal components
biplot(pca, choices = c(1, 2), scale = 0)

# Color the points by satisfaction level
ggplot(df_std, aes(x = PC1, y = PC2, color = Satisfaction)) +
  geom_point() +
  theme_bw() +
  xlab("PC1") +
  ylab("PC2") +
  ggtitle("PCA Plot")

summary(df)

df <- data.frame(lapply(df, as.numeric))

# Load the necessary libraries
library(ggplot2)
library(dplyr)

# Read the data from a CSV file
df <- read.csv("path/to/your/data.csv")

library(dplyr)

library(ggplot2)

library(dplyr)

# Recode "satisfaction" variable as binary
df <- df %>% mutate(satisfaction_binary = ifelse(Satisfaction == "Satisfied", 1, 0))

# Load the required package
library(ggplot2)

###

library(ggplot2)
library(ggformula)

library(ggplot2)
library(ggfortify)

# Load the required package
library(ggplot2)

###

library(ggplot2)

# create a data frame with the satisfaction and in-flight wifi columns
df <- data.frame(
  satisfaction = c("Satisfied", "Satisfied", "Unsatisfied", "Satisfied", "Unsatisfied", "Satisfied"),
  in_flight_wifi = c(4, 2, 3, 5, 1, 4)
)

###

# convert the satisfaction column to a binary variable
df$satisfaction_binary <- ifelse(df$Satisfaction == "Satisfied", 1, 0)

# fit a logistic regression model
model <- glm(satisfaction_binary ~ In.flight.Wifi.Service, data = df, family = binomial())

# create a sequence of values for the in-flight wifi rating
wifi_seq <- seq(from = 1, to = 5, by = 0.01)

# predict the probability of satisfaction for each value in the sequence
pred <- predict(model, newdata = data.frame(In.flight.Wifi.Service = wifi_seq), type = "response")

# combine the predicted probabilities with the wifi sequence into a data frame
plot_df <- data.frame(wifi_seq, pred)

# create the plot using ggplot2
ggplot(df, aes(x = In.flight.Wifi.Service, y = satisfaction_binary)) +
  geom_point(aes(color = Satisfaction)) +
  geom_line(data = plot_df, aes(x = wifi_seq, y = pred), color = "blue") +
  scale_color_manual(values = c("Satisfied" = "green", "Unsatisfied" = "red")) +
  labs(x = "In-Flight WiFi Rating", y = "Probability of Satisfaction", title = "Logistic Regression") +
  theme_minimal()

###

# create a data frame with the satisfaction and in-flight wifi columns
df <- data.frame(
  satisfaction = c("Satisfied", "Satisfied", "Unsatisfied", "Satisfied", "Unsatisfied", "Satisfied"),
  in_flight_wifi = c(4, 2, 3, 5, 1, 4)
)

# convert the satisfaction column to a binary variable
df$satisfaction_binary <- ifelse(df$Satisfaction == "Satisfied", 1, 0)

# fit a logistic regression model
model <- glm(satisfaction_binary ~ Online.Boarding, data = df, family = binomial())

# create a sequence of values for the in-flight wifi rating
online_boarding_seq <- seq(from = 1, to = 5, by = 0.01)

# predict the probability of satisfaction for each value in the sequence
pred <- predict(model, newdata = data.frame(Online.Boarding = online_boarding_seq), type = "response")

# combine the predicted probabilities with the wifi sequence into a data frame
plot_df <- data.frame(online_boarding_seq, pred)

# create the plot using ggplot2
ggplot(df, aes(x = Online.Boarding, y = satisfaction_binary)) +
  geom_point(aes(color = Satisfaction)) +
  geom_line(data = plot_df, aes(x = online_boarding_seq, y = pred), color = "blue") +
  scale_color_manual(values = c("Satisfied" = "green", "Unsatisfied" = "red")) +
  labs(x = "Online Boarding Rating", y = "Probability of Satisfaction", title = "Logistic Regression") +
  theme_minimal()

###

# fit a logistic regression model
model <- glm(satisfaction_binary ~ In.flight.Entertainment, data = df, family = binomial())

# create a sequence of values for the in-flight wifi rating
entertainment_seq <- seq(from = 1, to = 5, by = 0.01)

# predict the probability of satisfaction for each value in the sequence
pred <- predict(model, newdata = data.frame(In.flight.Entertainment = entertainment_seq), type = "response")

# combine the predicted probabilities with the wifi sequence into a data frame
plot_df <- data.frame(entertainment_seq, pred)

# create the plot using ggplot2
ggplot(df, aes(x = In.flight.Entertainment, y = satisfaction_binary)) +
  geom_point(aes(color = Satisfaction)) +
  geom_line(data = plot_df, aes(x = entertainment_seq, y = pred), color = "blue") +
  scale_color_manual(values = c("Satisfied" = "green", "Unsatisfied" = "red")) +
  labs(x = "Entertainment Rating", y = "Probability of Satisfaction", title = "Logistic Regression") +
  theme_minimal()

###

# fit a logistic regression model
model <- glm(satisfaction_binary ~ Seat.Comfort, data = df, family = binomial())

# create a sequence of values for the in-flight wifi rating
seat_seq <- seq(from = 1, to = 5, by = 0.01)

# predict the probability of satisfaction for each value in the sequence
pred <- predict(model, newdata = data.frame(Seat.Comfort = seat_seq), type = "response")

# combine the predicted probabilities with the wifi sequence into a data frame
plot_df <- data.frame(seat_seq, pred)

# create the plot using ggplot2
ggplot(df, aes(x = Seat.Comfort, y = satisfaction_binary)) +
  geom_point(aes(color = Satisfaction)) +
  geom_line(data = plot_df, aes(x = seat_seq, y = pred), color = "blue") +
  scale_color_manual(values = c("Satisfied" = "green", "Unsatisfied" = "red")) +
  labs(x = "Seat Comfort Rating", y = "Probability of Satisfaction", title = "Logistic Regression") +
  theme_minimal()

###

# fit a logistic regression model
model <- glm(satisfaction_binary ~ Leg.Room.Service, data = df, family = binomial())

# create a sequence of values for the in-flight wifi rating
leg_room_seq <- seq(from = 1, to = 5, by = 0.01)

# predict the probability of satisfaction for each value in the sequence
pred <- predict(model, newdata = data.frame(Leg.Room.Service = seat_seq), type = "response")

# combine the predicted probabilities with the wifi sequence into a data frame
plot_df <- data.frame(leg_room_seq, pred)

# create the plot using ggplot2
ggplot(df, aes(x = Leg.Room.Service, y = satisfaction_binary)) +
  geom_point(aes(color = Satisfaction)) +
  geom_line(data = plot_df, aes(x = leg_room_seq, y = pred), color = "blue") +
  scale_color_manual(values = c("Satisfied" = "green", "Unsatisfied" = "red")) +
  labs(x = "Leg Room Rating", y = "Probability of Satisfaction", title = "Logistic Regression") +
  theme_minimal()

###

library(likert)
library(rpart)

# Load the Airline dataset
data(Airline)

# Fit a decision tree model to predict satisfaction
dt <- rpart(Satisfaction ~ ., df = Airline, method = "class")

# Plot the decision tree
plot(dt, margin = 0.1)

# Calculate the Gini importance of each variable
varimp <- varImp(dt, scale = FALSE)

# Print the variable importance table
print(varimp)

# Create the decision tree
tree <- rpart(Satisfaction ~ Departure.and.Arrival.Time.Convenience + Ease.of.Online.Booking + Check.in.Service + Online.Boarding + Gate.Location + On.board.Service + Seat.Comfort + Leg.Room.Service + Cleanliness + Food.and.Drink + In.flight.Service + In.flight.Wifi.Service + In.flight.Entertainment + Baggage.Handling, data = df, method = "class")

# Print the decision tree
print(tree)

# Plot the decision tree
plot(tree, uniform = TRUE, main = "Decision Tree for Passenger Satisfaction")

# Rank the variables in order of importance
var_importance <- varImp(tree, scale = FALSE)
print(var_importance)


