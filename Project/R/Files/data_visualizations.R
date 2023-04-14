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

df = read.csv("airline_passenger_satisfaction.csv",header = T)
df = data.frame(df)

head(df,3)

summary = as.data.frame(skim(df))
summary

# We know that less than 30% is maximum missing values that are allowed to drop
# Since the arrival delay column contains 0.3% null values, we can drop them

df = df %>% na.omit()

colors <- c("red", "blue", "green", "orange", "purple", "brown", "pink", "gray", "olivedrab", "cyan", "magenta", "yellow", "darkred", "darkblue", "darkgreen", "coral", "violet", "beige", "lavender", "turquoise", "lightpink", "lightgray", "olivedrab1")

# Create a bar chart of Gender Distribution
ggplot(df, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

# Create a histogram of Age Distribution
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Create a pie chart of Customer Type Distribution

# Count the number of observations for each category
obs_count_cust_type <- table(df$Customer.Type)

# Calculate the percentage of each category
obs_percent_cust_type <- round(prop.table(obs_count_cust_type) * 100, 1)

# Create a data frame for the pie chart
pie_data <- data.frame(Customer.Type = names(obs_percent_cust_type), obs_percent_cust_type)

# Create the pie chart using ggplot2
ggplot(pie_data, aes(x = "", y = obs_percent_cust_type, fill = Customer.Type)) +
  geom_col() +
  geom_text(aes(label = paste0(obs_percent_cust_type, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()

# Create a pie chart of Type of Travel Distribution

# Count the number of observations for each category
obs_count_type_travel <- table(df$Type.of.Travel)

# Calculate the percentage of each category
obs_percent_type_travel <- round(prop.table(obs_count_type_travel) * 100, 1)

# Create a data frame for the pie chart
pie_data <- data.frame(Type.of.Travel = names(obs_percent_type_travel), obs_percent_type_travel)

# Create the pie chart using ggplot2
ggplot(pie_data, aes(x = "", y = obs_percent_type_travel, fill = Type.of.Travel)) +
  geom_col() +
  geom_text(aes(label = paste0(obs_percent_type_travel, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()

# Create a pie chart of Type of Class

# Count the number of observations for each category
obs_count_class <- table(df$Class)

# Calculate the percentage of each category
obs_percent_class <- round(prop.table(obs_count_class) * 100, 1)

# Create a data frame for the pie chart
pie_data <- data.frame(Class = names(obs_percent_class), obs_percent_class)

# Create the pie chart using ggplot2
ggplot(pie_data, aes(x = "", y = obs_percent_class, fill = Class)) +
  geom_col() +
  geom_text(aes(label = paste0(obs_percent_class, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()

# Create a list of columns to plot histograms
columns_to_plot_histogram <- c("Flight.Distance", "Departure.Delay", "Arrival.Delay", 
                     "Departure.and.Arrival.Time.Convenience", "Ease.of.Online.Booking", 
                     "Check.in.Service", "Online.Boarding", "Gate.Location", "On.board.Service", 
                     "Seat.Comfort", "Leg.Room.Service", "Cleanliness", "Food.and.Drink", 
                     "In.flight.Service", "In.flight.Wifi.Service", "In.flight.Entertainment", 
                     "Baggage.Handling")

# loop through the columns and plot the histograms
for (col in columns_to_plot_histogram) {
  ggplot(df, aes(x = df[[col]])) +
    geom_histogram(fill = "olive", alpha = 0.5) +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal()
}

# Create a pie chart of Satisfaction

# Count the number of observations for each category
obs_count_satisfaction <- table(df$Satisfaction)

# Calculate the percentage of each category
obs_percent_satisfaction <- round(prop.table(obs_count_satisfaction) * 100, 1)

# Create a data frame for the pie chart
pie_data <- data.frame(Satisfaction = names(obs_percent_satisfaction), obs_percent_satisfaction)

# Create the pie chart using ggplot2
ggplot(pie_data, aes(x = "", y = obs_percent_satisfaction, fill = Satisfaction)) +
  geom_col() +
  geom_text(aes(label = paste0(obs_percent_satisfaction, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()

# Create violin plot of Age by Customer Type and Satisfaction

ggplot(df, aes(x=Customer.Type, y=Age, color=Satisfaction)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_color_discrete(name="Satisfaction") +
  ggtitle("Violin Plot of Age by Customer Type and Satisfaction") +
  xlab("Customer Type") +
  ylab("Age") +
  theme_minimal()

# Create violin plot of Onboard Service Satisfaction

ggplot(df, aes(x = On.board.Service, fill = Satisfaction)) + 
  geom_bar(position = "dodge") + 
  ggtitle("On-board Service/Satisfaction") + 
  xlab("On-board Service") + 
  ylab("Count") + 
  theme_minimal()

# Create histogram of Seat Comfort

ggplot(df, aes(x = Seat.Comfort)) + 
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  ggtitle("Histogram of Seat Comfort") +
  xlab("Seat Comfort") +
  ylab("Frequency")

# Create a distplot of Satisfaction x Seat Comfort

ggplot(df, aes(x=Seat.Comfort, fill=Satisfaction)) + 
  geom_density(alpha=0.7, adjust=5) + 
  labs(x='Seat Comfort', title='Seat Comfort/Satisfaction', fill='Satisfaction') + 
  scale_fill_manual(values=c("red","blue")) + 
  theme_classic()

# Create a stacked histogram of Satisfaction x Seat Comfort

ggplot(df, aes(x=Seat.Comfort, fill=Satisfaction)) + 
  geom_histogram(position="stack", binwidth=0.5) +
  ggtitle("Seat Comfort/Satisfaction (Stacked Histogram)") +
  xlab("Seat Comfort") +
  ylab("Count") +
  theme_minimal()

# Create a distplot of Satisfaction x Cleanliness

ggplot(df, aes(x=Cleanliness, fill=Satisfaction)) + 
  geom_density(alpha=0.5) + 
  scale_fill_manual(values=c("red", "blue")) + 
  ggtitle("Cleanliness/Satisfaction", subtitle = "") + 
  xlab("Cleanliness") + 
  ylab("Density")

# Create a stacked histogram of Satisfaction x Seat Comfort

ggplot(df, aes(x=Ease.of.Online.Booking, fill=Satisfaction)) + 
  geom_histogram(position="stack", binwidth=0.5) +
  ggtitle("Ease of Online Booking/Satisfaction (Stacked Histogram)") +
  xlab("Ease of Online Booking") +
  ylab("Count") +
  theme_minimal()

# Create a stacked histogram of Age x Class

ggplot(df, aes(x = Age, fill = Class)) +
  geom_histogram(binwidth = 5, position = "stack") +
  labs(x = "Age", y = "Count", fill = "Class") +
  scale_fill_manual(values = c("red", "blue", "green")) +
  theme_classic()

# Create a stacked histogram of Age x Type of Travel

ggplot(df, aes(x = Age, fill = Type.of.Travel)) +
  geom_histogram(binwidth = 5, position = "stack") +
  labs(x = "Age", y = "Count", fill = "Type.of.Travel") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_classic()

# Create histogram of Flight Distance
ggplot(df, aes(x = Flight.Distance)) +
  geom_histogram(bins = 20, fill = "blue", color = "white") +
  labs(title = "Histogram of Flight Distance", x = "Flight Distance", y = "Frequency")

# Create histogram of Departure Delay
ggplot(df, aes(x = Departure.Delay)) +
  geom_histogram(bins = 20, fill = "blue", color = "white") +
  labs(title = "Histogram of Departure Delay", x = "Departure Delay", y = "Frequency")

# Create histogram of Arrival Delay
ggplot(df, aes(x = Arrival.Delay)) +
  geom_histogram(bins = 20, fill = "blue", color = "white") +
  labs(title = "Histogram of Arrival Delay", x = "Arrival Delay", y = "Frequency")

# Create stacked histogram of Age vs. Satisfaction
df %>%
  filter(Age >= 5, Age < 80) %>%
  ggplot(aes(x = Age, fill = Satisfaction)) +
  geom_bar(position = "stack") +
  scale_x_continuous(breaks = seq(5, 80, by = 5)) +
  labs(x = "Age", y = "Age vs. Passenger Satisfaction") +
  theme_minimal()

# Create barchart of Departure and Arrival Time Convenience by Class
ggplot(df, aes(x = Class, fill = as.factor(Departure.and.Arrival.Time.Convenience))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Departure and Arrival Time Convenience by Class") +
  theme_minimal()

# Create barchart of Ease of Online Booking by Class
ggplot(df, aes(x = Class, fill = as.factor(Ease.of.Online.Booking))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Ease of Online Booking by Class") +
  theme_minimal()

# Create barchart of Check-in Service
ggplot(df, aes(x = Class, fill = as.factor(Check.in.Service))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Check-in Service by Class") +
  theme_minimal()

# Create barchart of Online Boarding
ggplot(df, aes(x = Class, fill = as.factor(Online.Boarding))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Online Boarding by Class") +
  theme_minimal()

# Create barchart of Gate Location
ggplot(df, aes(x = Class, fill = as.factor(Gate.Location))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Gate Location by Class") +
  theme_minimal()

# Create barchart of Onboard Service
ggplot(df, aes(x = Class, fill = as.factor(On.board.Service))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Onboard Service by Class") +
  theme_minimal()

# Create barchart of Seat Comfort
ggplot(df, aes(x = Class, fill = as.factor(Seat.Comfort))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Seat Comfort by Class") +
  theme_minimal()

# Create barchart of Leg Room Service
ggplot(df, aes(x = Class, fill = as.factor(Leg.Room.Service))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Leg Room Service by Class") +
  theme_minimal()

# Create barchart of Cleanliness
ggplot(df, aes(x = Class, fill = as.factor(Cleanliness))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Cleanliness by Class") +
  theme_minimal()

# Create barchart of Food and Drink
ggplot(df, aes(x = Class, fill = as.factor(Food.and.Drink))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Food and Drink by Class") +
  theme_minimal()

# Create barchart of In-flight Service
ggplot(df, aes(x = Class, fill = as.factor(In.flight.Service))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "In-flight Service by Class") +
  theme_minimal()

# Create barchart of In-flight Wifi Service
ggplot(df, aes(x = Class, fill = as.factor(In.flight.Wifi.Service))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "In-flight Wifi Service by Class") +
  theme_minimal()

# Create barchart of In-flight Entertainment
ggplot(df, aes(x = Class, fill = as.factor(In.flight.Entertainment))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "In-flight Entertainment by Class") +
  theme_minimal()

# Create barchart of Baggage Handling
ggplot(df, aes(x = Class, fill = as.factor(Baggage.Handling))) +
  geom_bar(position = position_dodge(), alpha = 0.8) +
  scale_fill_discrete(name = "Rating") +
  labs(x = "Class", y = "Count", title = "Baggage Handling by Class") +
  theme_minimal()

# Convert columns to factors
df$Gender <- as.factor(df$Gender)
df$Customer.Type <- as.factor(df$Customer.Type)
df$Type.of.Travel <- as.factor(df$Type.of.Travel)
df$Class <- as.factor(df$Class)
df$Satisfaction <- as.factor(df$Satisfaction)

# Create the decision tree
dt <- rpart(Satisfaction ~ ., data = df, method = "class")

# Plot the decision tree
plot(dt, uniform = TRUE, main = "Airline Passenger Satisfaction Decision Tree")
text(dt, use.n = TRUE, all = TRUE, cex = 0.5)

# Make predictions on the same dataset
df$predicted <- predict(dt, type = "class")

# Create the confusion matrix
confusion_matrix <- table(df$Satisfaction, df$predicted)

# Print the confusion matrix
confusion_matrix

# Calculate the accuracy of the decision tree
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

# Make predictions on a test dataset
test_data <- df[1001:1500, ]  # assuming first 1000 rows are training data
predicted <- predict(dt, newdata = test_data, type = "class")

# Create a data frame to compare actual vs predicted values
result_df <- data.frame(Y_test = test_data$Satisfaction, Y_pred = predicted)

# Show first 10 rows of the result data frame
head(result_df, 10)

# Define a function to create a feature importance plot
f_importances <- function(coef, names, top = -1) {
  # Combine feature names and coefficients, and sort by coefficient value
  df <- data.frame(names, coef)
  df <- df[order(-df$coef),]
  
  # Show all features if top is negative
  if (top < 0) {
    top <- nrow(df)
  }
  
  # Create a horizontal bar plot of the top features
  barplot(df$coef[1:top]/max(df$coef), horiz = TRUE, names.arg = df$names[1:top],
          main = "Feature Importances for Decision Tree", cex.names = 0.8, xaxt = "n")
  
  # Add a custom x-axis using a 0 to 1 scale
  axis(side = 1, at = seq(0, 1, length.out = 6), labels = seq(0, 1, length.out = 6))
}

# Extract feature names and importances from the decision tree object
feature_names <- names(dt$variable.importance)
importances <- dt$variable.importance

# Create a feature importance plot of the top 7 features
f_importances(importances, feature_names, top = 8)

# Create a bar plot with labels
ggplot(importances[1:7,], aes(x=feature_names, y=importance_values)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=importance_values), vjust=1.5, color="white", size=4)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train <- iris[trainIndex,]
test <- iris[-trainIndex,]

# Train a decision tree model
model <- rpart(Species ~ ., data = train)

# Evaluate the model using 10-fold cross-validation
cv <- trainControl(method = "cv", number = 10)
cv_results <- train(Species ~ ., data = train, method = "rpart", trControl = cv)
print(cv_results)

# Plot learning curves
plotLearningCurve(model, train$x, train$y, metric = "Accuracy", ylim = c(0.7, 1), main = "Learning Curve")

# Create a random forest model and fit it to the training data
rf <- randomForest(Satisfaction ~ ., data = df, ntree = 500, mtry = 8, maxdepth = 7)

# Get the accuracy score of the trained model on the training set
predicted_train <- predict(rf, x_train)
accuracy_train <- sum(predicted_train == y_train) / length(y_train)
accuracy_train

# Set ggplot theme
theme_set(theme_bw())

# Create barplot of Ease of Online Booking
ggplot(df, aes(x = `Ease.of.Online.Booking`, fill = Satisfaction)) +
  geom_bar(position = "dodge") +
  labs(title = "Ease of Online Booking/Satisfaction", x = "Ease of Online Booking", y = "Count", fill = "Satisfaction") +
  theme(plot.title = element_text(size = 18, color = "black", face = "italic"))

# Create barplot of Ease of On-board Service
ggplot(df, aes(x = On.board.Service, fill = Satisfaction)) + 
  geom_bar(position = "dodge") +
  labs(x = "On-board Service", y = "Count", fill = "Satisfaction") + 
  scale_fill_discrete(name = "Satisfaction") +
  ggtitle("On-board Service/Satisfaction") + 
  theme(plot.title = element_text(size = 18, color = "black", face = "italic"))

# Subset the data to include only "Neutral or Dissatisfied" and "Satisfied" for "In-flight Wifi Service"
df_sub <- df %>% filter(Satisfaction %in% c("Neutral or Dissatisfied", "Satisfied") & 
                          !is.na(In.flight.Wifi.Service))

# Count the number of observations in each category
df_count <- df_sub %>% count(Satisfaction)

# Calculate the percentage of each category
df_count <- df_count %>% mutate(percent = n/sum(n))

# Create a pie chart with percentage labels
ggplot(df_count, aes(x="", y=percent, fill=Satisfaction)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  labs(title = "Percentage of Neutral or Dissatisfied and Satisfied in In-flight Wifi Service") +
  scale_fill_manual(values = c("red", "green")) +
  theme_void()

# Create the density plot of In-flight Entertainment vs. Satisfaction
ggplot(df, aes(x = `In.flight.Entertainment`, fill = Satisfaction, weight = 3)) +
  geom_density(alpha = 0.5) +
  labs(title = "In-flight Entertainment/Satisfaction", x = "In-flight Entertainment", y = "Density") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()

# Create a scatterplot of Flight Distance vs. Departure Delay
ggplot(df, aes(x = Departure.Delay, y = Flight.Distance)) +
  geom_point(color = "black", fill = "white", size = 3, shape = 21, stroke = 1) +
  labs(title = "Departure Delay vs. Flight Distance", x = "Departure Delay", y = "Flight Distance") +
  theme_bw()

# Create a scatterplot of Flight Distance vs. Arrival Delay
ggplot(df, aes(x = Arrival.Delay, y = Flight.Distance)) +
  geom_point(color = "black", fill = "white", size = 3, shape = 21, stroke = 1) +
  labs(title = "Arrival Delay vs. Flight Distance", x = "Arrival Delay", y = "Flight Distance") +
  theme_bw()

# Create a scatterplot of Departure Delay vs. Arrival Delay
ggplot(df, aes(x = `Departure.Delay`, y = `Arrival.Delay`)) +
  geom_point(color = "black", fill = "white", size = 3, shape = 21) +
  labs(title = "Departure Delay vs Arrival Delay", x = "Departure Delay", y = "Arrival Delay") +
  theme_bw()

# Create a density plot of Satisfaction vs. Age
ggplot(df, aes(x=Age, color=Satisfaction, linetype=Satisfaction)) +
  geom_density() +
  scale_color_manual(values=c("red", "blue")) +
  scale_linetype_manual(values=c("solid", "solid")) +
  labs(x="Age", y="Distribution", title="Satisfaction + Age") +
  theme_bw() +
  theme(plot.title=element_text(size=16, face="bold"),
        axis.title=element_text(size=14))

# Density plot of Type of Travel vs. Age vs. Satisfaction
ggplot(df, aes(x=`Age`, color=`Type.of.Travel`, linetype=`Satisfaction`)) + 
  geom_density() + 
  scale_color_manual(values=c("blue", "black", "red")) +
  scale_linetype_manual(values=c("dotted", "dashed")) +
  labs(x="Age", y="Distribution", title="Satisfaction + Type of Travel + Age") +
  theme_bw() +
  theme(plot.title=element_text(size=16, face="bold"),
        axis.title=element_text(size=14),
        strip.text=element_text(size=12, face="bold"))

# Density plot of Satisfaction vs. Age
ggplot(df, aes(x=Age, color=Satisfaction, linetype=Satisfaction)) + 
  geom_density() + 
  scale_color_manual(values=c("blue", "red")) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  labs(x="Age", y="Distribution", title="Satisfaction + Age") +
  theme_bw() +
  theme(plot.title=element_text(size=16, face="bold"),
        axis.title=element_text(size=14),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Density plot of Satisfaction vs. Class vs. Age
ggplot(df, aes(x=Age, color=Satisfaction, linetype=Class)) + 
  geom_density() + 
  scale_color_manual(values=c("blue", "red")) +
  scale_linetype_manual(values=c("solid", "longdash", "dotted")) +
  labs(x="Age", y="Distribution", title="Satisfaction + Age + Class") +
  theme_bw() +
  theme(plot.title=element_text(size=16, face="bold"),
        axis.title=element_text(size=14),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

num_features <- df[, !colnames(df) %in% c("Customer Type", "Class", "Type of Travel", "Satisfaction")]

features_0_5 <- num_features %>% 
  select(-Flight.Distance, -Departure.Delay, -Arrival.Delay)

for (feature in names(df)[!(names(df) %in% c("Flight.Distance", "Departure.Delay", "Arrival.Delay"))]) {
  print(paste0(feature, ": ", unique(df[[feature]])))
}

df_long <- pivot_longer(df, cols = c(-Satisfaction, -ID, -Gender, -Age, -Customer.Type, -Type.of.Travel, -Class, -Flight.Distance, -Departure.Delay, -Arrival.Delay), names_to = "Features", values_to = "Values")

df %>%
  select(-c("Satisfaction", "ID", "Gender", "Age", "Customer.Type", "Type.of.Travel", "Class", "Flight.Distance", "Departure.Delay", "Arrival.Delay")) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Feature)) +
  geom_bar(position = "dodge") +
  labs(x = "Value", y = "Count", title = "Count of Features 0-5") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.position = "right",
        legend.title = element_blank())

# Scatterplot of Departure vs. Arrival Delays
ggplot(df, aes(x = `Departure.Delay`, y = `Arrival.Delay`, color = Satisfaction)) + 
  geom_point(size = 2, alpha = 0.8, shape = 21, fill = "white", color = "black", stroke = 0.5) +
  facet_wrap(~ `Satisfaction`, ncol = 2) +
  scale_color_manual(values = c("#E74C3C", "#2ECC71")) +
  labs(x = "Departure Delay (in minutes)", y = "Arrival Delay (in minutes)", color = "Satisfaction") +
  theme_minimal()

# Line Plot of Feature Counts
df %>%
  select(Online.Boarding, Seat.Comfort, Leg.Room.Service, In.flight.Wifi.Service, In.flight.Entertainment) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
  group_by(Feature, Value) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Value, y = Count, group = Feature, colour = Feature)) +
  geom_line(size = 1) +
  labs(x = "Value", y = "Count", title = "Overall Satisfaction") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.position = "right",
        legend.title = element_blank())

# Line Plot of Feature Counts - Neutral or Dissatisfied
df %>%
  filter(Satisfaction == "Neutral or Dissatisfied") %>%
  select(Online.Boarding, Seat.Comfort, Leg.Room.Service, In.flight.Wifi.Service, In.flight.Entertainment) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
  group_by(Feature, Value) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Value, y = Count, group = Feature, colour = Feature)) +
  geom_line(size = 1) +
  labs(x = "Value", y = "Count", title = "Neutral or Dissatisfied") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.position = "right",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) # to remove padding from the x axis

# Line Plot of Feature Counts - Satisfied
df %>%
  filter(Satisfaction == "Satisfied") %>%
  select(Online.Boarding, Seat.Comfort, Leg.Room.Service, In.flight.Wifi.Service, In.flight.Entertainment) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
  group_by(Feature, Value) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Value, y = Count, group = Feature, colour = Feature)) +
  geom_line(size = 1) +
  labs(x = "Value", y = "Count", title = "Satisfied") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.position = "right",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) # to remove padding from the x axis

table(df$Satisfaction)












