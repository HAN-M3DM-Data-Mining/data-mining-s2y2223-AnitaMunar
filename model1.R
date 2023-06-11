#Load in neccessary libraries
library(class)
library(ggplot2)
library(caret)
library(lattice)
library(tidyverse)
library(RColorBrewer)
library(tm)
library(slam)
library(e1071)
library(ISLR)
library(magrittr)
library(dplyr)
library(car)
library(stats)
library(reshape2)
library(randomForest)

# Step 1: Business Understanding (optional)
# Specify the objective and context of the analysis

# Step 2: Data Understanding
# Read the dataset from the CSV file
data <- read.csv("https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/data-mining-s2y2223-AnitaMunar/master/Wellbeing_and_lifestyle_data_Kaggle.csv")

# Explore the structure and summary statistics of the dataset
str(data)
summary(data)

# Step 3: Data Preparation
#  Separate raw data
selected_data <- data
# Check for missing values
sum(is.na(selected_data))

#editing incorrect data types by referring back to str()


#variable recoding
# Convert "DAILY_STRESS" column to integer
selected_data$DAILY_STRESS <- as.integer(selected_data$DAILY_STRESS)
#check after conversion
str(selected_data)
#check NA
sum(is.na(selected_data))
# Remove rows with missing values
selected_data <- na.omit(selected_data)
sum(is.na(selected_data))

# Convert "Female" to 0 and "Male" to 1
selected_data$GENDER <- ifelse(selected_data$GENDER == "Female", 0, 1)
str(selected_data)
#check NA 
sum(is.na(selected_data))



# Recode age variable
selected_data$AGE <- ifelse(selected_data$AGE %in% c("Less than 20"), 1,
                            ifelse(selected_data$AGE %in% c("21 to 35"), 2,
                                   ifelse(selected_data$AGE %in% c("36 to 50"), 3,
                                          ifelse(selected_data$AGE %in% c("51 or more"), 4, NA))))

# Convert AGE column to integer
selected_data$AGE <- as.integer(selected_data$AGE)


#check NA
sum(is.na(selected_data))
str(selected_data)

#edit timestamp variable
selected_data$Timestamp <- as.Date(selected_data$Timestamp, format = "%m/%d/%y")
str(selected_data)

# Check for missing values
sum(is.na(selected_data))




#distributions

# Filter integer variables
integer_vars <- selected_data[, sapply(selected_data, is.integer)]

# Run summary statistics
summary(integer_vars)


# Calculate the means of integer variables
means <- colMeans(integer_vars, na.rm = TRUE)

# Create a dataframe for plotting variable means
plot_data <- data.frame(variable = names(means), mean = means)

ggplot(plot_data, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  xlab("") +
  ylab("Mean") +
  ggtitle("Mean of Integer Variables") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#gender distribution
# Convert GENDER variable to factor with appropriate labels
selected_data$GENDER <- factor(selected_data$GENDER, levels = c(0, 1), labels = c("Female", "Male"))

# Pie chart for GENDER variable
gender_counts <- table(selected_data$GENDER)

pie_chart_gender <- ggplot(data = data.frame(gender_labels = names(gender_counts), gender_counts),
                           aes(x = "", y = gender_counts, fill = gender_labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Gender") +
  ggtitle("Distribution of Gender") +
  theme_void()

# Display the pie chart and mean
print(pie_chart_gender)


#wlb distribution
# Mean of WORK_LIFE_BALANCE variable
mean_work_life_balance <- mean(selected_data$WORK_LIFE_BALANCE, na.rm = TRUE)
print(paste("Mean of Work Life Balance:", round(mean_work_life_balance, 2)))



# Histogram for WORK_LIFE_BALANCE variable
histogram_work_life_balance <- ggplot(data = selected_data, aes(x = WORK_LIFE_BALANCE_SCORE)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Work Life Balance", y = "Frequency") +
  ggtitle("Distribution of Work Life Balance") +
  theme_minimal()

# Display the histogram
print(histogram_work_life_balance)



# Visualize the spread of each variable

# Create a histogram for FRUITS_VEGGIES
ggplot(selected_data, aes(x = FRUITS_VEGGIES)) +
  geom_histogram(fill = "coral", color = "black", bins = 6) +
  labs(title = "Distribution of FRUITS_VEGGIES") +
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  theme_minimal()

# Create a bar plot for DAILY_STRESS
ggplot(selected_data, aes(x = as.factor(DAILY_STRESS))) +
  geom_bar(fill = "maroon", color = "black") +
  labs(title = "Distribution of DAILY_STRESS") +
  theme_minimal()

# Create a histogram for PLACES_VISITED
ggplot(selected_data, aes(x = PLACES_VISITED)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 11) +
  labs(title = "Distribution of PLACES_VISITED") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Set the range of threshold values to test
threshold_values <- seq(3, 10, by = 0.5)

# Perform sensitivity analysis
results <- data.frame(Threshold = threshold_values, OutlierCount = NA)

for (i in seq_along(threshold_values)) {
  # Detect outliers based on the current threshold value for PLACES_VISITED
  outliers <- selected_data[selected_data$PLACES_VISITED >= threshold_values[i], ]
  
  # Record the count of outliers
  results$OutlierCount[i] <- nrow(outliers)
}

# Print the sensitivity analysis results
print(results)


# Create a histogram for CORE_CIRCLE
ggplot(selected_data, aes(x = CORE_CIRCLE)) +
  geom_histogram(fill = "navy", color = "black", bins = 11) +
  labs(title = "Distribution of CORE_CIRCLE") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for SUPPORTING_OTHERS
ggplot(selected_data, aes(x = SUPPORTING_OTHERS)) +
  geom_histogram(fill = "gold", color = "black", bins = 11) +
  labs(title = "Distribution of SUPPORTING_OTHERS") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for SOCIAL_NETWORK
ggplot(selected_data, aes(x = SOCIAL_NETWORK)) +
  geom_histogram(fill = "cyan", color = "black", bins = 11) +
  labs(title = "Distribution of SOCIAL_NETWORK") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for ACHIEVEMENT
ggplot(selected_data, aes(x = ACHIEVEMENT)) +
  geom_histogram(fill = "red", color = "black", bins = 11) +
  labs(title = "Distribution of ACHIEVEMENT") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for DONATION
ggplot(selected_data, aes(x = DONATION)) +
  geom_histogram(fill = "magenta", color = "black", bins = 11) +
  labs(title = "Distribution of DONATION") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for BMI_RANGE
ggplot(selected_data, aes(x = BMI_RANGE)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Distribution of BMI_RANGE") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Below 25", "Over 25")) +
  theme_minimal()


# Create a histogram for TODO_COMPLETED
ggplot(selected_data, aes(x = TODO_COMPLETED)) +
  geom_histogram(fill = "peru", color = "black", bins = 11) +
  labs(title = "Distribution of TODO_COMPLETED") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for FLOW
ggplot(selected_data, aes(x = FLOW)) +
  geom_histogram(fill = "grey", color = "black", bins = 11) +
  labs(title = "Distribution of FLOW") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a bar plot for DAILY_STEPS
ggplot(selected_data, aes(x = DAILY_STEPS)) +
  geom_bar(fill = "darksalmon", color = "black") +
  labs(title = "Distribution of DAILY_STEPS") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()


# Create a histogram for LIVE_VISION
ggplot(selected_data, aes(x = LIVE_VISION)) +
  geom_histogram(fill = "chartreuse", color = "black", bins = 11) +
  labs(title = "Distribution of LIVE_VISION") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a bar plot for SLEEP_HOURS
ggplot(selected_data, aes(x = SLEEP_HOURS)) +
  geom_bar(fill = "darkgreen", color = "black") +
  labs(title = "Distribution of SLEEP_HOURS") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_minimal()

# Create a histogram for LOST_VACATION
ggplot(selected_data, aes(x = LOST_VACATION)) +
  geom_histogram(fill = "wheat", color = "black", bins = 11) +
  labs(title = "Distribution of LOST_VACATION") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for DAILY_SHOUTING
ggplot(selected_data, aes(x = DAILY_SHOUTING)) +
  geom_histogram(fill = "darkorange", color = "black", bins = 11) +
  labs(title = "Distribution of DAILY_SHOUTING") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a bar plot for SUFFICIENT_INCOME
ggplot(selected_data, aes(x = as.factor(SUFFICIENT_INCOME))) +
  geom_bar(fill = "lightcyan", color = "black") +
  labs(title = "Distribution of SUFFICIENT_INCOME") +
  scale_x_discrete(labels = c("Insufficient", "Sufficient")) +
  theme_minimal()

# Create a histogram for PERSONAL_AWARDS
ggplot(selected_data, aes(x = PERSONAL_AWARDS)) +
  geom_histogram(fill = "lightpink", color = "black", bins = 11
  ) +
  labs(title = "Distribution of PERSONAL_AWARDS") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for TIME_FOR_PASSION
ggplot(selected_data, aes(x = TIME_FOR_PASSION)) +
  geom_histogram(fill = "yellow", color = "black", bins = 11) +
  labs(title = "Distribution of TIME_FOR_PASSION") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()

# Create a histogram for WEEKLY_MEDITATION
ggplot(selected_data, aes(x = WEEKLY_MEDITATION)) +
  geom_histogram(fill = "linen", color = "black", bins = 11) +
  labs(title = "Distribution of WEEKLY_MEDITATION") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()


# Create a histogram for AGE
ggplot(selected_data, aes(x = AGE)) +
  geom_bar(fill = "khaki", color = "black") +
  labs(title = "Distribution of AGE") +
  theme_minimal()


#score frequency test
# Specify the specific score to check for
specific_score <- 10

# Iterate over each column in the selected_data dataset
for (col in names(selected_data)) {
  # Check if the specific score exists in the column
  if (!(specific_score %in% selected_data[[col]])) {
    cat("Column:", col, "does not contain the specific score\n")
    next
  }
  
  # Calculate the proportion of the specific score in the column
  score_proportion <- sum(selected_data[[col]] == specific_score) / length(selected_data[[col]])
  
  # Print the column name and the proportion for all columns
  cat("Column:", col, "\tProportion:", score_proportion, "\n")
}





#average scores of variables per year
# Extract the year from the timestamp variable
selected_data$Year <- format(selected_data$Timestamp, "%Y")

# Create a new dataframe with the averages by year
averages_by_year <- selected_data %>%
  group_by(Year) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

# Print the new dataframe with averages by year
print(averages_by_year)


# FRUITS_VEGGIES
ggplot(averages_by_year, aes(x = Year, y = FRUITS_VEGGIES, group = 1)) +
  geom_point(color = "coral") +
  geom_line(color = "coral") +
  labs(title = "Yearly Changes in FRUITS_VEGGIES") +
  theme_minimal()


# DAILY_STRESS
ggplot(averages_by_year, aes(x = Year, y = DAILY_STRESS, group = 1)) +
  geom_point(color = "maroon") +
  geom_line(color = "maroon") +
  labs(title = "Yearly Changes in DAILY_STRESS") +
  theme_minimal()

# PLACES_VISITED
ggplot(averages_by_year, aes(x = Year, y = PLACES_VISITED, group = 1)) +
  geom_point(color = "steelblue") +
  geom_line(color = "steelblue") +
  labs(title = "Yearly Changes in PLACES_VISITED") +
  theme_minimal()


# CORE_CIRCLE
ggplot(averages_by_year, aes(x = Year, y = CORE_CIRCLE, group = 1)) +
  geom_point(color = "navy") +
  geom_line(color = "navy") +
  labs(title = "Yearly Changes in CORE_CIRCLE") +
  theme_minimal()

# SUPPORTING_OTHERS
ggplot(averages_by_year, aes(x = Year, y = SUPPORTING_OTHERS, group = 1)) +
  geom_point(color = "black") +
  geom_line(color = "gold") +
  labs(title = "Yearly Changes in SUPPORTING_OTHERS") +
  theme_minimal()

# SOCIAL_NETWORK
ggplot(averages_by_year, aes(x = Year, y = SOCIAL_NETWORK, group = 1)) +
  geom_point(color = "cyan") +
  geom_line(color = "cyan") +
  labs(title = "Yearly Changes in SOCIAL_NETWORK") +
  theme_minimal()


# ACHIEVEMENT
ggplot(averages_by_year, aes(x = Year, y = ACHIEVEMENT, group = 1)) +
  geom_point(color = "red") +
  geom_line(color = "red") +
  labs(title = "Yearly Changes in ACHIEVEMENT") +
  theme_minimal()

# DONATION
ggplot(averages_by_year, aes(x = Year, y = DONATION, group = 1)) +
  geom_point(color = "magenta") +
  geom_line(color = "magenta") +
  labs(title = "Yearly Changes in DONATION") +
  theme_minimal()

# BMI_RANGE
ggplot(averages_by_year, aes(x = Year, y = BMI_RANGE, group = 1)) +
  geom_point(color = "purple") +
  geom_line(color = "purple") +
  labs(title = "Yearly Changes in BMI_RANGE") +
  theme_minimal()

# TODO_COMPLETED
ggplot(averages_by_year, aes(x = Year, y = TODO_COMPLETED, group = 1)) +
  geom_point(color = "peru") +
  geom_line(color = "peru") +
  labs(title = "Yearly Changes in TODO_COMPLETED") +
  theme_minimal()


# FLOW
ggplot(averages_by_year, aes(x = Year, y = FLOW, group = 1)) +
  geom_point(color = "grey") +
  geom_line(color = "grey") +
  labs(title = "Yearly Changes in FLOW") +
  theme_minimal()

# DAILY_STEPS
ggplot(averages_by_year, aes(x = Year, y = DAILY_STEPS, group = 1)) +
  geom_point(color = "darksalmon") +
  geom_line(color = "darksalmon") +
  labs(title = "Yearly Changes in DAILY_STEPS") +
  theme_minimal()

# LIVE_VISION
ggplot(averages_by_year, aes(x = Year, y = LIVE_VISION, group = 1)) +
  geom_point(color = "chartreuse") +
  geom_line(color = "chartreuse") +
  labs(title = "Yearly Changes in LIVE_VISION") +
  theme_minimal()

# SLEEP_HOURS
ggplot(averages_by_year, aes(x = Year, y = SLEEP_HOURS, group = 1)) +
  geom_point(color = "darkgreen") +
  geom_line(color = "darkgreen") +
  labs(title = "Yearly Changes in SLEEP_HOURS") +
  theme_minimal()

# LOST_VACATION
ggplot(averages_by_year, aes(x = Year, y = LOST_VACATION, group = 1)) +
  geom_point(color = "black") +
  geom_line(color = "wheat") +
  labs(title = "Yearly Changes in LOST_VACATION") +
  theme_minimal()


# DAILY_SHOUTING
ggplot(averages_by_year, aes(x = Year, y = DAILY_SHOUTING, group = 1)) +
  geom_point(color = "darkorange") +
  geom_line(color = "darkorange") +
  labs(title = "Yearly Changes in DAILY_SHOUTING") +
  theme_minimal()

# SUFFICIENT_INCOME
ggplot(averages_by_year, aes(x = Year, y = SUFFICIENT_INCOME, group = 1)) +
  geom_point(color = "black") +
  geom_line(color = "lightcyan") +
  labs(title = "Yearly Changes in SUFFICIENT_INCOME") +
  theme_minimal()

# PERSONAL_AWARDS
ggplot(averages_by_year, aes(x = Year, y = PERSONAL_AWARDS, group = 1)) +
  geom_point(color = "lightpink") +
  geom_line(color = "lightpink") +
  labs(title = "Yearly Changes in PERSONAL_AWARDS") +
  theme_minimal()

# TIME_FOR_PASSION
ggplot(averages_by_year, aes(x = Year, y = TIME_FOR_PASSION, group = 1)) +
  geom_point(color = "black") +
  geom_line(color = "yellow") +
  labs(title = "Yearly Changes in TIME_FOR_PASSION") +
  theme_minimal()

# WEEKLY_MEDITATION
ggplot(averages_by_year, aes(x = Year, y = WEEKLY_MEDITATION, group = 1)) +
  geom_point(color = "black") +
  geom_line(color = "linen") +
  labs(title = "Yearly Changes in WEEKLY_MEDITATION") +
  theme_minimal()

# AGE
ggplot(averages_by_year, aes(x = Year, y = AGE, group = 1)) +
  geom_point(color = "black") +
  geom_line(color = "khaki") +
  labs(title = "Yearly Changes in AGE") +
  theme_minimal()




# Define the WLB score groups and breakpoints
score_groups <- c("Lowest", "Low", "Mid", "High", "Highest")
breakpoints <- quantile(selected_data$WORK_LIFE_BALANCE, probs = seq(0, 1, length.out = 6), na.rm = TRUE)

# Create empty data frames for each score group
df_lowest <- data.frame()
df_low <- data.frame()
df_mid <- data.frame()
df_high <- data.frame()
df_highest <- data.frame()

# Assign observations to the corresponding score group data frames
for (group in score_groups) {
  group_data <- selected_data[selected_data$WORK_LIFE_BALANCE >= breakpoints[which(score_groups == group)] &
                                selected_data$WORK_LIFE_BALANCE <= breakpoints[which(score_groups == group) + 1], ]
  
  # Assign to the corresponding data frame
  if (group == "Lowest") {
    df_lowest <- group_data
  } else if (group == "Low") {
    df_low <- group_data
  } else if (group == "Mid") {
    df_mid <- group_data
  } else if (group == "High") {
    df_high <- group_data
  } else if (group == "Highest") {
    df_highest <- group_data
  }
}

#check highest wlb group
highest_stats <- summary(df_highest)
print(highest_stats)

#means
numeric_vars <- sapply(df_highest, is.numeric)
means_highest <- colMeans(df_highest[, numeric_vars], na.rm = TRUE)
print("Means:")
print(means_highest)


numeric_vars <- sapply(df_highest, is.numeric)
medians_highest <- apply(df_highest[, numeric_vars], 2, median, na.rm = TRUE)
print("Medians:")
print(medians_highest)




#correlation matrix

# Select only the numeric columns from selected_data
numeric_data <- selected_data[, sapply(selected_data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data)

# Print the correlation matrix
print(cor_matrix)


#heatmap
# Select only numeric variables
numeric_vars <- sapply(selected_data, is.numeric)
numeric_data <- selected_data[, numeric_vars]

# Calculate correlation matrix
cor_matrix <- cor(numeric_data)

# Reshape correlation matrix for visualization
cor_matrix_melted <- reshape2::melt(cor_matrix)

# Create heatmap
ggplot(data = cor_matrix_melted, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



#inspect high and moderate correlations
# Set the correlation threshold
threshold <- 0.3

# Create a copy of the correlation matrix
cor_matrix_filtered <- cor_matrix

# Set correlations below the threshold to NA
cor_matrix_filtered[abs(cor_matrix_filtered) < threshold] <- NA

# Print the filtered correlation matrix
print(cor_matrix_filtered)





#Modelling
# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(selected_data$WORK_LIFE_BALANCE_SCORE, p = 0.8, list = FALSE)
train_data <- selected_data[train_index, ]
test_data <- selected_data[-train_index, ]



#LINEAR INDIVIDUAL regression
model_age <- lm(WORK_LIFE_BALANCE_SCORE ~ AGE, data = train_data)
summary(model_age)


model_gender <- lm(WORK_LIFE_BALANCE_SCORE ~ GENDER, data = train_data)
summary(model_gender)


model_fruits_veggies <- lm(WORK_LIFE_BALANCE_SCORE ~ FRUITS_VEGGIES, data = train_data)
summary(model_fruits_veggies)


model_bmi_range <- lm(WORK_LIFE_BALANCE_SCORE ~ BMI_RANGE, data = train_data)
summary(model_bmi_range)


model_daily_steps <- lm(WORK_LIFE_BALANCE_SCORE ~ DAILY_STEPS, data = train_data)
summary(model_daily_steps)


model_sleep_hours <- lm(WORK_LIFE_BALANCE_SCORE ~ SLEEP_HOURS, data = train_data)
summary(model_sleep_hours)


model_daily_stress <- lm(WORK_LIFE_BALANCE_SCORE ~ DAILY_STRESS, data = train_data)
summary(model_daily_stress)


model_flow <- lm(WORK_LIFE_BALANCE_SCORE ~ FLOW, data = train_data)
summary(model_flow)



model_weekly_meditation <- lm(WORK_LIFE_BALANCE_SCORE ~ WEEKLY_MEDITATION, data = train_data)
summary(model_weekly_meditation)

model_daily_shouting <- lm(WORK_LIFE_BALANCE_SCORE ~ DAILY_SHOUTING, data = train_data)
summary(model_daily_shouting)


model_live_vision <- lm(WORK_LIFE_BALANCE_SCORE ~ LIVE_VISION, data = train_data)
summary(model_live_vision)


model_social_network <- lm(WORK_LIFE_BALANCE_SCORE ~ SOCIAL_NETWORK, data = train_data)
summary(model_social_network)



model_supporting_others <- lm(WORK_LIFE_BALANCE_SCORE ~ SUPPORTING_OTHERS, data = train_data)
summary(model_supporting_others)


model_core_circle <- lm(WORK_LIFE_BALANCE_SCORE ~ CORE_CIRCLE, data = train_data)
summary(model_core_circle)

model_achievement <- lm(WORK_LIFE_BALANCE_SCORE ~ ACHIEVEMENT, data = train_data)
summary(model_achievement)


model_sufficient_income <- lm(WORK_LIFE_BALANCE_SCORE ~ SUFFICIENT_INCOME, data = train_data)
summary(model_sufficient_income)


model_personal_awards <- lm(WORK_LIFE_BALANCE_SCORE ~ PERSONAL_AWARDS, data = train_data)
summary(model_personal_awards)

model_time_for_passion <- lm(WORK_LIFE_BALANCE_SCORE ~ TIME_FOR_PASSION, data = train_data)
summary(model_time_for_passion)



model_todo_completed <- lm(WORK_LIFE_BALANCE_SCORE ~ TODO_COMPLETED, data = train_data)
summary(model_todo_completed)


model_donation <- lm(WORK_LIFE_BALANCE_SCORE ~ DONATION, data = train_data)
summary(model_donation)


model_lost_vacation <- lm(WORK_LIFE_BALANCE_SCORE ~ LOST_VACATION, data = train_data)
summary(model_lost_vacation)



#COLLECTIVE LINEAR REGRESSION
model <- lm(WORK_LIFE_BALANCE_SCORE ~ AGE + GENDER + FRUITS_VEGGIES + BMI_RANGE + DAILY_STEPS +
              SLEEP_HOURS + DAILY_STRESS + FLOW + WEEKLY_MEDITATION + DAILY_SHOUTING +
              LIVE_VISION + SOCIAL_NETWORK + SUPPORTING_OTHERS + CORE_CIRCLE + ACHIEVEMENT +
              SUFFICIENT_INCOME + PERSONAL_AWARDS + TIME_FOR_PASSION + TODO_COMPLETED +
              DONATION + LOST_VACATION, data = train_data)
summary(model)




#RANDOM FOREST
# Specify the column index of WLB in your dataset
response_col <- 24  

# Perform random forest regression
rf_model <- randomForest(selected_data[, -response_col],  
                         # Input features
                         selected_data[, response_col],    
                         # Response variable
                         ntree = 100,                      
                         # Number of trees in the forest
                         mtry = sqrt(ncol(selected_data) - 1),  
                         # Number of variables randomly sampled as candidates at each split
                         importance = TRUE)                




#Model Evaluation

# Evaluate the model
mse <- mean((predictions - test_data[, response_col])^2)  # Mean Squared Error
rmse <- sqrt(mse)  # Root Mean Squared Error
rsquared <- cor(predictions, test_data[, response_col])^2  # R-squared

# Print evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsquared, "\n")

# Print variable importance
var_importance <- importance(rf_model)
print(var_importance)



