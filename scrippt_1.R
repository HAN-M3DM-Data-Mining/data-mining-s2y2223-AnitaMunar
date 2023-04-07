# Step 1: Load the necessary packages
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
library(class)
library(ggplot2)
library(caret)
library(lattice)
library(tidyverse)
library(tm)
library(slam)
library(e1071)
library(ISLR)
library(magrittr)
library(dplyr)
library(fastDummies)
library(readr)
library(psych)
library(effsize)
library(reshape2)
library(grid)
library(gridExtra)
library(VennDiagram)
library(venn)


#load the data
rawDF <- read.csv("https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/data-mining-s2y2223-AnitaMunar/master/1652126alienation_data%20(1).csv", header = TRUE)
alien_data <- rawDF
#check mean, median, range and st dev
mean_alienation <- mean(alien_data$alienation)
median_alienation <- median(alien_data$alienation)
score_range <- max(alien_data$alienation) - min(alien_data$alienation)
ggplot(alien_data, aes(y = alienation)) +
  geom_boxplot() +
  geom_hline(yintercept = mean_alienation, color = "red", linetype = "dashed") +
  geom_hline(yintercept = median_alienation, color = "blue", linetype = "dashed") +
  labs(title = "Distribution of Alienation Scores",
       y = "Alienation Score",
       x = "") +
  theme_classic()
std_dev <- sd(alien_data$alienation)
#summary
summary(alien_data$alienation)

#histgram of alienation
ggplot(alien_data, aes(x=alienation)) +
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlab("Alienation Scores") +
  ylab("Frequency") +
  ggtitle("Histogram of Alienation")

# Conduct one-sample t-test
t_test <- t.test(alien_data$alienation, mu = mean(alien_data$alienation))

# Print results
cat("p-value =", round(t_test$p.value, 3), "\n")
cat("95% Confidence Interval:", t_test$conf.int[1], "-", t_test$conf.int[2], "\n")


#gender divide
#mean male
mean_male_alienation <- alien_data %>% filter(male==1) %>% summarise(mean(alienation)) %>% pull()
#mean female
mean_female_alienation <- alien_data %>% filter(male==0) %>% summarise(mean(alienation)) %>% pull()
#median male
median_male_alienation <- alien_data %>% filter(male==1) %>% summarise(median(alienation)) %>% pull()
#median female
median_female_alienation <- alien_data %>% filter(male==0) %>% summarise(median(alienation)) %>% pull()

#range male
range_score_male <- max(alien_data %>% filter(male==1) %>% pull(alienation)) - min(alien_data %>% filter(male==1) %>% pull(alienation))
cat("Range of 'score' column on male respondents: ", range_score_male, "\n")
#range female
range_score_female <- max(alien_data %>% filter(male==0) %>% pull(alienation)) - min(alien_data %>% filter(male==0) %>% pull(alienation))
cat("Range of 'score' column on female respondents: ", range_score_female, "\n")

#st dev male
std_male_alienation <- sd(alien_data %>% filter(male==1) %>% pull(alienation))
#st dev female
std_female_alienation <- sd(alien_data %>% filter(male==0) %>% pull(alienation))

#male histogram
table(alien_data$alienation[alien_data$male == 1])

ggplot(data = alien_data %>% filter(male == 1), aes(x = alienation)) +
  geom_histogram(color = "blue", fill = "white", boundary = 0.5, bins = 10) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 25, 1), minor_breaks = NULL) +
  labs(x = "Alienation Scores", y = "Respondents", 
       title = "Histogram of Alienation for Male Respondents")


#female histogram
table(alien_data$alienation[alien_data$male == 0])

ggplot(data = alien_data %>% filter(male == 0), aes(x = alienation)) +
  geom_histogram(color = "red", fill = "white", boundary = 0.5, bins = 10) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 25, 1), minor_breaks = NULL) +
  labs(x = "Alienation Scores", y = "Respondents", 
       title = "Histogram of Alienation for Female Respondents")

# Perform ANOVA
anova2 <- aov(alienation ~ male, data = alien_data)

# Print ANOVA table
summary(anova2)



#income
#the mean
mean_income <- mean(alien_data$income)
summary(mean_income)

alien_data$income <- round(alien_data$income)
alien_data$income <- as.numeric(alien_data$income)
income <- na.omit(alien_data$income)
mean(income)
#the median
median_income <- median(income)
#the standard deviation
sd_income <- sd(income)
#range
range_income <- range(income)


#remove NA
alien_data <- na.omit(alien_data)
# Split income into 5 segments with equal width
income_groups <- cut(alien_data$income, breaks = 5, labels = FALSE)

# Add income groups as a new column to the data frame
alien_data$income_group <- income_groups

# Plot histograms of alienation scores for each income group
ggplot(data = alien_data, aes(x = alienation)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  facet_wrap(~ income_group) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 25, 1), minor_breaks = NULL) +
  labs(x = "Alienation Scores", y = "Respondents", 
       title = "Histogram of Alienation by Income Group")



# Perform ANOVA on income
anova <- aov(alienation ~ income_group, data = alien_data)

# Print ANOVA table
summary(anova)

#regression income
model_income <- lm(alienation ~ income, data = alien_data)
summary(model_income)
#regression gender
model_gender <- lm(alienation ~ male, data = alien_data)
summary(model_gender)

#validity check gender distribution

# Create a data frame with counts of males and females
gender_count <- data.frame(
  Gender = c("Male", "Female"),
  Count = c(sum(alien_data$male == 1), sum(alien_data$male == 0))
)

# Create the bar plot
ggplot(gender_count, aes(x = Gender, y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("pink", "blue")) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

income_by_gender <- table(alien_data$income_group, alien_data$male)
income_by_gender

#visual for distribution of income per gender
# Create a data frame with income and gender variables
income_by_gender_df <- alien_data %>%
  filter(!is.na(income)) %>%
  group_by(male, income_group) %>%
  summarise(Freq = n()) %>%
  ungroup()

# Convert the income group variable to a factor with labels
income_by_gender_df$income_group <- factor(income_by_gender_df$income_group, labels = c("Lowest", "Low", "Medium", "High", "Highest"))

# Plot the income distribution by gender
ggplot(income_by_gender_df, aes(fill = income_group, y = Freq, x = factor(male, levels = c(0, 1), labels = c("Female", "Male")))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Gender", y = "Frequency", title = "Income Distribution by Gender") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12),
        legend.position = "right") +
  guides(fill = guide_legend(reverse = TRUE))



#consult
# Create a frequency table for the consult column
consult_freq <- table(alien_data$consult)

# Print the frequency table
consult_freq
# Plot the frequency table as a bar chart
barplot(consult_freq, main = "Consultation Help", xlab = "Consultation (0 = No, 1 = Yes)", ylab = "Frequency")
print(barplot)
# Subset the rows where consult is 1
consult_yes <- subset(alien_data, consult == 1)

# Select the male column and any other columns you want to see
consult_yes_gender <- consult_yes[, c("male")]

# Print the resulting data frame
consult_yes_gender
# Create a summary table showing the count of males and females who sought consultation
consult_summary <- table(consult_yes$male)
print(consult_summary)
# Create a bar chart to show the count of males and females who sought consultation
ggplot(consult_yes, aes(x = factor(male, levels = c(0, 1)), fill = factor(male, levels = c(0, 1)))) +
  geom_bar() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Female", "Male")) +
  labs(title = "Consultation by Gender", x = "Gender", y = "Frequency") +
  theme_bw()


#venn
# Create the sets for each group
set1 <- alien_data$male[alien_data$male == 1]
set2 <- alien_data$male[alien_data$male == 0]
set3 <- alien_data$consult[alien_data$consult == 1]

# Count the number of men and women seeking consultation
n_male_consult <- sum(alien_data$male == 1 & alien_data$consult == 1)
n_female_consult <- sum(alien_data$male == 0 & alien_data$consult == 1)

# Create the Venn diagram
vd <- draw.triple.venn(
  area1 = length(set1),
  area2 = length(set2),
  area3 = length(set3),
  n12 = length(intersect(set1, set2)),
  n13 = length(intersect(set1, set3)),
  n23 = length(intersect(set2, set3)),
  n123 = length(intersect(intersect(set1, set2), set3)),
  category = c("Male", "Female", "Consult"),
  fill = c("pink", "skyblue", "purple"),
  lty = "blank",
  cex = 1.5,
  cat.cex = 1.5,
  cat.col = c("black", "black", "black"),
  margin = 0.05,
  scaled = FALSE,
  euler.d = FALSE,
  alpha = 0.5,
  main = "Venn Diagram of Set 1, Set 2, and Set 3"
)

# Display the Venn diagram
grid.newpage()
grid.draw(vd)

# Add labels to the diagram
grid.text(n_male_consult, x = 0.27, y = 0.55, gp = gpar(fontsize = 16))
grid.text(n_female_consult, x = 0.73, y = 0.55, gp = gpar(fontsize = 16))

grid.text(paste("Men seeking consultation:", n_male_consult, "\nWomen seeking consultation:", n_female_consult), x = 0.5, y = 0.25, gp = gpar(fontsize = 16))



#currency
exchange_rate <- 0.9157
alien_data$income_eur <- alien_data$income * exchange_rate

alien_data$income_eur <- as.integer(alien_data$income_eur)

# Calculate mean and standard deviation of income_usd
mean_usd <- mean(alien_data$income)
sd_usd <- sd(alien_data$income)

# Calculate mean and standard deviation of income_eur
mean_eur <- mean(alien_data$income_eur)
sd_eur <- sd(alien_data$income_eur)

# Combine the results into a table
stats <- rbind(data.frame(Measure = "Mean", USD = mean_usd, EUR = mean_eur),
               data.frame(Measure = "Standard Deviation", USD = sd_usd, EUR = sd_eur))

# Print the table
print(stats)

# Create a data frame with income and income_eur
income_df <- data.frame(income_usd = alien_data$income,
                        income_eur = round(alien_data$income_eur))

# Create a box plot with income and income_eur
ggplot(melt(income_df), aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "", y = "Income (EUR)", title = "Comparison of USD and EUR") +
  scale_x_discrete(labels = c("Income (USD)", "Income (EUR)")) +
  theme_bw()

# create histogram for income in USD
p1 <- ggplot(alien_data, aes(x = income)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "skyblue") +
  ggtitle("Income Distribution in USD")

# create histogram for income in EUR
p2 <- ggplot(alien_data, aes(x = income_eur)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "orange") +
  ggtitle("Income Distribution in EUR")

# arrange plots side by side
grid.arrange(p1, p2, ncol = 2)




#centered income
alien_data$centered_income <- alien_data$income - mean(alien_data$income)
centered_income <- alien_data$income - mean(alien_data$income)
mean_centered_income <- mean(centered_income)
sd_centered_income <- sd(centered_income)

cat("Mean of centered income: ", mean_centered_income, "\n")
cat("Standard deviation of centered income: ", sd_centered_income, "\n")

# Boxplot of centered income
ggplot(alien_data, aes(y = centered_income)) +
  geom_boxplot() +
  labs(title = "Centered Income Boxplot", y = "Centered Income")

# Histogram of centered income
ggplot(alien_data, aes(x = centered_income)) +
  geom_histogram(binwidth = 1000) +
  labs(title = "Centered Income Histogram", x = "Centered Income", y = "Frequency")

#compare
# Create a data frame for visualization
df <- data.frame(
  variable = c(rep("Income", nrow(alien_data)), rep("Centered Income", nrow(alien_data))),
  value = c(alien_data$income, centered_income)
)
# Create a box plot
ggplot(df, aes(x = variable, y = value)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Centered Income", "Income")) +
  labs(title = "Comparison of Income and Centered Income",
       y = "Income in USD") +
  theme_bw()



