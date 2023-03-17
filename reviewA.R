library(tidyverse)
library(googlesheets4)
library(class)
library(caret)
library(creditmodel)
library(cvms)


## Business Understanding

Diabetes prevents your body to turn food into energy. This can cause serious health problems, such as heart disease, vision loss, and kidney disease [*(What Is Diabetes?, 2022)*]{style="color:blue"}. Diabetes can be treated and be recognized early before it turns into an urgent case by doing some testing (blood glucose). Diabetes detection with early symptoms can reduce the death rate and prevent it from getting worse before it is too late. With machine learning algorithms, it could improve the accuracy and effectiveness of detection.

## Data Understanding

The dataset is orginially retrieved from National Institute of Diabetes and Digestive and Kidney Diseases.With the result of diagnostic measurements, a patient is predicted to have diabetes or not. Donor database is Vincent Sigillito from RMI Group Leader Applied Physics Laboratory The Johns Hopkins University Johns Hopkins Road Laurel. The dataset used is downloaded from open source [*(Diabetes Dataset, 2020)*]{style="color:blue"}. The instances are females at least 21 years old of Pima Indian heritage.\
The dataset is stored in a Github repo and cloned into local repo. It is in .csv file. To read this, we use `readr` package.

```{r message=FALSE}
rawDF <- read_csv("https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/data-mining-s2y2223-lynnevo170701/master/datasets/KNN-diabetes.csv")
```

By using `str()` function, we can have first sense of data.

```{r}
str(rawDF)
```

The dimension of dataset is 768 x 9, which means that the data has 768 observations and 9 variables:
  
  \- `Pregnancies`: Number of times pregnant

\- `Glucose`: Plasma glucose concentration a 2 hours in an oral glucose tolerance test - `BloodPressure`: Diastolic blood pressure (mm Hg)

\- `SkinThickness`: Triceps skin fold thickness (mm)

\- `Insulin`: 2-Hour serum insulin (mu U/ml)

\- `BMI`: Body mass index (weight in kg/(height in m)\^2)

\- `DiabetesPedigreeFunction`: Diabetes pedigree function

\- `Age`: Age (years)

\- `Outcome`: Class variable (0 or 1)

The data type of all variables are `num`. We can use `summary` function to see the summary information of each variable and we can check if each has Nas values, which we need to clean.


summary(rawDF)


Then we should check if the data has duplicated values

```{r}
dup_check <- sum(duplicated(rawDF))
dup_check
```

## Data Preparation

The `outcome` variable has the values we need to predict "0 - negative" and "1 - positive".We need to set level and label for it to easily sort or call it later.

```{r}
rawDF$Outcome <- factor(rawDF$Outcome, levels = c(0, 1), labels = c("Negative", "Positive")) %>%  relevel("Positive")
```

We can clearly see that the variables have different ranges. We need to normalize it before splitting into train and test set.


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
cleanDF <- rawDF 
cleanDF[1:8] <- normalize(cleanDF[1:(ncol(rawDF)-1)])
summary(cleanDF)

##REVIEW
#in line 74, the cleaning command is missing a comma between the cleandDF[,1 in both assigned variable and command
#therefore, correct code there is
cleanDF[,1:8] <- normalize(cleanDF[,1:(ncol(rawDF)-1)])
#the comma in R can be trailing (for columns) or leading (for rowns) comma. 
#The "[]" is for subsets.
#example from Anita using https://rpubs.com/tomhopper/brackets 
#my_df[1:3] (no comma) will subset my_df, returning the first three columns as a data frame.
#my_df[1:3, ] (with comma, numbers to left of the comma) will subset my_df and return the first three rows as a data frame.
#my_df[, 1:3] (with comma, numbers to right of the comma) will subset my_df and return the first three columns as a data frame, the same as my_df[1:3].
#therefore, the correct code returns the first 8 columns of the dataframe.


After running `summary`, we see that normalization has set all the parameters into the same range Now, we split our data into training and test sets by using `train_test_split` from package `creditmodel`

```{r}
test_train <- train_test_split(cleanDF)
test <- test_train[[1]]
train <- test_train[[2]]
```

These two data frames inclue `outcome`. But we only need parameters without `outcome` variable to set as `train_feat` or \`test_feat, so we will exclude that variable.

```{r}
test_feat <- test[-9]
train_feat <- train[-9]
```

Creating labels for training and test set

```{r}
test_labels <- test[, 9]
train_labels <- train[,9]
```

## Modeling

I will use `knn` from package `class`. It takes the set with `train_feat` and the set with `train_labels`. The trained model is applied to the set with `test_feat` and it will return a set of predictions. First, I find the optimal `k` value to make the knn has the lowest error rate by creating function `error_k` with the argument `k`. `error_k` will return a data frame of `error_rate` values corresponding to `k` values. Using `relevel` function to prevent the warning message " Levels are not in the same order for reference and data. Refactoring data to match" when running `confusionMatrix`.


error_k <- function(k) {
  error_rate <- data.frame()
  for (i in k) {
    knn <- knn(train = as.matrix(train_feat), test = as.matrix(test_feat), cl = as.matrix(train_labels), k = i)
    knn <- relevel(knn, "Positive")
    matrix <- confusionMatrix(knn, test_labels[[1]], positive = NULL, dnn = c("Prediction", "True"))
    error_rate <- rbind(error_rate, 1-matrix$overall[["Accuracy"]])
  }
  error_k <- data.frame(k, error_rate)
  colnames <- c("k", "error_rate")
  error_k
}

##REVIEW:
#in line 125, the colnames should be expressed for the error_k function. 
#the colnames() code can change the column names in the dataframe or
#matrix-like object with at least 2 dimensions it is assigned to
#we always have to specify for which
#therefore, the correct code:
colnames(error_k) <- c("k", "error_rate")
#now, the 2 columns of the eror_k is called "k" and "error rate" respectively




Now, I will plot the error rate with k from 1 to 40.


plot <- error_k(1:40)
ggplot(plot, aes(x = k, y = error_rate )) +
  geom_point(colour = "red", size = 3, alpha = 0.5) +
  geom_line(colour = "blue", size = 0.7, linetype = 2) +
  labs(title = "Error Rate vs k value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Error Rate")


As shown in the graph, k = 10 will return the lowest error rate. 10 will be used as the amount of nearest neighbours `k` in `knn` function below.

```{r}
cleanDF_test_pred <- knn(train = as.matrix(train_feat), test = as.matrix(test_feat), cl = as.matrix(train_labels), k = 10)
cleanDF_test_pred <- relevel(cleanDF_test_pred, "Positive")
head(cleanDF_test_pred)
```

We have predicted labels. We will compare them with actual labels.

```{r}
cf <- confusionMatrix(cleanDF_test_pred, test_labels[[1]], positive = NULL, dnn = c("Prediction", "True"))
cf
```

To visualize the confusion matrix, I will use `plot_confusion_matrix` function from package `cvms`. First I need to convert matrix type of `cf` to tibble because this function only accepts data.frame.

```{r}
table <- as_tibble(cf$table)
plot_confusion_matrix(table, target_col = "True", prediction_col = "Prediction", counts_col = )
```

##REVIEW
#in line 170, the counts_col value is missing. 
#correct code:
plot_confusion_matrix(table, target_col = "True", prediction_col = "Prediction", counts_col = "n")



## Evaluation and Deployment

Overall, the accuracy percentage is 72.1%. 51.7% of test feature has True Negative and 20.4% is True Positive. 7.8% is False positive and 20% is False negative.

Of the 230 cases, 93 are positive (47 TPs and 46 FNs) and 137 are negative (119 TNs and 18 FPs).

Of 137 negative diabetes, 86.9% are correctly predicted. This is a good result.

#REVIEW:
#after I run the code, in my confusion matrix out of the 137 negative diabetes, 92% is actually predicted to be negative correctly.
#therefore I am not sure why the author (Linh) has the number 86.9%

However, of 93 positive cases, only 50.5% are correctly identified. Half of the cases go wrong. This is a bad outcome.
##REVIEW
#for me, this number is 45.2%, even less accuracy.
**Precision** in this model is 72.3%. Precision tells us that when it predicts someone is positive, 72.3% of the time is correct.
##REVIEW
#"Accuracy" is the correct name, not precision.
**Recall** in this model is 50.5%. Recall is quite high and Precision is quite low. This can be improved by increasing the classification threshold.

## References

-   What is Diabetes? (2022, July 7). Centers for Disease Control and Prevention. <https://www.cdc.gov/diabetes/basics/diabetes.html>
  -   Diabetes Dataset. (2020, August 5). Kaggle. <https://www.kaggle.com/datasets/mathchi/diabetes-data-set>