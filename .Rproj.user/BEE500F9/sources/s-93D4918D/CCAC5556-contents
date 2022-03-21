# We have applied random forest classifier on the provided data set. I will explain each and every step one by one.
#Indeed, it is a supervised machine learning, so we could use 3 methods :
# - K-means
# - Regression logistic
# - Decision trees
# The data could have 2 labels so a regression logistic with a QDA approach could have a good accuracy
# Thus, the data set is not huge (less than 100 000 rows), so I decision trees methods with a random forest approach
# could significantly increase the accuracy.

# Importing required libraries.
library(caret)
library(ggplot2)
library(randomForest)

# Read the data set.

data <- read.csv('data.csv')

dim(data)

#Dim allow us to see the dimensions of the data set. It contains 19594 rows and 21 columns.
str(data)
summary(data)

#Wen can have some information thanks to str() and summary() functions.


# Drop all null values from the data set.
data <- na.omit(data)


# Check if null values are removed. If the sum is 0 then that means no null values are present.
sum(is.na(data))


# Divide the data into training and testing set. A split of 70% and 30% is used for training and testing respectively.
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Initiate a random forest classifier. As the target variable is a binary variable hence we will factor it. All other features will be used to train the model.
rf <- randomForest(factor(y) ~., data=train, proximity=TRUE)

# Printing a summary of the trained model
print(rf)

# Perform predictions on the training set and print confusion matrix to see True positives, flase positives, true negatives, false negatives. We can see that accuracy score is 99.79% on training set.
p1 <- predict(rf, train)
confusionMatrix(p1, factor(train$y))

# Use 30% test data for predictions.
y <- predict(rf, test)

# Calculate a confusion matrix and we can see that the accuracy on the test set is 91.38% which is good.
confusionMatrix(y, factor(test$y))

# Plot the most important features of the data set.
varImpPlot(rf)

# Read the testing data set to perform predictions.

test_data <- read.csv('test.csv')
head(test_data)

# Perform prediction on given test sets.
y <- predict(rf, test_data)

# Display classification results.
table(y)

to_be_submitted = data.frame(id=rownames(test_data), y)
write.csv(to_be_submitted , file = "BENYEMNA Hamza & BITAR Aref & AZZOUZI Hajar - DIA2 groupe13 - predictions .csv", row.names = F)



