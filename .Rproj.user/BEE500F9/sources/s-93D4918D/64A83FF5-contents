library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(ggplot2)
library(randomForest)
library(MASS)

data <- read.csv("data.csv",sep=",")

#Regression logistique

data <- na.omit(data)

#Choix des prédicteurs

data["y"][data["y"] == "yes"] <- "1"
data["y"][data["y"] == "no"] <- "0"

dim(test)

data["y"]=transform(data["y"], y = as.numeric(y))

sapply(data["y"], mode)

data2 =data[,c("age","duration","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y")]





pred <- vector(length = 1000)
pred



test3 =test2[,c("age","duration","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y")]
test3


matrice_cor = cor(data2)
matrice_cor

model_lda <- glm(y ~., family = binomial, data= data2)
prediction <- predict(model_lda, test3, type="response")


model_qda <- qda(y~., data = data2)
pred.qda <- predict(model_qda, newdata = test3, type="response")


mc <- table(data2, test3)
accuracy <- sum(diag(mc)) / sum(mc)
print(paste('Accuracy for test', accuracy))



#Decision trees

tree = rpart(y~. , data = data)
plot(tree)
text(tree, pretty = 0)
title(main = "Regression Tree")

rpart.plot(tree)
prp(tree)

summary(tree)
printcp(tree)
plotcp(tree)

rmse = function(actual, predicted) 
{
  sqrt(mean((actual - predicted) ^ 2))
}


tree_pred = predict(tree, newdata = data)
pred = tree_pred

rmse(pred, data$y)



tree_bagging = randomForest(y ~ ., data = data, mtry = 13,
                              importance = TRUE, ntrees = 2)
tree_bagging


tree_bagging_pred = predict(tree_bagging, newdata = data)
rmse(tree_bagging_pred, data$y)

tree_forest = randomForest(y ~ ., data = data, mtry = 4,
                             importance = TRUE, ntrees = 1)
tree_forest

tree_forest_pred = predict(tree_forest, newdata = test2)
rmse(tree_forest_pred, tree_test$y)

importance(tree_forest, type = 1)

varImpPlot(tree_forest, type = 1)

par(mfrow=c(2,2))
plot(tree_pred, test2$y, 
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Single Tree, Test Data",
     col = "#cd0050", pch = 20)
grid()
abline(0, 1, col = "dodgerblue", lwd = 2)

write.csv(test2,"C:\\Users\\hamza\\Desktop\\test.csv", row.names = FALSE)

rf <- randomForest(factor(y) ~., data= data, proximity=TRUE)

print(rf)



to_be_submitted = data.frame(id=rownames(test), y=pred)
write.csv(to_be_submitted , file = "to_be_submitted.csv", row.names = F)