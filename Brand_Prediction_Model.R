#install.packages("caret")
library(readr)
library(dplyr)
library(C50)
library(caret)
library(data.table)
library(ggplot2)

#Importing the Data
CompleteResponses <- read_csv("~/Desktop/ubiqum/Data Science projects/DS2/Task#2/CompleteResponses.csv")
View(CompleteResponses)
summary(CompleteResponses)

SurveyIncomplete <- read_csv("~/Desktop/ubiqum/Data Science projects/DS2/Task#2/SurveyIncomplete.csv")
View(SurveyIncomplete)

#Pre-proccessing the Data
CompleteResponses$brand <- as.factor(CompleteResponses$brand)
CompleteResponses$elevel <- as.factor(CompleteResponses$elevel)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
View(CompleteResponses)

SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)
SurveyIncomplete$elevel <- as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)
View(SurveyIncomplete)

#Correlation matrix

#descrCor <- cor(CompleteResponses)
#plot(descrCor)
#highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
#plot(highCorr)
#descrCor <- cor(CompleteResponses)
#summary(descrCor[upper.tri(descrCor)])

#Working pipeline and Data splitting
set.seed(123)
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
str(inTraining)
training <- CompleteResponses[inTraining,]
nrow(training)
testing <- CompleteResponses[-inTraining,]
nrow(testing)

#trainControl() Cross Validation
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Model "C5.0"
system.time(rfFit_c50 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, metric = "Accuracy", tuneLength = 2))
rfFit_c50
plot(rfFit_c50)

#train Model "rf"
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))
system.time(rfFit_RF <- train(brand~., data = training, method = "rf", trControl=fitControl,tuneGrid=rfGrid))
rfFit_RF

#prediction based on "C5.0"
Predictionc50 <- predict(rfFit_c50,testing)
Predictionc50
View(Predictionc50)

#prediction based on "rf"
PredictionRF <- predict(rfFit_RF,testing)
PredictionRF
View(PredictionRF)

#Prediction brand for Survey Incomplete
Prediction <- predict(rfFit_c50,SurveyIncomplete)
View(Prediction)
postResample(Prediction,Predictionc50)

#Setting names to 0 = Acer and 1 = Sony
named <- factor(Prediction,
                levels = c("0","1"),
                labels =c("Acer","Sony"))
View(named)
summary(named)
# writing csv file with predictions
write.csv(data.frame(named), "prediction.csv")


