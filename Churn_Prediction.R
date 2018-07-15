#To predict the churn rate of the customer

#install packages
install.packages("dplyr")

#load the library
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation

#set the working directory
setwd("E:\\IMS Course Content\\Course Content\\Data Science Term 1\\Capstone Project\\Data")

#read the data working directory
Tele.Train <- read.csv('Train_tele.csv', header = TRUE, stringsAsFactors = F)
Tele.Test <- read.csv('Test_tele.csv', header = TRUE, stringsAsFactors = F)

#------------------------------------------------------------------------------------------------------------------------------------------
#Exploratory Data Analysis

#Summary of the data
summary(Tele.Train)
summary(Tele.Test)

str(Tele.Train)
str(Tele.Test)

#Bind the data
Tele.Full <- bind_rows(Tele.Train, Tele.Test)
summary(Tele.Full)
str(Tele.Full)




#Filling the NA for Number Vmail Msgs with 0 as the customer doenst have a voice mail plan
Tele.Full[79,]
Tele.Full[79,]$number.vmail.messages <- 0

#Filling the Mean NA for Total Day Calls
boxplot(Tele.Full$total.day.calls)
ggplot(data = Tele.Train, aes(x = factor(churn), y = total.day.calls)) + geom_boxplot()
mean(Tele.Full$total.day.calls)
TotDayCall <- Tele.Full[c(-30,-90),8:9]
mean(TotDayCall$total.day.calls)
Tele.Full[c(30,90),8] <- 100

#Filling the Mean NA for Total Day Charge based on State
boxplot(Tele.Full$total.day.charge)
TotDayCharge <- Tele.Full[,c(1,9,17)]
mean(TotDayCharge$total.day.charge, na.rm = TRUE)

#Total day Charge for IN State
TotDayChargeIN <- Tele.Full[Tele.Full$state=='IN',c(1,9)]
mean(TotDayChargeIN$total.day.charge, na.rm = TRUE)
Tele.Full[11,9] <- 33.57

#Total day Charge for OK State
TotDayChargeOK <- Tele.Full[Tele.Full$state=='OK',c(1,9)]
mean(TotDayChargeOK$total.day.charge, na.rm = TRUE)
Tele.Full[35,9] <- 30.59

#Total day Charge for NM State
ggplot(data = Tele.Full[Tele.Full$state=='NM',], aes(x = total.day.charge)) + geom_histogram(bins = 15)
TotDayChargeNM <- Tele.Full[Tele.Full$state=='NM',c(1,9)]
mean(TotDayChargeNM$total.day.charge, na.rm = TRUE)
Tele.Full[73,9] <- 28.92

#Filling the NA for Total Eve 
TotEve <- Tele.Full[,c(1,10,11)]

#Filling NA for Total Eve Call for MA State
TotEveCallMA <- TotEve[TotEve$state=='MA',c(1,2)]
mean(TotEveCallMA$total.eve.calls, na.rm = TRUE)
mean(Tele.Full[Tele.Full$state=='MA',10], na.rm = TRUE)
Tele.Full[7,10]
Tele.Full[7,10] <- 98

#Filling NA for Total Eve Call for OR State
mean(Tele.Full[Tele.Full$state=='OR',10], na.rm = TRUE)
Tele.Full[46,10]
Tele.Full[46,10] <- 98

#Filling NA for Total Eve Charge for SC State
mean(Tele.Full[Tele.Full$state=='SC',11], na.rm = TRUE)
Tele.Full[24,11]
Tele.Full[24,11] <- 17.73

#Filling NA for Total Eve Charge for MD State
mean(Tele.Full[Tele.Full$state=='MD',11], na.rm = TRUE)
Tele.Full[42,11]
Tele.Full[42,11] <- 16.65

#Filling NA for Total Eve Charge for OR State
mean(Tele.Full[Tele.Full$state=='OR',11], na.rm = TRUE)
Tele.Full[82,11]
Tele.Full[82,11] <- 16.65

#Filling NA for Total Eve Charge for ID State
mean(Tele.Full[Tele.Full$state=='ID',11], na.rm = TRUE)
Tele.Full[100,11]
Tele.Full[100,11] <- 16.52

#Filling the NA rows for Total night based on state wise

#Filling the NA for Total Night Calls for IA State and  415 Area Code
mean(Tele.Full[Tele.Full$state=='IA' & Tele.Full$area.code==415,12], na.rm = TRUE)
Tele.Full[15,12]
Tele.Full[15,12] <- 97

#Filling the NA for Total Night Calls for WI State and  415 Area Code
mean(Tele.Full[Tele.Full$state=='WI' & Tele.Full$area.code==415,12], na.rm = TRUE)
Tele.Full[59,12]
Tele.Full[59,12] <- 101

#Filling the NA for Total Night Calls for LA State and  415 Area Code
mean(Tele.Full[Tele.Full$state=='LA' & Tele.Full$area.code==415,12], na.rm = TRUE)
Tele.Full[92,12]
Tele.Full[92,12] <- 98

#Filling NA for Total Night Charge 
mean(Tele.Full[Tele.Full$state=='IA' & Tele.Full$area.code==510,13], na.rm = TRUE)
Tele.Full[101,13]
Tele.Full[101,13] <- 9.63

#Filling the NA Values for Total International

#Filling the NA for Total International Call for SC State and Area Code
mean(Tele.Full[Tele.Full$state=='SC' & Tele.Full$area.code==415,14], na.rm = TRUE)
Tele.Full[24,14]
Tele.Full[24,14] <- 4

#Filling the NA for Total International Call for WI State and Area Code
mean(Tele.Full[Tele.Full$state=='WI' & Tele.Full$area.code==415,14], na.rm = TRUE)
Tele.Full[59,14]
Tele.Full[59,14] <- 4


#Filling the NA for Total International Charge for IN State and Area Code
mean(Tele.Full[Tele.Full$state=='IN' & Tele.Full$area.code==415,15], na.rm = TRUE)
Tele.Full[11,15]
Tele.Full[11,15] <- 2.77

#Filling the NA for Total International Charge for VT State and Area Code
mean(Tele.Full[Tele.Full$state=='VT' & Tele.Full$area.code==510,15], na.rm = TRUE)
Tele.Full[18,15]
Tele.Full[18,15] <- 2.37

#Filling the NA for Total International Charge for VT State and Area Code
mean(Tele.Full[Tele.Full$state=='WY' & Tele.Full$area.code==415,15], na.rm = TRUE)
Tele.Full[55,15]
Tele.Full[55,15] <- 2.75

#Filling the NA for Customer Service Call
mean(Tele.Full[Tele.Full$state=='NM' & Tele.Full$area.code==510,16], na.rm = TRUE)
Tele.Full[73,16]
Tele.Full[73,16] <- 1

#Converting Internation Plan to factors
Tele.Full$international.plan <- ifelse(Tele.Full$international.plan == 'yes', 1, 0)
#Tele.Full$international.plan <- as.factor(Tele.Full$international.plan)
Tele.Full$international.plan <- as.numeric(Tele.Full$international.plan)

#Converting Voice Mail Plan to factors
Tele.Full$voice.mail.plan <- ifelse(Tele.Full$voice.mail.plan == 'yes', 1, 0)
#Tele.Full$voice.mail.plan <- as.factor(Tele.Full$voice.mail.plan)
Tele.Full$voice.mail.plan <- as.numeric(Tele.Full$voice.mail.plan)

#Converting Churn to factors
Tele.Full$churn <- ifelse(Tele.Full$churn == TRUE, 1, 0)

#Converting State to factor and then numbers
Tele.Full$state <- as.factor(Tele.Full$state)
Tele.Full$state <- as.numeric(Tele.Full$state)

#Converting Area Code to factor and then numbers
Tele.Full$area.code <- as.factor(Tele.Full$area.code)
Tele.Full$area.code <- as.numeric(Tele.Full$area.code)

#converting Account length to numeric
Tele.Full$account.length <- as.numeric(Tele.Full$account.length)
#-------------------------------------------------------------------------------------------------------------------------------------
#TrainTele <- Tele.Full[Tele.Full$churn != 'NA',]
#Create the Training data
TrainTele <- Tele.Full[1:2850,]
summary(TrainTele)
#Convert the training churn to factor
#Tele.Train$churn <- as.factor(Tele.Train$churn)

#Create the Test dataset
TestTele <- Tele.Full[2851:3333,]

#------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#Logistic regression Approach to find the churn rate of the telecom Customer
install.packages('stringi')
install.packages('caret')
library('caret')
library('stringi')

#split the Training date for Train and Test
set.seed(1234)
inTrain = createDataPartition(TrainTele$churn, p=0.8, list = FALSE)
TrainingData = TrainTele[inTrain,]
TestingData = TrainTele[-inTrain,]

str(TrainingData)
str(TestingData)

#To find the Dimension of the Data
dim(TrainingData)
dim(TestingData)
summary(Tele.Full)

#To check for the Mulitcolinearity of the Variables in the Dataset
# multicollinearity a phenomenon in which one predictor variable in a multiple regression model can be linearly predicted from the others with a 
#substantial degree of accuracy.
#variance inflation factor (VIF) is the ratio of variance in a model with multiple terms, divided by the variance of a model with one term alone
library('usdm')
vifstep(Tele.Full[,c(-4,-17)], th=2)

#Akaike Information Criterion
#is an estimator of the relative quality of statistical models for a given set of data. Given a collection of models for the data, AIC estimates 
#the quality of each model, relative to each of the other models. Thus, AIC provides a means for model selection.

#To check for the correlation of the variable and the graph
#dependence or association is any statistical relationship, whether causal or not, between two random variables or bivariate data. 
#Correlation is any of a broad class of statistical relationships involving dependence, though in common usage it most often refers to how close 
#two variables are to having a linear relationship with each other
library('corrplot')
cr <- cor(Tele.Full[,c(-4,-17)])
corrplot(cr, type = "lower")

#Fitting the model with the VIF value below 2

fit0 <- glm(churn ~ state + account.length + area.code + international.plan +  number.vmail.messages + total.day.calls + total.day.charge +
              total.eve.calls + total.eve.charge + total.night.calls + total.night.charge + total.intl.calls + total.intl.charge + customer.service.calls, 
            data = TrainingData, family = binomial(link = "logit"))

summary(fit0)

#Select the model with the VIF variable for the next stepwise regression and get the minimum AIC Value
library('MASS')
step = stepAIC(fit0, direction = "both")

#Optimizing the model
#Null Deviance is the deviance of the actual value of the dataset - not using the independent variable only using the intercept - should be the same for all the model

#Residual Deviance - including the independent variable the value is less showing the deviance from the dependent variable - should be less

#AIC - should be less

#Degree of Freedom - The number of independent ways by which a dynamic system can move, without violating any constraint imposed on it, is called 
#number of degrees of freedom. In other words, the number of degrees of freedom can be defined as the minimum number of independent coordinates that 
#can specify the position of the system completely.

#Fitting the model with the lowest AIC value
fit1 <- glm(churn ~ international.plan + number.vmail.messages + total.day.charge + 
              total.eve.charge + total.night.charge + total.intl.calls + 
              total.intl.charge + customer.service.calls, data = TrainingData, family = binomial(link = "logit"))
summary(fit1)
#Fit1 is the best fit with the AIC score of 1529.3

fit2 <- update(fit1, .~. -total.intl.calls, data = TrainingData)
Summary(fit2)


#Predicting the Model with the Test Dataset
#To get the probability value of the rtesponse
Pred <- predict(fit1, newdata=TestingData[,-17], type = "response")
Pred
View(Pred)

#Converting the Predicted to 0 and 1 with .5 and the threshold
Preds <- ifelse(Pred<0.5, 0, 1)
#Confusion MAtrix is a special kind of contingency table, with two dimensions ("actual" and "predicted"), and identical sets of "classes" in both 
#dimensions (each combination of dimension and class is a variable in the contingency table).
#TN FP
#FN TP
confusionMatrix(table(TestingData$churn,Preds,dnn=list('Actual', 'Predicted')))
#Predicted with 89% Accuracy
((486+21)/(486+10+53+21))*100

#-------------------------------------------------------------------------
#ROC Curve to find the threshold
#Receiver Operating Characteristic(ROC) Curve
#The ROC curve is created by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings. 
res <- predict(fit1, newdata=TrainingData, type = "response")

install.packages('ROCR')
library(ROCR)

ROCRPred <- prediction(res,TrainingData$churn)
#Performance of the ROC curve from the TPR and FPR
ROCRPref <- performance(ROCRPred, "tpr", "fpr")

#plot the ROCR Graph to find the TPR and FPR
plot(ROCRPref, colorize = TRUE, print.cutoffs.at=seq(0.1, by=0.1))

PredROCR1 <- ifelse(Pred<0.5, 0, 1)
confusionMatrix(table(TestingData$churn,PredROCR1,dnn=list('Actual', 'Predicted')))

#Area under the ROC Curve
install.packages("InformationValue")
library(InformationValue)
plotROC(actuals=TrainingData$churn,predictedScores=as.numeric(fitted(fit1)))

#-------------------------------------------------------------------------------
## chart measures the performance of classification  models :- Kolmogorov Smirnov  test
ks_plot(actuals=TrainingData$churn,predictedScores=as.numeric(fitted(fit1)))

### Kolmogorov Smirnov Statistic :- Higher the value , better the model 
ks_stat(actuals=TrainingData$churn,predictedScores=as.numeric(fitted(fit1)))
#model is not that efficient in capturing the responders
#-------------------------------------------------------------------------
#Goodness of Fit – Hosmer Lemeshow Test
#The test assesses whether or not the observed event rates match expected event rates in subgroups of the model population.
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(TrainingData$churn,fitted(fit1),g=10)

#----------------------------------------------------------------------------
#Wald Test 
# if explanatory variables in a model are significant.
install.packages("survey")
library(survey)
regTermTest(fit1,"international.plan")

#-----------------------------------------------------------------------------
## Pseudo R2 statistic  :- Mc Fadden test

install.packages("pscl")
library(pscl)
pR2(fit1)

#----------------------------------------------------------------------------
#Plotting the Grapoh for logistic Regression
library(ggplot2)
ggplot(TrainingData, aes(state,churn)) + geom_point() + geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))
#--------------------------------------------------------------------------

#Predicting the value for the test data
Tele.test1 <- TestTele[,-17]
summary(Tele.test1)

PredTest <- predict(fit1, newdata=Tele.test1, type = "response")
PredTest

#Converting the Predicted to 0 and 1 with .5 and the threshold
churn <- ifelse(PredTest<0.5, 0, 1)
churn

TelecomPredicted <- cbind(Tele.test1,churn)
write.csv(TelecomPredicted, file = 'Telecom Churn Logistic Regression.csv', row.names = F)

#------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#Naive Bayes Algorithm Approach to find the churn rate of the telecom Customer
install.packages(e1071)
library('e1071')

tele_nb <- naiveBayes(churn ~ state + account.length + area.code + international.plan +  number.vmail.messages + total.day.calls + total.day.charge +
                        total.eve.calls + total.eve.charge + total.night.calls + total.night.charge + total.intl.calls + total.intl.charge + customer.service.calls, 
                      data = TrainingData)
tele_nb
