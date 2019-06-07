###################################################
# Loading the needed libraries                    #    
###################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR")
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(rpart)
library(ROCR)

##################################################
# Read Data
##################################################

adult_full <- read_csv("data/adult.csv")

str(adult_full)

head(adult_full)


#################################################
# Data Wrangling
#################################################


#A quick look at the str shows a few problems that we need to address with the data.

#There are some "?" in the data. Replace them with NA
for (i in 1:ncol(adult_full)) {adult_full[,i][adult_full[,i] == '?',] = NA}


#where are the missing values
sapply(adult_full, FUN = function(adult_full_cols) {
  table(is.na(adult_full_cols))
})
# NA's are in workclass, occupation and country as observed from the occurance of TRUE. 

# Now let's see if the missing data in those columns are related. 
table(which(is.na(adult_full['workclass'])) == which(is.na(adult_full['occupation'])))
table(which(is.na(adult_full['workclass'])) %in% which(is.na(adult_full['occupation'])))
#So whenever we have a NA in the workclass, we have NA in occupation most of the time.


#Now let's see the country of those NA's in the workingclass 
table(adult_full[is.na(adult_full['workclass']),]$native.country)
#Most of them are in the US.

#What about their income? 
table(adult_full[is.na(adult_full['workclass']),]$income)
#Most of them have income less than 50K

#Missing value treatment

#adult_full[adult_full == "?"] <- NA
sum(is.na(adult_full))

#Remove missing values
adult_full <- na.omit(adult_full)
adult_full <- data.frame(adult_full)

#Having looked closely at workclass, We we that we can collapse workclass to meaningful categories
adult_full[adult_full$workclass %in% c("Federal-gov","Local-gov","State-gov"), "workclass"] <- "Government"
adult_full[adult_full$workclass %in% c("Self-emp-inc","Self-emp-not-inc"), "workclass"] <- "Self-Employed"
adult_full[adult_full$workclass %in% c("Never-worked","Without-pay","Other","Unknown"), "workclass"] <- "Other/Unknown"


#A quick look at the native.country column tells that we have an opportunity to collapse to meaningful number of levels
str(factor(adult_full$native.country))
#It has 41 levels and it looks like not a good predictor in its current form.
table(adult_full$native.country)
levels(factor(adult_full$native.country))

#replace country in place based on the grouping given below inline
adult_full[adult_full$native.country %in% c("Vietnam","Laos","Cambodia","Thailand"), "native.country"] <- "SEAsia"
adult_full[adult_full$native.country %in% c("South"), "native.country"] <- "unknown"
adult_full[adult_full$native.country %in% c("China","India","HongKong","Iran","Philippines","Taiwan", "Japan"), "native.country"] <- "Asia"
adult_full[adult_full$native.country %in% c("Canada","Mexico","Puerto-Rico","United-States"), "native.country"] <- "NorthAmerica"
adult_full[adult_full$native.country %in% c("Ecuador","Peru","Columbia","Trinadad&Tobago", "Cuba","Dominican-Republic","Guatemala","Haiti","Honduras","Jamaica", "Nicaragua","El-Salvador"), "native.country"] <- "SouthAmerica"
adult_full[adult_full$native.country %in% c("France","Germany","Greece","Holand-Netherlands","Italy","Hungary","Ireland","Poland","Portugal","Scotland","England","Yugoslavia", "France"), "native.country"] <- "Europe"
adult_full[adult_full$native.country %in% c("Outlying-US(Guam-USVI-etc)"), "native.country"] <- "Oceania"

#now reduced to 8 levels + the one NA
str(factor(adult_full$native.country))
levels(factor(adult_full$native.country))
table(adult_full$native.country)




#For ease of working, we will also recode 'income' to be 0 if '<=50K' or 1 otherwise   
adult_full$income <- ifelse(adult_full$income == "<=50K", 0, 1)
adult_full$income <- as.factor(adult_full$income)
levels(adult_full$income)


#
# Years of education can be discarded as we get the info from the education.num
adult_full$education <- NULL

# continous veriable and is not very meaningful
adult_full$fnlwgt <- NULL

#Relationship is also not helpful
adult_full$relationship <- NULL


#Now we are ready for moving to the next stage.. let's take a closer look
str(adult_full)
head(adult_full)

####################################
## Data Analysis 
####################################

# Relationship between Age and Income


# histogram of age by income group

adult_full %>% ggplot(aes(age)) + 
  geom_histogram(aes(fill=income),color='black',binwidth=1,alpha=0.5) + 
  theme_bw() + ggtitle("Age & Income")


#First look at the histogram reveals that yonger people appear to have lower income and
#and the spread of higher income appears to spread mostly between people at 40's - 50's

#We can confirm it with a box plot
adult_full %>% ggplot() + geom_boxplot(aes(y=age,fill=income),color='black',alpha=0.5) +
theme_bw() + ggtitle("Age & Income Boxplot")



#Workclass Vs Income
ggplot(adult_full,aes(workclass,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.5, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Workclass & Income")
#People in workclass 'Private' clearly stand out


#marital.status vs Income
ggplot(adult_full,aes(marital.status,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.5, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Marital Status & Income")
#Married-civ-spouse status stands out clearly showing a higher propotion of income


#occupation  vs income
ggplot(adult_full,aes(occupation,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.5, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Occupation & Income")
#Exec-managerial & Prof-specialty have a higher propotion of income


#race vs income
ggplot(adult_full,aes(race,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.5, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Race & Income")
#As we can see the whites stand out

#sex vs income
ggplot(adult_full,aes(sex,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.5, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Sex & Income")
# As we can see, the male gender has a higher propotion of income over 50K. 


#native.country vs income
ggplot(adult_full,aes(native.country,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.5, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Country & Income")
# The data collected has a bias to NorthAmerica, as it is from US census.. hence may not be a good predictor


# histogram of capital.gain.. distribution looks like skewed 
ggplot(adult_full,aes(capital.gain,group=income)) + geom_histogram(aes(fill=income),bins=10, color='black',alpha=0.5)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Capital Gain")

#Also more than 90% of the people have reported 0 capital gain 
sum(adult_full$capital.gain == 0)/length(adult_full$capital.gain) * 100


# histogram of capital.loss.. here also the distribution looks skewed 
ggplot(adult_full,aes(capital.loss,group=income)) + geom_histogram(aes(fill=income),bins=10, color='black',alpha=0.5)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Capital Loss")

#Also more than 95% of the people have reported 0 capital loss 
sum(adult_full$capital.loss == 0)/length(adult_full$capital.loss) * 100

#Histogram of hours per week. It looks like majority are around the 40-50 mark
ggplot(adult_full,aes(hours.per.week,group=income)) + geom_histogram(aes(fill=income),bins=10, color='black',alpha=0.5)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Hours Per Week")

# education Vs income. It shows that propotion of people with higher education tend to earn more than 50K than the less educated counterparts.
ggplot(adult_full,aes(education.num,group=income)) + geom_histogram(aes(fill=income),bins=15, color='black',alpha=0.5)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Number of Years of Eduction")


################
#Model Fitting
################


# Split the adult_full data set into training and test sets, test set will be 20% of adult_full set

set.seed(1234)

test_index <- createDataPartition(y = adult_full$income, times = 1, p = 0.25, list = FALSE)
adult_test <- adult_full[test_index,]
adult_train <- adult_full[-test_index,]


dim(adult_test) 
#7541 obs. of  12 variables


dim(adult_train) 
#22621 obs. of  12 variables



# Classification And Regression Trees

#Fit model on training set
tree_adult <- rpart(income ~ ., data = adult_train, method = 'class', minbucket=20)
tree_pred <- predict(tree_adult, newdata = adult_test, type = 'class') 
tree_pred_prob <- predict(tree_adult, adult_test, type = "prob")

#Let's print the confusion matrix
confusionMatrix(adult_test$income, tree_pred)


tab_tree <- table(tree_pred, adult_test$income)
#tab_tree
#83.49 %

error_tree = 1 - sum(tab_tree[row(tab_tree)==col(tab_tree)])/sum(tab_tree)
error_tree
#15.5%

plot(tree_adult, margin = 0.1)
text(tree_adult, cex = 0.75)
title("Classification And Decision Tree")


#Logistic Regression Model
#set.seed(100)

glm_fit <- glm(income ~ ., family = binomial(logit), data = adult_train)

glm_pred<- predict(glm_fit, adult_test, type = "response")

tab_glm <- table(actual= adult_test$income, predicted= glm_pred>0.5)
#tab_glm
#84.78 % accuracy

#Let's run a summary statistics
summary(glm_fit)

#Run the prediction using a few significant predictors to see if there is any change in accuracy
glm_fit1<- glm(income ~ age + workclass + education.num +  marital.status, family=binomial(link='logit'),data = adult_train)
glm_pred1<- predict(glm_fit1, adult_test, type = "response")
tb1 <- table(actual= adult_test$income, predicted= glm_pred1>0.5)
tb1
#accuracy reduced to 81%, so let's stick to all predictors


error_glm = 1 - sum(tab_glm[row(tab_glm)==col(tab_glm)])/sum(tab_glm)
error_glm
#15.89 %


## KNN model


fit_knn <- knn3(income ~ ., adult_train,  k = 5, prob=TRUE)

knn_pred <- predict(fit_knn, adult_test, type="class")
knn_pred_prob <- predict(fit_knn, adult_test, type = "prob")

confusionMatrix(adult_test$income, knn_pred)

tab_knn <- table(knn_pred, adult_test$income)
#tab_knn
#84.17%


error_knn = 1 - sum(tab_knn[row(tab_knn)==col(tab_knn)])/sum(tab_knn)
error_knn
#16%



#ROC curve
#ROC curve is a plot of true positive rate against false positive rate under all threshold values. Confusion matrix is a measurement of overall prediction accuracy. Since the majority of observations in the data set has income less than $50,000 a year, sensitivity and specificity contribute to the overall accuracy by different weights. The different classifiers are compared using ROC curve.

#Create a prediction object & dataframe of True Positives and False Positives for Logistic
pr <- prediction(glm_pred, adult_test$income)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
dd <- data.frame(FP = prf@x.values[[1]], TP = prf@y.values[[1]])
#dd


#Create a prediction object & dataframe of True Positives and False Positives for CART
#tree_pred_prob[,2]
pr2 <- prediction(tree_pred_prob[,2], adult_test$income)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
dd2 <- data.frame(FP = prf2@x.values[[1]], TP = prf2@y.values[[1]])
#dd2

#Create a prediction object & dataframe of True Positives and False Positives for KNN
#knn_pred_prob[,2]
pr3 <- prediction(knn_pred_prob[,2], adult_test$income)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
dd3 <- data.frame(FP = prf3@x.values[[1]], TP = prf3@y.values[[1]])
#dd3


# plot ROC curves for the three prediction objects
ggplot() + 
  geom_line(data = dd, aes(x = FP, y = TP, color = 'Logistic Regression')) +
  geom_line(data = dd2, aes(x = FP, y = TP, color = 'CART')) +
  geom_line(data = dd3, aes(x = FP, y = TP, color = 'KNN')) +
  ggtitle('ROC Curve') + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate') 


# Area Under ROC curve represents the accuracy
auc <- rbind( performance(pr, measure = 'auc')@y.values[[1]],
              performance(pr2, measure = 'auc')@y.values[[1]],
              performance(pr3, measure = 'auc')@y.values[[1]]
              )
rownames(auc) <- (c('Logistic Regression', 'CART', 'KNN'))
colnames(auc) <- 'Area Under ROC Curve'
round(auc, 4)

#As we see, the logistic regression provided the best accuracy, followed by KNN. 
#CART had the least accuracy on the model that we applied here.
