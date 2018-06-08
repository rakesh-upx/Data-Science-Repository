##Predictive Analytics on Diabetes dataset in R
## Using Naive Bayes and SVM

## Import the dataset
diabetes<-read.csv("C:/Users/Bubun/Downloads/Diabetes_latest.csv")
## Viewing first 4 rows
head(diabetes,4)
#Finding the data type of all the variables given
str(diabetes)
## Find the shape of the dataset
dim(diabetes)
library(plyr)
### Changing the name of the columns into new ones using 'rename' function of 'plyr' package and store the dataframe in the same 
### rename diabetes
diabetes<-rename(diabetes,c("Pregnancies"="pregnancy","Glucose"="glucose","BloodPressure"="bp","SkinThickness"="skin","Insulin"="insulin","BMI"="bmi",
                            "DiabetesPedigreeFunction"="pedigree","Age"="age","Outcome"="outcome"))


print(names(diabetes))

### Missing value detection
## Importing mice for missing value
##library(mice)
##a<-mice(diabetes)
## summary returns number of missing values per variable
##summary(a)
summary(diabetes)## Find NA values(Missing Values) at the bottom per column
## Another approach is by sapply. Here function(x) means all the variables given, then we want to find the sum of NA values
## given as is.na(x) of all the variables given
diabetes.mv<-sapply(diabetes,function(x)sum(is.na(x)))
## Finding Missing value per column
diabetes.mv

### Missing value replacement
## Replacement of Missing Value of Blood Pressure with column mean
## 'replace' function is used for this. first argument(diabetes$bp) is the name of the dataset,
## 2nd Argument(is.na(diabetes$bp)) is the variable having missing values.Then we replaced
## the missing values of bp variable with mean of bp variable."na.rm"=TRUE means all the complete
## values excluding the missing values of bp variable are taken for mean computation.
diabetes$bp<-replace(diabetes$bp,is.na(diabetes$bp),median(diabetes$bp,na.rm = TRUE))
## Replacement of Missing Value of Glucose with column mean
diabetes$glucose<-replace(diabetes$glucose,is.na(diabetes$glucose),median(diabetes$glucose,na.rm = TRUE))
## Replacement of Missing Value of Diabetes Pedigreee Function with column mean
diabetes$pedigree<-replace(diabetes$pedigree,is.na(diabetes$pedigree),median(diabetes$pedigree,na.rm = TRUE))
## Replacement of Missing Value of Age with column mean
diabetes$age<-replace(diabetes$age,is.na(diabetes$age),median(diabetes$age,na.rm = TRUE))

## Now check again whether we have missing values or not
summary(diabetes)


## To make the response variable(outcome)Categorical, we need to use "as.factor" function. 
diabetes$outcome<-as.factor(diabetes$outcome)

### Draw histogram for each variable to see number of zero values
hist(diabetes$pregnancy)
hist(diabetes$glucose)
hist(diabetes$bp)
hist(diabetes$skin)
hist(diabetes$insulin)
hist(diabetes$bmi)
hist(diabetes$pedigree)
hist(diabetes$age)
### Replace zero values for ....with column median
diabetes$pregnancy<-replace(diabetes$pregnancy,diabetes$pregnancy==0,median(diabetes$pregnancy,na.rm = TRUE))
table(diabetes$pregnancy)
table(diabetes$glucose)
diabetes$glucose<-replace(diabetes$glucose,diabetes$glucose==0,median(diabetes$glucose,na.rm = TRUE))
table(diabetes$bp)
diabetes$bp<-replace(diabetes$bp,diabetes$bp==0,median(diabetes$bp,na.rm = TRUE))
table(diabetes$skin)
diabetes$skin<-replace(diabetes$skin,diabetes$skin==0,median(diabetes$skin,na.rm = TRUE))
table(diabetes$insulin)
diabetes$insulin<-replace(diabetes$insulin,diabetes$insulin==0,median(diabetes$insulin,na.rm = TRUE))
table(diabetes$bmi)
diabetes$bmi<-replace(diabetes$bmi,diabetes$bmi==0,median(diabetes$bmi,na.rm = TRUE))
table(diabetes$pedigree)## No zero value
table(diabetes$age)## No zero value

### Check by finding the minimum value of each column
min(diabetes$pregnancy)
min(diabetes$glucose)
min(diabetes$bp)
min(diabetes$skin)
min(diabetes$insulin)
min(diabetes$bmi)
min(diabetes$pedigree)
min(diabetes$age)
## ggplot2 package is imported for plotting graphs
library(ggplot2)
## boxplot helps us to find or visualise if there is any Outlier in the variable. We check the same of bp with respect
## to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$bp))+geom_boxplot()
## We check the presence of any outlier in pregnancy with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$pregnancy))+geom_boxplot()
## We check the presence of any outlier in glucose with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$glucose))+geom_boxplot()
## We check the presence of any outlier in skin with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$skin))+geom_boxplot()
## We check the presence of any outlier in insulin with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$insulin))+geom_boxplot()
## We check the presence of any outlier in bmi with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$bmi))+geom_boxplot()
## We check the presence of any outlier in pedigree with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$pedigree))+geom_boxplot()
## We check the presence of any outlier in age with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$age))+geom_boxplot()
### Pair plot will show the relationship between all  the variables(Except outcome variable) concerned in a single frame
windows(7,7)+pairs(diabetes[,-9])

## Finding quartile(1st Qu.,2nd Qu. or Median,3rd Qu.,Max),in other words(25%,50%,75%,100%)
summary(diabetes)
## Finding quartile(1st Qu.,2nd Qu. or Median,3rd Qu.,Max),in other words(25%,50%,75%,100%).0% menas the least number of the variable

print(quantile(diabetes$glucose))

print(quantile(diabetes$bp))

print(quantile(diabetes$insulin))

print(quantile(diabetes$bmi))

print(quantile(diabetes$age))
## we can visualize the variable to check if they follow normal diatribution using qqnorm & qqline
qqnorm(diabetes$age)
qqline(diabetes$age)
qqnorm(diabetes$glucose)
qqline(diabetes$glucose)

# Standardization of The dataset. Rescaling the dataset into one scale so that  the mean becomes 0 & standard deviation 1
## We don't want to rescale our target variable.Inside the square braces -9 means we don't want 9th column(label) to be rescaled
scaled_data<-scale(diabetes[,-9])
### Quick check the Standard deviation & Mean of the standardized data
print(mean(scaled_data))
## Quick check the Standard deviation of the standardized data
print (sd(scaled_data))
## Now we append outcome variable to scaled_diabetes using the data.frame function
scaled_data<-data.frame(scaled_data,diabetes$outcome)
head(scaled_data)
##Histogram
hist(scaled_data$pregnancy)
qqnorm(scaled_data$pregnancy)
qqline(scaled_data$pregnancy)
mean(scaled_data$pregnancy)## Only check whether mean becomes 0 or not to get a standard normal distribution
sd(scaled_data$pregnancy)## Only check whether standard deviation becomes 1 or not to get a standard normal distribution
## Above mean & standard deviation check applicable for all the variables
### Data partitioning
## Caret needs to be imported for the following functions
library(caret)
## First argument scaled_diabetes" is a vector of outcomes or response that we have to define.
## p=.75 means 75% data goes to training set
Training_testing <- createDataPartition(scaled_data$diabetes.outcome, p = .75, list = FALSE) 
## [Training_testing,] it means we want first set of rows of train_testing 
training <- diabetes[Training_testing,]
## remaining amount data will go to testing
testing  <- diabetes[-Training_testing,]
### Check the dimension of training_set & testing_set
dim(training)
dim(testing)
head(training)
## perform 10-fold cross validation
## 10-fold CV ## repeated ten times
fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 10)

## Train a Naive Bayes Model with the training data set"
library(naivebayes)## Install this Package then Import
NBFit <- train(outcome~ ., data = training, method = "naive_bayes",trControl = fitControl)
print(NBFit)
NB_predict<-predict(NBFit,testing[,-9])## Predicting testing set with our model.[,-9] means except 9th column(label)
confusionMatrix(NB_predict,testing$outcome)## Check  the classification Report
table(testing$outcome)


## We performe Linear SVM(svmlinear2) available in caret package.
svmFit <- train(outcome ~ ., data = training, 
                method = "svmLinear2", 
                trControl = fitControl,verbose = FALSE)
print(svmFit)## Check the trained model at various accuracy
svm_prd<-predict(svmFit, newdata = head(testing,20))## Predict with first 20 instances
svm_prd
head(testing$outcome,20)
svm_prd1<-predict(svmFit, newdata =testing)### predict the entire test data
confusionMatrix(testing$outcome,svm_prd1)## Confusion Matrix for classification report

### Algorithms applied

