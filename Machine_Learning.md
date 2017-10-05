# Practical Machine learning
Danie Roelofse  
03 October 2017  



## Introduction and background
This project attempts to create a prediction model to decide if a particular exercise had been performed correctly, or if one of four common mistakes has occurred during the routine. Data has been sourced from [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har), where six participants preformed 10 repetitions of the above mentioned exercise in five different ways, __Class A__ being done correctly, and __Class B__ to __E__ making a predefined common mistake during the exercise. Measuring devices such as accelerators were placed on four locations to record the movement and changes in orientation of the following regions: hips, arm, forearm and the dumbbell itself.

We will be using the *__R__* package *__caret__* for model building and creating data partitions for validations later on.


## Initial Data analysis and preporcessing


```r
library('caret')
wm <- read.csv("C:\\Coursera\\Machine Learning\\Assignment\\pml-training.csv", header = TRUE, na.strings = c("NA", ""))
wm_test <- read.csv("C:\\Coursera\\Machine Learning\\Assignment\\pml-testing.csv", header = TRUE, na.strings = c("NA", ""))
```

### Initial conclusions
There are alot of variables in this dataset, but the majority is mostly blank. We will use the _colSums_ fuction to sort that out for us. We will also then create a new training set and a validation set called training and validate, using the _seed_ function, all to make sure we don't overfit

```r
csums <- colSums(is.na(wm))
csums_log <- (csums == 0)
training_fewer_cols <- wm[, (colSums(is.na(wm)) == 0)]
wm_test <- wm_test[, (colSums(is.na(wm)) == 0)]
##

investigate <- wm[,which(names(wm) %in%
      c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2",
        "cvtd_timestamp","new_window","num_window","classe"))]
```

I will now remove all factor variables and also remove the first 6 variables which will not have any prediction value

```r
col_names <- c()
n <- ncol(training_fewer_cols)-1
for (i in 1:n) {
 if (is.factor(training_fewer_cols[,i])){
 col_names <- c(col_names,i)
 }
}
col_names <- c()
n <- ncol(training_fewer_cols)-1
for (i in 1:n) {
 if (is.factor(training_fewer_cols[,i])){
 col_names <- c(col_names,i)
 }
}
training_fewer_cols <- training_fewer_cols[,-col_names]

#Removing the first 7 variables
training_fewer_cols <- training_fewer_cols[,-c(1:7)]
```
##Cross validation.
I used a for loop to set up cross validation using random subsampling to fit three random forest models to random subsets of the training data, called "trainingSet". I then used these models to predict the "classe" variable of the testing subsets, called "testingSet". I was hoping for an out of sample error of less than 20%.


```r
library(caret)
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.4.2
```

```r
first_seed <- 123355
accuracies <-c()
for (i in 1:3){
 set.seed(first_seed)
 first_seed <- first_seed+1
 trainIndex <- createDataPartition(y=training_fewer_cols$classe, p=0.75, list=FALSE)
 trainingSet<- training_fewer_cols[trainIndex,]
testingSet<- training_fewer_cols[-trainIndex,]
modelFit <- randomForest(classe ~., data = trainingSet)
 prediction <- predict(modelFit, testingSet)
testingSet$rightPred <- prediction == testingSet$classe
t<-table(prediction, testingSet$classe)
 print(t)
 accuracy <- sum(testingSet$rightPred)/nrow(testingSet)
 accuracies <- c(accuracies,accuracy)
 print(accuracy)
}
```

```
##           
## prediction    A    B    C    D    E
##          A 1392   10    0    0    0
##          B    0  937   12    0    0
##          C    0    2  843   17    0
##          D    0    0    0  786    4
##          E    3    0    0    1  897
## [1] 0.9900082
##           
## prediction    A    B    C    D    E
##          A 1395    9    0    0    0
##          B    0  931   16    0    0
##          C    0    9  839   15    1
##          D    0    0    0  785    1
##          E    0    0    0    4  899
## [1] 0.9887847
##           
## prediction    A    B    C    D    E
##          A 1393    4    0    0    0
##          B    1  943   11    0    3
##          C    0    2  844   25    1
##          D    0    0    0  778    5
##          E    1    0    0    1  892
## [1] 0.9889886
```


The accuracies for the models are: 
0.9900082, 0.9887847, 0.9889886
and the mean accuracies would then be: 
0.9892605
Thus the out of time sample error is then 
0.0107395

I am happy, let's now score the testing set and answer the quiz questions.

```r
modelFit <- randomForest(classe ~., data = training_fewer_cols)
nrow(wm_test)
```

```
## [1] 20
```

```r
prediction <- predict(modelFit, wm_test)
prediction
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```


