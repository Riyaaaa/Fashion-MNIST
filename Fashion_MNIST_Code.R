# TASK : APPLY K-NN ALGORITHM TO FASHION-MNIST DATASET
###Step 1: Importing required libraries###
library(MASS)
library(class)
library(gmodels)
library(caret)


###Step 2:Importing Training and Testing dataset###
train.data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)
test.data <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)


###Step 3: Checking the organization of datasets###
#Checking the structure of test and train data
str(train.data)
str(test.data)

#Viewing the contents of train and test data
View(train.data)
View(test.data)

#Viewing the label column of train and test data
table(train.data$X9)
table(test.data$X9)


#S##tep 4: Normalization of values###
#Create a function to normalize the numeric predictors to rescale them
normalize <- function(x) {
  y <- (x - min(x))/(max(x) - min(x))
  y
}

#Create new variable of train data with normalized data in all numeric columns
train_n_L <- lapply(train.data[2:785], normalize)
train_n <- data.frame(train_n_L)
train_n

#Create new variable of test data with normalized data in all numeric columns
test_n_L <- lapply(test.data[2:785], normalize)
test_n <- data.frame(test_n_L)
test_n


###Step 5: Find K and create vectors###
#Calculation of value of k
sqrt(nrow(train.data))
k <- 245

#Creation of vector of training dataset
MyTrainData <- train_n[1:59999, ]
#Creation of vector of testing dataset
MyTestData <- test_n[1:9999, ]

#Creation of vector of training dataset labels
MyTrainData.labels <- train.data[1:59999, 1]
#Creation of vector of testing dataset labels
MyTestData.labels <- test.data[1:9999, 1]


####Step 6: Apply k-NN and create Cross Table###
#Apllying k-NN algorithm
knn_predict <- knn(train = MyTrainData, test = MyTestData, cl = MyTrainData.labels, k = 245)

#Creation of Cross Table
CrossTable(x = MyTestData.labels, y = knn_predict,
           prop.chisq=FALSE)


####Step 7: Z- score standardisation method for data re-scaling####
#Z-score dataframe for training data
MyZScoreData <- as.data.frame(scale(train.data[-1]))

#Z-score dataframe for testing data
MyZScoreData2 <- as.data.frame(scale(test.data[-1]))


#Creation of Z-score vectors for training data
MyZScoreTrainData <- MyZScoreData[1:59999, ]
#Creation of Z-score vectors for testing data
MyZScoreTestData <- MyZScoreData2[1:9999, ]

#Creation of Z-score vectors for training data labels
MyZScoreTrainData.labels <- train.data[1:59999, 1]
#Creation of Z-score vectors for testing data labels
MyZScoreTestData.labels <- test.data[1:9999, 1]



###Step 8: Applying knn to Z-score prediction
MyZScorePredictedData <- knn(train = MyZScoreTrainData, 
                             test = MyZScoreTestData,
                             cl = MyZScoreTrainData.labels, 
                             k=245)

#Cross-Table for Z-score predicted data
CrossTable(x = MyZScoreTestData.labels, y = MyZScorePredictedData,
           prop.chisq=FALSE)


###Step 9: Evaluation of Results###


# Creation of table for MyZScorePredictedData vs MyZScoreTestData.labels
eval_results <- table(MyZScorePredictedData, MyZScoreTestData.labels)
#True Negative
TN = eval_results[1,1] 
#False Positive
FP = eval_results[1,2]
#False Negative
FN = eval_results[2,1]
#False Positive
TP = eval_results[2,2]

###Step 10: Calculations of Accuracy and Error Rate
#Calculation of Accuracy
accuracy = (TN + TP) / (TN + TP + FP + FN)

#Calculation of error rate
error_rate = 1-accuracy

###Plotting an image###
rotate <- function(x) {
  return(t(apply(x, 2, rev)))
}
plot_matrix <- function(vec) {
  q <- matrix(vec, 28, 28, byrow = TRUE)
  nq <- apply(q, 2, as.numeric)
  image(rotate(nq), col = gray((0:255)/255))
}
#If you want to plot the eighteenth image, for example, you can call the plot function as follows:
plot_matrix(test.data[18, 2:785])


###Findings###
#Exporting the test data labels into a .csv file 
write.csv( test.data$X9, "C:/Users/riyan/OneDrive/Documents/6020_Predictive_Analytics/Week 1/fashion/Test_data_labels.csv")
#Exporting the MyZScorePredictedData into a .csv file for comparison with test data labels
write.csv( MyZScorePredictedData, "C:/Users/riyan/OneDrive/Documents/6020_Predictive_Analytics/Week 1/fashion/ZScorePredictedData.csv")


