rm(list = ls())

library(caret)

library(parallel)
library(doMC)

numCores = detectCores()
registerDoMC(cores = numCores)


# uploading the datasets
training = read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))

testing = read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

# partitioning the training into the training and validation sets
set.seed(100)
part = createDataPartition(training$classe,p=0.8,list=F)

training1 = training[part,]
validation = training[-part,]
# since this is no more necessary
rm(part) 

# Now I want to clear my predictors

anyNA(training1)
# I will ignore all those predictors who have many NAs

colSums(is.na(training1)) == 0 # all those predictors which have no NA

training1 = training1[,colSums(is.na(training1)) == 0]

# now all those predictors must be also in validation set

validation = validation[,colSums(is.na(validation)) == 0]

length(nearZeroVar(training1))
# this shows there are 34 predictors with no variance at all, so we remove them

training1 = training1[,-nearZeroVar(training1)]
validation = validation[,-nearZeroVar(validation)]

# Now we clean the IDs variables

training1 = training1[,-c(1:7)]
validation = validation[,-c(1:7)]

# Now we have cleaned the data and are ready to apply our algorithm
fitControl = trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE)

part2 = createDataPartition(training1$classe,p=0.5,list=F)
model = train(classe~.,data = training1[part2,],method = "rf",control = fitControl)


pred = predict(model,newdata = validation)
confusionMatrix(pred,validation$classe)

# Now we predict on actual test set

predictions = predict(model,newdata = testing)
predictions
