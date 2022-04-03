library(ggplot2)
library(ggcorrplot)
library(lattice)
library(caret)
library(dplyr)
library(plyr)
library(tidyverse)
library(lubridate)
library(e1071)
library(Hmisc)
library(corrplot)
# library for C5 algorithm
library(C50)
# library for OneR algorithm
library(OneR)
# library for RIPPER algorithm
library(RWeka)
# library for Neural Network
library(neuralnet)

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(factoextra)

# read from csv file, but file is not comma separated so use delim
Data = read.delim("marketing_campaign_1.csv", stringsAsFactors = FALSE)

# # basic observations over data
# head(Data)
# summary(Data)
# dim(Data)
# is.na(Data)

# functions

# function for finding y percent of x
prcnt <- function(x, y){
  z = (x/100)*y
  return(z)
}

# function to determine whether if a cell is convertible to date
is.convertible.to.date <- function(x){
  !is.na(as.Date(as.character(x), tz = 'UTC', format = '%d-%m-%Y'))
}

# minmaxnorm <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }

# 5th percentile
fivep <- function(x){
  y = as.numeric(quantile(x, c(.05)))
  return(y)
}

# 95th percentile
ninetyfivep <- function(x){
  y = as.numeric(quantile(x, c(.95)))
  return(y)
}

# 1st quantile
firstquantile <- function(x){
  y = as.numeric(quantile(x, c(.25)))
  return(y)
}

# 3rd quantile
thirdquantile <- function(x){
  y = as.numeric(quantile(x, c(.75)))
  return(y)
}

###############################################################################

# print all column types, used for inspecting column types 
for(i in 1:(ncol(Data))) {
  if(is.numeric(Data[, i]) == TRUE){
    cat(sprintf("column %s is numeric\n", i))
  }
  else if(is.factor(Data[, i]) == TRUE){
    cat(sprintf("column %s is factor\n", i))
  }
  else{
    cat(sprintf("column %s is char\n", i))
  }
}

# replace missing values with mean of column IF they are numerical
for(i in 1:ncol(Data)){
  if(is.numeric(Data[, i]) == TRUE){
    for(n in 1:nrow(Data)){
      if(is.na(Data[n, i]) == TRUE){
        cat(sprintf("row: %s column: %f changed from NA\n", n, i))
        Data[n, i] <- mean(Data[, i], na.rm = TRUE)
      }
    }
  }
  else{
    cat(sprintf("%s is non-numeric\n", i))
  }
}

# extract year from dates formatted dd-mm-YY, other formats can be added
for(i in 1:ncol(Data)){
  if(is.character(Data[, i]) & is.convertible.to.date(Data[1, i]) == TRUE){
    Data[, i] = format(as.Date(Data[, i], "%d-%m-%Y", optional = FALSE), "%Y")
  }
}

# convert char features to factor, convert columns with <= 3 unique values to factor, print levels
for(i in 1:ncol(Data)){
  if(is.character(Data[, i]) == TRUE){
    Data[, i] <- as.factor(Data[, i])
    if(is.factor(Data[, i]) == TRUE){
      cat(sprintf("feature %s has been converted into factor\n", i))
      cat(sprintf("with levels: %s\n", levels(Data[, i])))
    }
    else{
      cat(sprintf("failed conversion for column %s\n", i))
    }
  }
  else if(length(unique(Data[, i])) <= 3){
    Data[, i] <- as.factor(Data[, i])
    cat(sprintf("feature %s has been converted into factor\n", i))
  }
  else{
    cat(sprintf("%s column is not char\n", i))
  }
}

# drop column with ALL unique values, aimed at ID-like unique values, col_list contains column drop index
col_list = c()
for(i in 1:ncol(Data)){
  if(length(unique(Data[, i])) == nrow(Data)){
    col_list = c(col_list, i)
    cat(sprintf("column %s has been dropped (IDENTIFIER)\n", i))
  }
  else if(length(unique(Data[, i])) == 1){
    col_list = c(col_list, i)
    cat(sprintf("column %s has been dropped (CONSTANT)\n", i))
  }
  else{
    cat(sprintf("column %s will be included in modelling\n", i))
  }
}
Data = subset(Data, select = -c(col_list))
cat("Columns have been dropped.")

# find values that only appear 2% of the time for columns with 3+ levels (outliers for factor), outliers for numerical using boxplot.stats$out
val_list = c()
for(i in 1:ncol(Data)){
  if(is.factor(Data[, i]) == TRUE){
    freq = table(Data[, i])
    for(x in 1:length(freq)){
      if(freq[x] < prcnt(nrow(Data), 2) & length(freq) > 3){
        val_list = c(val_list, names(freq[x]))
      }
    }
  }
}

# compare dataset values to lists val_list and outlier_values, store rows in frow_list (factor) and nrow_list (numeric)
frow_list = c()
for(i in 1:ncol(Data)){
  for(n in 1:nrow(Data)){
    if(is.factor(Data[, i]) == TRUE){
      for(x in 1:length(val_list)){
        if(val_list[x] == Data[n, i]){
          frow_list = c(frow_list, n)
        }
      }
    }
  }
}
# remove rows that contain outliers for factor
Data = Data[-c(frow_list),]

# # graphs using ggplot
# plot_list = list()
# for(i in 1:ncol(Data)){
#   var = names(Data)[i]
#   
#   if(is.numeric(Data[, i]) == TRUE){
#     # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var) + coord_flip()
#     plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_histogram(bins = 10) + labs(title = var) + coord_flip()
#     print(plot_list[[i]])
#   }
#   else if(is.factor(Data[, i]) == TRUE){
#     plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_bar() + labs(title = var) + coord_flip()
#     print(plot_list[[i]])
#   }
#   else{
#     cat(sprintf("feature %s is not graphable\n", i))
#   }
# }

# print correlation matrix
model.matrix(~0+., Data) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# capping extreme outliers 
for(i in 1:ncol(Data)){
  if(is.numeric(Data[, i]) == TRUE){
    for(n in 1:nrow(Data)){
      if(Data[n, i] > (1.5*(IQR(Data[, i]))) + thirdquantile(Data[, i])){
        Data[n, i] = ninetyfivep(Data[, i])
        cat(sprintf("row %s column %f is an outlier (too large)\n", n, i))
      }
      else if(Data[n, i] < firstquantile(Data[, i]) - (1.5*(IQR(Data[, i])))){
        Data[n, i] = fivep(Data[, i])
        cat(sprintf("row %s column %f is an outlier (too small)\n", n, i))
      }
    }
  }
}

# visualise outliers after capping
plot_list = list()
for(i in 1:ncol(Data)){
  var = names(Data)[i]
  if(is.numeric(Data[, i]) == TRUE){
    plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var)
    # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_histogram(bins = 10) + labs(title = var) + coord_flip()
    print(plot_list[[i]])
  }
}

# min-max normalisation 
norm_list = c()
for(i in 1:ncol(Data)){
  if(is.numeric(Data[, i])){
    norm_list = c(norm_list, i)
  }
}
Data <- Data %>% mutate_each_(list(~scale(.) %>% as.vector), vars = norm_list)

# one-hot encoding (on a new df for testing)
DataOH <- as.data.frame(model.matrix(~0+., Data), row.names = NULL, optional = FALSE)

# pfc - performance frame counter
pfc <- 0 

# partition percentage for loop
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)

cat("Modeling step has been reached.\n")

# rename last column to target, last column must always be the target variable
names(Data)[ncol(Data)] <- "Target"
names(DataOH)[ncol(DataOH)] <- "Target"

# principal component analysis
numlist = c()
for(i in 1:ncol(Data)){
  if(is.numeric(Data[,i]) == TRUE){
    numlist = c(numlist, i)
  }
}
DataNum = Data[,numlist]
Data.pca <- prcomp(DataNum, center = TRUE, scale. = TRUE)
fviz_eig(Data.pca)
ggrepel.max.overlaps = Inf
fviz_pca_var(Data.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# Feature importance using LVQ (Learning Vector Quantified)
cat("LVQ feature selection implementation\n")

set.seed(42)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(Target~., data = Data, trControl = train_control, method = "lvq")
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)
print(model)
confusionMatrix(model)

# Feature importance using RFE (Recursive Feature Elimination)
cat("RFE feature selection implementation\n")

set.seed(42)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(Data[,1:ncol(Data)-1], Data$Target, sizes=c(1:ncol(Data)-1), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))

# # C5.0 algorithm (decision tree)
# cat("C5.0 implementation")
# for(t in training_data_percentages){
#   pfc = pfc + 1
#   
#   print("================================================================================================================")
#   cat(sprintf("Current training partition: %s\n", t))
#   
#   indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
#   training_data = Data[indx_partition,]
#   testing_data = Data[-indx_partition,]
#   
#   set.seed(42)
#   TrainedClassifier = C5.0(x = training_data[, 1:ncol(testing_data)-1], y = training_data[, ncol(Data)])
#   Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[, 1:ncol(testing_data)-1])
#   
#   cm <- confusionMatrix(testing_data[, ncol(testing_data)], Predicted_outcomes)
#   print(cm)
#   print("END OF RUN")
# }

# Naive Bayes algorithm
cat("Naive Bayes implementation")
for(t in training_data_percentages){
  pfc = pfc + 1

  print("================================================================================================================")
  cat(sprintf("Current training partition: %s\n", t))

  indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
  training_data = Data[indx_partition,]
  testing_data = Data[-indx_partition,]

  set.seed(42)
  TrainedClassifier = naiveBayes(Target ~ ., data = training_data, laplace=0)
  Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[,1:ncol(testing_data)-1])

  cm <- confusionMatrix(testing_data[, ncol(testing_data)], Predicted_outcomes)
  print(cm)
  print("END OF RUN")
}

# # OneR algorithm (not working)
# cat("OneR implementation")
# for(t in training_data_percentages){
#   pfc = pfc + 1
#   
#   print("================================================================================================================")
#   cat(sprintf("Current training partition: %s\n", t))
#   
#   indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
#   training_data = Data[indx_partition,]
#   testing_data = Data[-indx_partition,]
#   
#   set.seed(42)
#   TrainedClassifier = OneR(Target ~., data = training_data) 
#   Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[,1:ncol(testing_data)-1])
#   
#   cm <- confusionMatrix(testing_data[, ncol(testing_data)], Predicted_outcomes)
#   print(cm)
#   print("END OF RUN")
# }

# Neural Network algorithm
cat("Neural Network implementation")
for(t in training_data_percentages){
  pfc = pfc + 1

  print("================================================================================================================")
  cat(sprintf("Current training partition: %s\n", t))

  indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
  training_data = Data[indx_partition,]
  testing_data = Data[-indx_partition,]
  
  cat("Partitions created.\n")

  set.seed(42)
  # nn formula only takes 30 variables, has to be filtered down
  TrainedNeuralNet <- neuralnet(Target ~ ., data = training_data[9:ncol(training_data)], hidden = 2)
  cat("Neural network trained.\n")
  
  Predicted_Parameters <- compute(TrainedNeuralNet, testing_data)
  Predicted_Net_Results <- Predicted_Parameters$net.result
  Predicted_Data <- sapply(Predicted_Net_Results,round,digits=0)
  Predicted_Data <- data.frame(Predicted_Data)
  par(mfrow=c(2,1))
  corln <- cor(Predicted_Data, testing_data$Target)
  corln

  print("END OF RUN")
}
