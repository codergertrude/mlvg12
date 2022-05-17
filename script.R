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
library(devtools)
library(factoextra)
library(mlbench)
# library for C5 algorithm
library(C50)
# library for OneR algorithm
library(OneR)
# library for RIPPER algorithm
library(RWeka)
# library for Neural Network
library(neuralnet)
library(nnet)
#library for SMOTE
library(smotefamily)

# read from csv file, but file is not comma separated so use delim
Data = read.delim("marketing_campaign_1.csv", stringsAsFactors = FALSE)

# # basic observations over data
# head(Data)
# summary(Data)
# dim(Data)
# is.na(Data)

## function definitions 

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

levels(Data$Marital_Status)

# print correlation matrix
prompt.corrmatrix <- readline(prompt = "Type 1 to print a correlation matrix: ")
prompt.corrmatrix <- as.numeric(prompt.corrmatrix)

if(prompt.corrmatrix == 1){
  model.matrix(~0+., Data) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
}

lower_fence <- 0
upper_fence <- 0

# capping extreme outliers 
for(i in 1:ncol(Data)){
  if(is.numeric(Data[, i]) == TRUE){
    for(n in 1:nrow(Data)){
      if(Data[n, i] > (1.5*(IQR(Data[, i]))) + thirdquantile(Data[, i])){
        Data[n, i] = ninetyfivep(Data[, i])
        cat(sprintf("row %s column %1.0f is an outlier (too large)\n", n, i))
        upper_fence = upper_fence + 1
      }
      else if(Data[n, i] < firstquantile(Data[, i]) - (1.5*(IQR(Data[, i])))){
        Data[n, i] = fivep(Data[, i])
        cat(sprintf("row %s column %1.0f is an outlier (too small)\n", n, i))
        lower_fence = lower_fence + 1
      }
    }
  }
}

cat(sprintf("Upper fence outliers %1.0f\n", upper_fence))
cat(sprintf("Lower fence outliers %1.0f\n", lower_fence))

# prompt to show graphs, must convert prompt.graphs to numeric to use if() statement
prompt.graphs <- readline(prompt="Type 1 to see exploratory graphs: ")
prompt.graphs <- as.integer(prompt.graphs)

plot_list = list()
if(prompt.graphs == 1){
  for(i in 1:ncol(Data)){
    var = names(Data)[i]
    if(is.numeric(Data[, i]) == TRUE){
      plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var)
      # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_histogram(bins = 10) + labs(title = var) + coord_flip()
      print(plot_list[[i]])
    }
    else if(is.factor(Data[, i]) == TRUE){
      plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_bar() + labs(title = var) + coord_flip()
      print(plot_list[[i]])
    }
    else{
      cat(sprintf("feature %s is not graphable\n", i))
    }
  }
}

# min-max normalisation
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norm_list = c()
for(i in 1:ncol(Data)){
  if(is.numeric(Data[, i])){
    norm_list = c(norm_list, i)
  }
}

for(i in norm_list){
  cat(sprintf("Normalised column %s\n", i))
  Data[, i] <- as.data.frame(min_max_norm(Data[, i]))
}

tar_rat_initial <- prop.table(table(Data$Response)) * 100
tar_rat_initial


# Data <- Data %>% mutate_each_(list(~scale(.) %>% as.vector), vars = norm_list)

# partition percentage for loop
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)

cat("Modeling step has been reached.\n")

# rename last column to target, last column must always be the target variable
names(Data)[ncol(Data)] <- "Target"

# one-hot encoding (on a new df for testing)
DataOH <- as.data.frame(model.matrix(~0+., Data), row.names = NULL, optional = FALSE)
names(DataOH)[ncol(DataOH)] <- "Target"
# dataset for neural network implementation
DataNN <- as.data.frame(DataOH)
DataOH$Target <- as.factor(DataOH$Target)

# print statements prep
cat("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------", file = "Results.txt", sep = "\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "Results.txt", sep = "\n", append = TRUE) # Apply cat & append
cat("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------", file = "Results.txt", sep = "\n", append = TRUE)

pfc <- 0 # pfc - performance frame counter

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

# ## Feature transformation using principal components
# PC <- Data.pca[[5]] # creating component matrix
# PC <- PC[,1:2] # take first two components' variance (about 60% of total var)
# dim(PC)
# typeof(PC)
# DataM = t(as.matrix(DataNum)) # convert DF to matrix for mult
# dim(DataM)
# typeof(PC)
# DataFinal = PC %*% DataM

# Feature importance using LVQ (Learning Vector Quantified)
cat("LVQ feature selection implementation\n")

feature.prompt <- readline(prompt = "Type 1 to include feature selection: ")
feature.prompt <- as.integer(feature.prompt)

if(feature.prompt == 1){
  set.seed(42)
  train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  model <- train(Target~., data = Data, trControl = train_control, method = "lvq")
  importance <- varImp(model, scale=FALSE)
  print(importance)
  plot(importance)
  print(model)
  confusionMatrix(model)
}


# # Feature importance using RFE (Recursive Feature Elimination) (out of commission due to caret error)
# cat("RFE feature selection implementation\n")
# 
# set.seed(42)
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# results <- rfe(Data[,1:ncol(Data)-1], Data$Target, sizes=c(1:ncol(Data)-1), rfeControl=control)
# print(results)
# predictors(results)
# plot(results, type=c("g", "o"))



# one-hot encoding usage prompt
prompt.onehot <- readline(prompt = "Type 1 to one-hot encoded dataset for modelling: ")
prompt.onehot <- as.integer(prompt.onehot)

if(prompt.onehot == 1){
  Data <- DataOH
  
  # SMOTE for minority label
  cat("SMOTE implementation\n")
  
  # print target class splits
  tar_rat <- prop.table(table(DataNN$Target)) * 100
  rat1 <- tar_rat[1]
  rat2 <- tar_rat[2]
  cat(sprintf("Target class ratio: %s - %f\n", rat1, rat2))
  
  # prompt for SMOTE
  prompt.smote <- readline(prompt = "Enter 1 for SMOTE: ")
  prompt.smote <- as.integer(prompt.smote)
  
  if(prompt.smote == 1){
    # run SMOTE on OH encoded data, requires all numeric
    DataSM <- SMOTE(DataNN, Data$Target, K = 5)
    DataSM <- DataSM[["data"]] # extract dataframe from object
    DataSM <- DataSM[, -ncol(DataSM)] # remove auto-added class col
    
    # print new target class ratios
    tar_rat <- prop.table(table(DataSM$Target)) * 100
    rat1 <- tar_rat[1]
    rat2 <- tar_rat[2]
    cat(sprintf("Target class ratio: %s - %f\n", rat1, rat2))
    
    # reorganize data variables
    Data <- DataSM
    DataNN <- DataSM
    Data$Target <- as.factor(Data$Target)
  } else {
    sprintf("SMOTE will not be used for modelling.")
    
  }
}

# a second pass for constant columns, if one-hot encoding was employed
col_list = c()
for(i in 1:ncol(Data)){
  if(length(unique(Data[, i])) == nrow(Data)){
    col_list = c(col_list, i)
  }
  else if(length(unique(Data[, i])) == 1){
    col_list = c(col_list, i)
  }
  else{
    cat(sprintf("column %s will be included in modelling\n", i))
  }
}
Data = subset(Data, select = -c(col_list))
cat("Columns have been dropped.\n")

# model query implementation

prompt.models <- readline(prompt = "Type an algorithm's number: 
Decision Tree = 1
Naive Bayes = 2
Support-Vector Machine = 3
Logistic Regression = 4
Neural Network = 5 (uses OH encoding)")
prompt.models <- as.integer(prompt.models)

# modelling with prompts
if(prompt.models == 1){
  # C5.0 algorithm (decision tree)
  cat("C5.0 implementation")
  for(t in training_data_percentages){
    print("================================================================================================================")
    cat(sprintf("Current training partition: %s\n", t))
    
    pfc <- pfc + 1
    
    indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
    training_data = Data[indx_partition,]
    testing_data = Data[-indx_partition,]
    
    set.seed(42)
    TrainedClassifier = C5.0(x = training_data[, 1:ncol(testing_data)-1], y = training_data[, ncol(Data)])
    Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[, 1:ncol(testing_data)-1])
    
    cm <- confusionMatrix(testing_data[, ncol(testing_data)], Predicted_outcomes)
    print(cm)
    
    cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
        format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
        format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", 
        file = "Results.txt", sep = " ", append = TRUE)
    
    print("END OF RUN")
  }
} else if(prompt.models == 2){
    # Naive Bayes algorithm
    cat("Naive Bayes implementation")
    for(t in training_data_percentages){
      print("================================================================================================================")
      cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))
      
      pfc <- pfc + 1
      
      indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
      training_data = Data[indx_partition,]
      testing_data = Data[-indx_partition,]
      
      set.seed(42)
      TrainedClassifier = naiveBayes(Target ~ ., data = training_data, laplace=0)
      Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[,1:ncol(testing_data)-1])
      
      cm <- confusionMatrix(testing_data[, ncol(testing_data)], Predicted_outcomes)
      print(cm)
      
      cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", 
          file = "Results.txt", sep = " ", append = TRUE)
      
      print("END OF RUN")
    }
} else if(prompt.models == 3){
    # Support-Vector Machine
    cat("SVM implementation")
    for(t in training_data_percentages){
      print("================================================================================================================")
      cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))
      
      pfc <- pfc + 1
      
      indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
      training_data = Data[indx_partition,]
      testing_data = Data[-indx_partition,]
      
      set.seed(42)
      TrainedClassifier = svm(Target ~ ., data = training_data, scale = FALSE, type = 'C-classification', kernel = 'linear')
      Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[,1:ncol(testing_data)-1])
      
      cm <- confusionMatrix(testing_data[, ncol(testing_data)], Predicted_outcomes)
      print(cm)
      
      cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", 
          file = "Results.txt", sep = " ", append = TRUE)
      
      print("END OF RUN")
    }
} else if(prompt.models == 4){
    # Logistic regression algorithm (courtesy of Oil)
    cat("Logistic regression implementation")
    for(t in training_data_percentages){
      print("================================================================================================================")
      cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))
      
      pfc <- pfc + 1
      
      indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
      training_data = Data[indx_partition,]
      testing_data = Data[-indx_partition,]
      
      set.seed(42)
      TrainedClassifier = glm(Target ~ .,family=binomial(link='logit'), data = training_data)
      Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[,1:ncol(testing_data)-1], type = "response")
      
      # Set decision threshold
      Predicted_outcomes <- ifelse(Predicted_outcomes > 0.5, 1, 0)
      
      cat("modeling reached\n")
      
      cm <- confusionMatrix(testing_data$Target, as.factor(Predicted_outcomes))
      print(cm)
      
      cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", 
          file = "Results.txt", sep = " ", append = TRUE)
      
      print("END OF RUN")
    }
} else if(prompt.models == 5){
    # Neural Network algorithm
    cat("Neural Network implementation")
    for(t in training_data_percentages){
      print("================================================================================================================")
      cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))
      
      pfc <- pfc + 1
      
      indx_partition = createDataPartition(DataNN[, ncol(DataNN)], p = t, list = FALSE)
      training_data = DataNN[indx_partition,]
      testing_data = DataNN[-indx_partition,]
      
      cat("Partitions created.\n")
      
      set.seed(42)
      # nn formula only takes 30 variables, has to be filtered down
      # TrainedNeuralNet <- neuralnet(Target ~ ., data = training_data[9:ncol(training_data)], hidden = 2)
      
      cat("Neural network trained.\n")
  
      # Predicted_Parameters <- compute(TrainedNeuralNet, testing_data)

      TrainedNeuralNet <- train(Target ~ ., training_data, method='nnet', linout=TRUE, trace = FALSE, tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1))) 
      # Predicted_Net_Results <- Predicted_Parameters$net.result
      Predicted_Net_Results <- predict(TrainedNeuralNet, testing_data)
      Predicted_Net_Results <- ifelse(Predicted_Net_Results > 0.5, 1, 0)
      # Predicted_Data <- sapply(Predicted_Net_Results,round,digits=0)
      # Predicted_Data <- data.frame(Predicted_Data)
      par(mfrow=c(2,1))
      # corln <- cor(Predicted_Data, testing_data$Target)
      # corln
      cm <- confusionMatrix(as.factor(testing_data$Target), as.factor(Predicted_Net_Results))
      cm
      
      cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", 
          file = "Results.txt", sep = " ", append = TRUE)
      
      print("END OF RUN")
    }
  } else {
    sprintf("Invalid input: s", prompt.models)
}

# # OneR algorithm (not working)
# cat("OneR implementation")
# for(t in training_data_percentages){
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
