rm(list=ls())
library(cowplot)

# C50
c50 <- read_csv("c50.csv")
c50_smote <- read_csv("C50_smote.csv")

c50_mean <- c50 %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="C50", .before="Accuracy")

c50_smote_mean <- c50_smote %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="C50_smote", .before="Accuracy")

c50_plot <- rbind(c50_mean, c50_smote_mean)
g1 <- c50_plot %>% 
  pivot_longer(
    cols=c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision"),
    names_to="Measures",
    values_to="Values") %>% 
  ggplot(aes(x=Algorithm, y=Values, fill=Measures)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(breaks=c("C50", "C50_smote"), labels=c("Without SMOTE", "With SMOTE")) +
  labs(x="C50") +
  theme(legend.position="none")

# Naive Bayes
NB <- read_csv("NB.csv")
NB_smote <- read_csv("NB_smote.csv")

NB_mean <- NB %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="NB", .before="Accuracy")

NB_smote_mean <- NB_smote %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="NB_smote", .before="Accuracy")

NB_plot <- rbind(NB_mean, NB_smote_mean)
g2 <- NB_plot %>% 
  pivot_longer(
    cols=c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision"),
    names_to="Measures",
    values_to="Values") %>% 
  ggplot(aes(x=Algorithm, y=Values, fill=Measures)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(breaks=c("NB", "NB_smote"), labels=c("Without SMOTE", "With SMOTE")) +
  labs(x="Naive Bayes") +
  theme(legend.position="none")

# SVM
SVM <- read_csv("SVM.csv")
SVM_smote <- read_csv("SVM.csv")

SVM_mean <- SVM %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="SVM", .before="Accuracy")

SVM_smote_mean <- SVM_smote %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="SVM_smote", .before="Accuracy")

SVM_plot <- rbind(SVM_mean, SVM_smote_mean)
g3 <- SVM_plot %>% 
  pivot_longer(
    cols=c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision"),
    names_to="Measures",
    values_to="Values") %>% 
  ggplot(aes(x=Algorithm, y=Values, fill=Measures)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(breaks=c("SVM", "SVM_smote"), labels=c("Without SMOTE", "With SMOTE")) +
  labs(x="SVM") +
  theme(legend.position="none")

# Logistic regression
LR <- read_csv("LR.csv")
LR_smote <- read_csv("LR_smote.csv")

LR_mean <- LR %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="LR", .before="Accuracy")

LR_smote_mean <- LR_smote %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="LR_smote", .before="Accuracy")

LR_plot <- rbind(LR_mean, LR_smote_mean)
g4 <- LR_plot %>% 
  pivot_longer(
    cols=c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision"),
    names_to="Measures",
    values_to="Values") %>% 
  ggplot(aes(x=Algorithm, y=Values, fill=Measures)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(breaks=c("LR", "LR_smote"), labels=c("Without SMOTE", "With SMOTE")) +
  labs(x="Logistic regression") +
  theme(legend.position="none")

# Neural network
NN <- read_csv("NN.csv")
NN_smote <- read_csv("NN_smote.csv")

NN_mean <- NN %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="NN", .before="Accuracy")

NN_smote_mean <- NN_smote %>% 
  select(-TR_Data, -Recall) %>% 
  summarise_all(mean) %>% 
  add_column(Algorithm="NN_smote", .before="Accuracy")

NN_plot <- rbind(NN_mean, NN_smote_mean)
g5 <- NN_plot %>% 
  pivot_longer(
    cols=c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision"),
    names_to="Measures",
    values_to="Values") %>% 
  ggplot(aes(x=Algorithm, y=Values, fill=Measures)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(breaks=c("NN", "NN_smote"), labels=c("Without SMOTE", "With SMOTE")) +
  labs(x="Neural network")

plot_grid(g1, g2, g3 ,nrow=1)
plot_grid(g4 ,g5, rel_widths=c(0.8,1))
