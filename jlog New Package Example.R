##Load Libraries

library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(cowplot)
library(blorr)
library(olsrr)
library(caret)
library(pscl)
library(lmtest)
library(ipred)
library(survival)
library(ResourceSelection)
library(survey)
library(s2dverification)
library(lmtest)
library(pROC)
library(DescTools)

## NOTE: This is a proof of concept. Further validation work needs to take place.
##need to create train/datasets

data = read.csv('C:\\Datasets\\Regression\\data1.csv', stringsAsFactors = F)
head(data) #allows you to check the data, first few entries
summary(data) #produce result summaries of the results of various model fitting functions.
dim(data) #the dimension (e.g. the number of columns and rows) of a matrix, array or data frame.
str(data)
# this shows that we need to tell R which columns contain factors
# it will also show us if there are some missing values.
x<-data$out_admitted
hist(x)

#Baseline Accuracy
table(x)


#CATOOLS
library(caTools)
set.seed(123)
split = sample.split(x, SplitRatio = 0.80)
#replace x with dependent variable

datatrain = subset(data, split==TRUE)
datatest = subset(data, split==FALSE)

head(datatrain)
head(datatest)

nrow(datatrain)#Training Samples
nrow(datatest) #Test Samples

x

# Logistic Regression Model
t = out_admitted ~
  ed_arrival_mode +
  ed_stream +
  ed_blue_stream +
  flag_prev_last_positive_ordered_dt +
  flag_shielded_pat +
  ed_arrival_hour +
  flag_care_home_id +
  path_creatinine +
  path_prothrombin +
  path_aki +
  como_Peripheral_Vascular_Disease +
  como_Renal_Disease +
  como_Diabetes +
  como_Connective_Tissue_Disorder +
  como_Dementia +
  como_Severe_Liver_Disease +
  como_Metastatic_Cancer +
  como_HIV +
  como_Peptic_Ulcer +
  como_Congestive_Heart_Failure +
  como_Acute_Myocardial_Infarction +
  como_Paraplegia +
  como_Cerebral_Vascular_Accident +
  como_Hypertension+demo_gender

library(JLog)
jlog(t)

summary(jlog(t))

predictTrain = predict(jlog(t), type="response")
summary(predictTrain)
predictTrain
y=datatrain$out_admitted
tapply(predictTrain, y, mean)

library(ROCR)
ROCRpred = prediction(predictTrain,y)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


# Confusion matrix for threshold of 0.5 for training set
table(y, predictTrain > 0.5)

#Making prediction on the test set
predictTest = predict(jlog(t), type = "response", newdata = datatest)
z= datatest$out_admitted
cm<-table(z,predictTest >= 0.5)
cm
fourfoldplot(cm)
fourfoldplot(cm, std = "all.max")

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

#Compute the Accuracy
accuracy = sum(diag) / n
accuracy

#Compute the Precision
precision = diag / colsums
precision

#Compute the Sensitivity
recall = diag / rowsums
recall

#Compute the F Score
f1 = 2 * precision * recall / (precision + recall)
f1



