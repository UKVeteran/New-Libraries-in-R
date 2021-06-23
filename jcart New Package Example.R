##Load Libraries

library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(cowplot)
library(caret)
library(pROC)
library(ROCR)

## NOTE: This is a proof of concept. Further validation work needs to take place.
##need to create train/datasets

data = read.csv('C:\\Datasets\\Regression\\data1.csv', stringsAsFactors = F)
head(data) #allows you to check the data, first few entries
summary(data) #produce result summaries of the results of various model fitting functions.
dim(data) #the dimension (e.g. the number of columns and rows) of a matrix, array or data frame.
str(data)
# this shows that we need to tell R which columns contain factors
# it will also show us if there are some missing values.
hist(data$out_admitted)

#Baseline Accuracy
table(data$out_admitted)
#CATOOLS
library(caTools)
set.seed(123)
split = sample.split(data$out_admitted, SplitRatio = 0.80)
split

datatrain = subset(data, split==TRUE)
datatest = subset(data, split==FALSE)

head(datatrain)
head(datatest)

nrow(datatrain) # 42946 Training Samples
nrow(datatest)#10736 Test Samples

table(datatrain$out_admitted)
table(datatest$out_admitted)

data$out_admitted


library(rpart)
library(rpart.plot)

t= out_admitted ~
  ed_arrival_mode +
  ed_stream +
  ed_blue_stream +
  flag_prev_last_positive_ordered_dt +
  flag_shielded_pat +
  demo_age65 +
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
  como_Liver_Disease +
  como_Congestive_Heart_Failure +
  como_Acute_Myocardial_Infarction +
  como_Pulmonary_Disease +
  como_Paraplegia +
  como_Cerebral_Vascular_Accident +
  como_Smoker +
  como_Hypertension+demo_age+demo_gender

library(jcart)
jcart(t)

prp(jcart(t))

predTrain = predict(jcart(t))[,2]
summary(predTrain)
table(datatrain$out_admitted, predTrain >= 0.5)

predTest = predict(jcart(t), newdata=datatest)[,2]
table(datatest$out_admitted, predTest >= 0.5)
pred = prediction(predTest, datatest$out_admitted)
as.numeric(performance(pred, "auc")@y.values)
