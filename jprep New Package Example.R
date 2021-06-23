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

#Baseline Accuracy
library(JPrep)
dt=data$out_admitted
jprep()
