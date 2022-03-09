#Ucitavanje dataseta
setwd("C:/Users/Kostic/Desktop/proba/")
selected.df <- readRDS("preprocessed_darknet_.RDS")

#ucitavanje funkcija
source("util.R")

#Ucitavanje paketa
library(caret)
library(ROSE)
library(themis)
library(pROC)
#install.packages("randomForest")
library(randomForest)

#Kreiranje train i test datasetova 
set.seed(1)
train.indices <- createDataPartition(selected.df$Label, p = .80, list = FALSE)

train.data <- selected.df[train.indices,]
test.data <- selected.df[-train.indices,]

table(train.data$Label)

#prilagodjavanje dataseta subsampling metodama
train.data$Timestamp<-as.integer(train.data$Timestamp)
test.data$Timestamp<-as.integer(test.data$Timestamp)

levels(train.data$Label) <- c("Darknet", "Non.Darknet")
levels(test.data$Label) <- c("Darknet", "Non.Darknet")

#kreiranje prvog modela
rf <- randomForest(
  Label ~ .,
  data=train.data
)

rf.pred <- predict(object = rf, newdata = test.data, type = "class")

#matrica konfuzije i evaluacione metrike
rf.cm <- table(true=test.data$Label, predicted=rf.pred)
rf.cm
               #predicted
#true          Darknet Non.Darknet
#Darknet         162           4
#Non.Darknet       4         829

#izracunavanje evaluacionih metrika
rf.eval <- compute.eval.metrics(rf.cm)
rf.eval
#accuracy precision    recall        F1     Kappa 
#0.9919920 0.9759036 0.9759036 0.9759036 0.9711017 

#control
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#downSample
set.seed(1)
down_inside <- train(x = train.data[,-12], 
                     y = train.data$Label,
                     method = "rf",
                     metric = "ROC",
                     trControl = ctrl)

#up
ctrl$sampling <- "up"

set.seed(1)
up_inside <- train(x = train.data[,-12], 
                   y = train.data$Label,
                   method = "rf",
                   metric = "ROC",
                   trControl = ctrl)

#rose
ctrl$sampling <- "rose"

set.seed(1)
rose_inside <- train(x = train.data[,-12], 
                     y = train.data$Label,
                     method = "rf",
                     metric = "ROC",
                     trControl = ctrl)

#orig fit
ctrl$sampling <- NULL


set.seed(1)
orig_fit <- train(x = train.data[,-12], 
                  y = train.data$Label, 
                  method = "rf",
                  metric = "ROC",
                  trControl = ctrl)


inside_models <- list(original = orig_fit,
                      down = down_inside,
                      up = up_inside,
                      ROSE = rose_inside)

inside_resampling <- resamples(inside_models)
summary(inside_resampling, metric = "ROC")

#ROC 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#original 0.9902921 0.9995734 0.9999552 0.9985465 1.0000000 1.0000000    0
#down     0.9879122 0.9980905 0.9993684 0.9979127 0.9998097 1.0000000    0
#up       0.9906828 0.9996644 0.9999105 0.9988380 1.0000000 1.0000000    0
#ROSE     0.9229601 0.9443478 0.9526767 0.9529491 0.9607908 0.9742728    0

up_inside$finalModel

#Number of trees: 500
#No. of variables tried at each split: 6

#OOB estimate of  error rate: 0.07%
#Confusion matrix:
  #            Darknet Non.Darknet class.error
#Darknet        3334           0   0.0000000
#Non.Darknet       5        3329   0.0014997
##############################################################################
#Random Forest

rf2.pred <- predict(object = up_inside$finalModel, newdata = test.data, type = "class")

#Kreiranje matrice konfuzije
rf2.cm <- table(true=test.data$Label, predicted=rf2.pred)
rf2.cm
#             predicted
#true          Darknet Non.Darknet
#Darknet         162           4
#Non.Darknet       2         831

rf2.eval <- compute.eval.metrics(rf2.cm)
rf2.eval
#accuracy precision    recall        F1     Kappa 
#0.9939940 0.9878049 0.9759036 0.9818182 0.9782212 

#poredjenje
data.frame(rbind(rf.eval, rf2.eval),
           row.names = c(paste("FOREST_", 1:2, sep = "")))

        #accuracy precision    recall        F1     Kappa
#FOREST_1 0.991992 0.9759036 0.9759036 0.9759036 0.9711017
#FOREST_2 0.993994 0.9878049 0.9759036 0.9818182 0.9782212

#analiza znacajnosti atributa
varImp(up_inside, scale = TRUE)