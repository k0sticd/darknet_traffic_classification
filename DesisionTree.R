#Ucitavanje dataseta
#setwd("C:/Users/Kostic/Desktop/proba/")
selected.df <- readRDS("preprocessed_darknet_.RDS")

#ucitavanje funkcija
source("util.R")

#Ucitavanje paketa
library(caret)
library(ROSE)
library(themis)
library(pROC)
library(rpart)
library(rpart.plot)

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

#kreiranje prvog stabla - nebalansiranog
tree0 <- rpart(Label ~ ., data = train.data, method = "class")
rpart.plot(tree0)
tree0.pred <- predict(object = tree0, newdata = test.data, type = "class")

#matrica konfuzije i evaluacione metrike
tree0.cm <- table(true=test.data$Label, predicted=tree0.pred)
tree0.cm
               #predicted
#true          Darknet Non.Darknet
#Darknet         157           9
#Non.Darknet       9         824

#izracunavanje evaluacionih metrika
tree0.eval <- compute.eval.metrics(tree0.cm)
tree0.eval
#accuracy precision    recall        F1     Kappa 
#0.9819820 0.9457831 0.9457831 0.9457831 0.9349788 

#control
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#downSample
set.seed(1)
down_inside <- train(x = train.data[,-12], 
                     y = train.data$Label,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl)

#up
ctrl$sampling <- "up"

set.seed(1)
up_inside <- train(x = train.data[,-12], 
                   y = train.data$Label,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl)

#rose
ctrl$sampling <- "rose"

set.seed(1)
rose_inside <- train(x = train.data[,-12], 
                     y = train.data$Label,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl)

#smote
#ctrl$sampling <- "smote"

#set.seed(1)
#smote_inside <- train(x = train.data[,-12], 
# = train.data$Label,
#method = "rpart",
#metric = "ROC",
#trControl = ctrl)

#contingency table and chi-square
(tab1<-table(train.data$Label.1, train.data$Label))
#Darknet Non.Darknet
#Audio-Streaming     367         125
#Browsing              9         931
#Chat                132         208
#Email                18         152
#File-Transfer        58         239
#P2P                   6        1375
#Video-Streaming      35         243
#VOIP                 42          61
chisq.test(train.data$Label.1, train.data$Label)
#Pearson's Chi-squared test

#data:  train.data$Label.1 and train.data$Label
#X-squared = 1790.2, df = 7, p-value < 2.2e-16
#zbog p<0.05, odbacujemo nultu hipotezu. Atributi nisu nezavisni

#orig fit
ctrl$sampling <- NULL


set.seed(1)
orig_fit <- train(x = train.data[,-12], 
                  y = train.data$Label, 
                  method = "rpart",
                  metric = "ROC",
                  trControl = ctrl)


inside_models <- list(original = orig_fit,
                      down = down_inside,
                      up = up_inside,
                      #SMOTE = smote_inside,
                      ROSE = rose_inside)

inside_resampling <- resamples(inside_models)
summary(inside_resampling, metric = "ROC")


#Call:
#summary.resamples(object = inside_resampling, metric = "ROC")

#Models: original, down, up, ROSE 
#Number of resamples: 50 

#ROC 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#original 0.8355296 0.8766030 0.8992150 0.9037764 0.9367421 0.9632692
#down     0.8412442 0.9060329 0.9184132 0.9188691 0.9375560 0.9860831
#up       0.8757788 0.9060329 0.9199102 0.9175814 0.9334926 0.9506211
#ROSE     0.7765004 0.8128854 0.8271226 0.8304893 0.8456866 0.8811084
#NA's
#original    0
#down        0
#up          0
#ROSE        0


up_inside$finalModel

##############################################################################
#Decision tree

#Kreiranje predikcija pomocu klasifikacionog stabla
rpart.plot(up_inside$finalModel)
tree1.pred <- predict(object = up_inside$finalModel, newdata = test.data, type = "class")

#Kreiranje matrice konfuzije
tree1.cm <- table(true=test.data$Label, predicted=tree1.pred)
tree1.cm
#predicted
#true          Darknet Non.Darknet
#Darknet         164           2
#Non.Darknet     112         721

tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval

#accuracy precision    recall        F1     Kappa 
#0.8858859 0.5942029 0.9879518 0.7420814 0.6745426 

#kros validacija
#numFolds = trainControl( method = "cv", number = 10 )

cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.0025))

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "up")

set.seed(1)
up_inside_cp <- train(x = train.data[,-12], 
                   y = train.data$Label,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid = cpGrid)

up_inside_cp
#cp = 0.001

#kreiranje novog stabla nakon kros validacije

rpart.plot(up_inside_cp$finalModel)

tree2.pred <- predict(up_inside_cp$finalModel, newdata = test.data, type = "class")

tree2.cm <- table(true = test.data$Label, predicted = tree2.pred)
tree2.cm
#predicted
#true          Darknet Non.Darknet
#Darknet         164           2
#Non.Darknet      10         823

tree2.eval <- compute.eval.metrics(tree2.cm)
tree2.eval
#accuracy precision    recall        F1     Kappa 
#0.9879880 0.9425287 0.9879518 0.9647059 0.9574731

#poredjenje
data.frame(rbind(tree0.eval, tree1.eval, tree2.eval),
           row.names = c(paste("TREE_", 1:3, sep = "")))

        #accuracy precision    recall        F1     Kappa
#TREE_1 0.9819820 0.9457831 0.9457831 0.9457831 0.9349788
#TREE_2 0.8858859 0.5942029 0.9879518 0.7420814 0.6745426
#TREE_3 0.9879880 0.9425287 0.9879518 0.9647059 0.9574731

#analiza znacajnosti atributa
varImp(up_inside_cp, surrogates = FALSE, competes = TRUE, scale = TRUE)