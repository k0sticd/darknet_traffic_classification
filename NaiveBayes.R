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
library(bnlearn)
library(e1071)
library(naivebayes)

#provera raspodele numerickih variabli
str(selected.df)
selected.df$Timestamp<-as.numeric(selected.df$Timestamp)
num.vars<-c(2:11)
apply(X = selected.df[,num.vars], MARGIN = 2, FUN = shapiro.test)
#testirani atributi nemaju normalnu raspodelu.

#DISKRETIZACIJA varijabli koje nemaju normalnu raspodelu
to.discretize <- c(2:11)
selected.df$Flow.IAT.Min <- as.numeric(selected.df$Flow.IAT.Min)
selected.df$Fwd.Seg.Size.Min <- as.numeric(selected.df$Fwd.Seg.Size.Min)
selected.df$Flow.IAT.Max <- as.numeric(selected.df$Flow.IAT.Max)
selected.df$FWD.Init.Win.Bytes <- as.numeric(selected.df$FWD.Init.Win.Bytes)
selected.df$Flow.Duration <- as.numeric(selected.df$Flow.Duration)

discretized <- discretize(data = selected.df[,to.discretize], method = 'quantile', breaks = c(5,5,3,2,5,5,5,2,5,5))

#diskretizovani dataset
cols.to.add <- setdiff(names(selected.df), names(discretized))
darknet.disc <- cbind(discretized, selected.df[,cols.to.add])
str(darknet.disc)


#Kreiranje train i test datasetova 
set.seed(1)
train.indices <- createDataPartition(darknet.disc$Label, p = .80, list = FALSE)

train.data <- darknet.disc[train.indices,]
test.data <- darknet.disc[-train.indices,]

table(train.data$Label)

#prilagodjavanje dataseta subsampling metodama
levels(train.data$Label) <- c("Darknet", "Non.Darknet")
levels(test.data$Label) <- c("Darknet", "Non.Darknet")

#kreiranje prvog NB modela
nb1 <- naiveBayes(Label ~ ., data = train.data)
print(nb1)
nb1.pred <- predict(nb1, newdata = test.data, type = 'class')

#kreiranje matrice konfuzije
nb1.cm <- table(true = test.data$Label, predicted = nb1.pred) 
nb1.cm
               #predicted
#true          Darknet Non-Darknet
#Darknet         121          45
#Non.Darknet      55         778

#izracunavanje evaluacionih metrika
nb1.eval <- compute.eval.metrics(nb1.cm)
nb1.eval
#accuracy precision    recall        F1     Kappa 
#0.8998999 0.6875000 0.7289157 0.7076023 0.6472781 

#control
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#downSample
set.seed(1)
down_inside <- train(x = train.data[,-12], 
                     y = train.data$Label,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl)

#up
ctrl$sampling <- "up"

set.seed(1)
up_inside <- train(x = train.data[,-12], 
                   y = train.data$Label,
                   method = "naive_bayes",
                   metric = "ROC",
                   trControl = ctrl)

#rose
ctrl$sampling <- "rose"

set.seed(1)
rose_inside <- train(x = train.data[,-12], 
                     y = train.data$Label,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl)

chisq.test(train.data$Label.1, train.data$Label)

#orig fit
ctrl$sampling <- NULL


set.seed(1)
orig_fit <- train(x = train.data[,-12], 
                  y = train.data$Label, 
                  method = "naive_bayes",
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
#original 0.9034109 0.9247402 0.9322764 0.9338103 0.9429574 0.9640536    0
#down     0.9037694 0.9251168 0.9319309 0.9334313 0.9416689 0.9627986    0
#up       0.9023800 0.9253396 0.9323295 0.9335233 0.9424546 0.9620815    0
#ROSE     0.9055992 0.9259720 0.9329319 0.9341606 0.9429199 0.9618937    0

up_inside$finalModel

##############################################################################
#Naive Bayes

nb2.pred <- predict(up_inside$finalModel, newdata = test.data, type = 'class')

#kreiranje matrice konfuzije
nb2.cm <- table(true = test.data$Label, predicted = nb2.pred) 
nb2.cm

              #predicted
#true          Darknet Non.Darknet
#Darknet         129          37
#Non.Darknet      65         768

#izracunavanje evaluacionih metrika
nb2.eval <- compute.eval.metrics(nb2.cm)
nb2.eval
#accuracy precision    recall        F1     Kappa 
#0.8978979 0.6649485 0.7771084 0.7166667 0.6548545 

#izracunavanje verovatnoce za svaku od klasa
nb2.pred.prob <- predict(up_inside$finalModel, newdata = test.data, type = "prob")
head(nb2.pred.prob) 

#ROC
nb2.roc <- roc(response = as.numeric(test.data$Label), predictor = nb2.pred.prob[,1], levels = c(2, 1))
nb2.roc$auc
#Area under the curve: 0.9323

plot.roc(nb2.roc, print.thres = TRUE, print.thres.best.method = "youden")
#0.069 (0.820, 0.904)

nb2.coords <- coords(nb2.roc, ret = c("accuracy", "spec", "sens", "precision", "thr"), x = "local maximas")
nb2.coords
#49 local maximas
#Za argument ret ne postoji opcija da se izabere kappa metrika tako da sam se trudio
#da izaberem threshold na osnovu sensitivity i specificity parametara, gde je prioritet
#prepoznavanje darknet saobracaja. Dakle sensitivity > specificity. Odlucio sam se za
#threshold sa plot.roc-a jer daje relativno visok specificiti (preko 0.8) dok je sensitivity
#iznad 0.9

#koriscenje izabranog thresholda
prob.threshold <- 0.069

nb3.pred <- ifelse(test = nb2.pred.prob[,1] >= prob.threshold, yes = "Darknet", no = "Non.Darknet")
nb3.pred <- as.factor(nb3.pred)

nb3.cm <- table(actual = test.data$Label, predicted = nb3.pred)
nb3.cm
               #predicted
#actual        Darknet Non.Darknet
#Darknet         150          16
#Non.Darknet     150         683

nb3.eval <- compute.eval.metrics(nb3.cm) 
nb3.eval
#accuracy precision    recall        F1     Kappa 
#0.8338338 0.5000000 0.9036145 0.6437768 0.5468199


#koriscenje izabranog thresholda za nesto bolji precision localMaximas 26
prob.threshold2 <- 4.227088e-01
#   accuracy specificity sensitivity precision    threshold
#26 0.8888889   0.9075630   0.7951807 0.6315789 4.227088e-01

nb4.pred <- ifelse(test = nb2.pred.prob[,1] >= prob.threshold2, yes = "Darknet", no = "Non.Darknet")
nb4.pred <- as.factor(nb4.pred)

nb4.cm <- table(actual = test.data$Label, predicted = nb4.pred)
nb4.cm
               #predicted
#actual        Darknet Non.Darknet
#Darknet         132          34
#Non.Darknet      77         756

nb4.eval <- compute.eval.metrics(nb4.cm) 
nb4.eval
#accuracy precision    recall        F1     Kappa 
#0.8888889 0.6315789 0.7951807 0.7040000 0.6367118 

#poredjenje rezultata
data.frame(rbind(nb1.eval, nb2.eval, nb3.eval, nb4.eval),
           row.names = c(paste("NB_", 1:4, sep = "")))
      #accuracy precision    recall        F1     Kappa
#NB_1 0.8998999 0.6875000 0.7289157 0.7076023 0.6472781
#NB_2 0.8978979 0.6649485 0.7771084 0.7166667 0.6548545
#NB_3 0.8338338 0.5000000 0.9036145 0.6437768 0.5468199
#NB_4 0.8888889 0.6315789 0.7951807 0.7040000 0.6367118