#funkcije koje se ponavljaju

#eval.metrics
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  N <- sum(cmatrix)
  No <- sum(diag(cmatrix))
  Ne <- 1 / N * sum(colSums(cmatrix) * rowSums(cmatrix))
  Kappa <-( (No - Ne) / (N - Ne) )
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1, Kappa = Kappa)
}

#pROC
test_roc <- function(model, data) {
  roc_obj <- roc(data$Label, 
                 predict(model, data, type = "prob")[, "Darknet"],
                 levels = c("Non.Darknet", "Darknet"))
  ci(roc_obj)
}