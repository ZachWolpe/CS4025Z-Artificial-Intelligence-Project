
# ----------------------------------------- Load Data -----------------------------------------x

setwd("~/Desktop/Artificial Intelligence/project")


library(jpeg)
library(magick)
library(raster)
library(Rmagic)


# ---- load data ----x
# train
loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/vectorized images/train"
X_train <- readRDS(paste0(loc, '/vec_train_data.rds'))
Y_train <- readRDS(paste0(loc, '/vec_train_labels.rds'))

# test 
loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/vectorized images/test"
X_test <- readRDS(paste0(loc, '/vec_test_data.rds'))
Y_test <- readRDS(paste0(loc, '/vec_test_labels.rds'))

# val
loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/vectorized images/val"
X_val <- readRDS(paste0(loc, '/vec_val_data.rds'))
Y_val <- readRDS(paste0(loc, '/vec_val_labels.rds'))

# artists
artists <- unique(Y_train)



# ----------------------------------------- Visualize PCs -----------------------------------------x

plot(X_train_PC[,1], y=rep(0,length(X_train_PC[,1])), col=col_pal,
     ylab='', xlab='PC1', main='Principal Component 1', font.main=1, frame=F)
legend('topright',legend = artists, col = unique(col_pal), pch=20, box.lwd = 0.1)


# ---- vis first 2 PCs ----x
par(mfrow=c(1,1))
plot(X_train_PC[,1], X_train_PC[,2], col=col_pal, 
     xlab='PC1',ylab='PC2', pch=20, lwd=0.1,
     main='Principal Component Analysis', font.main=1, frame=F)
legend('topright',legend = artists, col = cols, pch=20, box.lwd = 0.1)











# ----------------------------- Pick Best Model -------------------------------x
library(tidyverse)
library(caret)
library(nnet)

# ------ test varying number of predictors ------x
acc <- c()
for (samp in 1:50) {
  
  
  # --- training data ---x
  data <- data.frame(X=X_train_PC[,1:samp], Y=factor(Y_train))
  
  
  # --- fit model ---x
  model <- nnet::multinom(Y~., data = data)

  # --- training data ---x
  predicted.classes <- model %>% predict(data)
  train_acc <- mean(predicted.classes == data$Y)
  
  # --- testing data ---x
  test_data <- data.frame(X=X_test_PC[,1:samp], Y=factor(Y_test))
  predicted.classes <- model %>% predict(test_data)
  test_acc <- mean(predicted.classes == test_data$Y)
  
  # --- val data ---x
  val_data <- data.frame(X=X_val_PC[,1:samp], Y=factor(Y_val))
  predicted.classes <- model %>% predict(val_data)
  val_acc <- mean(predicted.classes == val_data$Y)
  
  acc <- rbind(acc, cbind(train_acc, test_acc, val_acc))
  
  
}


# best test acc 
best_test_acc <- which.max(acc[,2])




# visualize model
plot(1:50, acc[,1], 'l', frame=F, main='Multinomial Logistic Regression', col='blue', lty=1, font.main=1, ylab='accuracy', xlab='PCs used', lwd=1)
lines(1:50, acc[,2], col='red', lty=1, lwd=1)
# lines(1:50, acc[,3], col='orange', lty=1, lwd=1)
abline(v=best_test_acc, col='darkgreen', lty=2)
legend('bottomright', legend = c('Train Accuracy', 'Test  Accuracy', 'Best  Fit'),
       col=c('blue','red', 'darkgreen'), lty=c(1,1,2), box.lwd = 0.2)







# -----------------------------  Best Model -------------------------------x

# --- number of PCs ---x
samp <- best_test_acc

# --- training data ---x
data <- data.frame(X=X_train_PC[,1:samp], Y=factor(Y_train))

# --- fit model ---x
model <- nnet::multinom(Y~., data = data)
summary(model)

# --- testing data ---x
test_data <- data.frame(X=X_test_PC[,1:samp], Y=factor(Y_test))
predicted.classes <- model %>% predict(test_data)
head(predicted.classes)
# Model accuracy
print(paste0('model accuracy: ', round(mean(predicted.classes == test_data$Y), 3)))


# ---- confusion table ----x
xtab <- table(predicted.classes, test_data$Y)
xtab


# ----- compute acc per class -----x
accs <- c()
preds <- predicted.classes
for (p in 1:length(preds)) {
  if (Y_test[p] == preds[p]) {
    # correct
    accs[[Y_test[p]]] <- c(accs[[Y_test[p]]], 1)
  }
  else accs[[Y_test[p]]] <- c(accs[[Y_test[p]]], 0)
  
}

# ---- Accuracy per Class ----
acx <- lapply(accs, mean)
acx









# -----------------------------  2 PCs -------------------------------x

# --- number of PCs ---x
samp <- 2

# --- training data ---x
data <- data.frame(X=X_train_PC[,1:samp], Y=factor(Y_train))

# --- fit model ---x
model <- nnet::multinom(Y~., data = data)
summary(model)

# --- testing data ---x
test_data <- data.frame(X=X_test_PC[,1:samp], Y=factor(Y_test))
predicted.classes <- model %>% predict(test_data)
head(predicted.classes)
# Model accuracy
print(paste0('model accuracy: ', round(mean(predicted.classes == test_data$Y), 3)))


# ---- confusion table ----x
xtab <- table(predicted.classes, test_data$Y)
xtab


# ----- compute acc per class -----x
accs <- c()
preds <- predicted.classes
for (p in 1:length(preds)) {
  if (Y_test[p] == preds[p]) {
    # correct
    accs[[Y_test[p]]] <- c(accs[[Y_test[p]]], 1)
  }
  else accs[[Y_test[p]]] <- c(accs[[Y_test[p]]], 0)
  
}

# ---- Accuracy per Class ----
acx <- lapply(accs, mean)
acx



