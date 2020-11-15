

# ---- load PCs ----x
loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/PCs"

# load SV: PCs 
psi <- readRDS(paste0(loc, '/train/psi.RDS'))


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

# --- artists ---x
artists <- unique(Y_train)



# ---- design col palette ----x
col_pal <- c()
cols <- c('steelblue', '#f08080', '#203650')
for (a in Y_train) {
  if (a ==artists[1]) col_pal <- c(col_pal, cols[1])
  if (a ==artists[2]) col_pal <- c(col_pal, cols[2])
  if (a ==artists[3]) col_pal <- c(col_pal, cols[3])
}


# ---- vis first 2 PCs ----x
plot_PCs <- function(pc1_2, title, Y, cls=NA) {
  
  # ---- design col palette ----x
  col_pal <- c()
  if (!is.na(cls)) cols <- sample(colors(), 3)
  for (a in Y) {
    if (a ==artists[1]) col_pal <- c(col_pal, cols[1])
    if (a ==artists[2]) col_pal <- c(col_pal, cols[2])
    if (a ==artists[3]) col_pal <- c(col_pal, cols[3])
  }
  
  plot(pc1_2, col=col_pal, 
       xlab='PC1',ylab='PC2', pch=20, lwd=0.1,
       main=title, font.main=1, frame=F)
  legend('topright',legend = artists, col = cols, pch=20, box.lwd = 0.1)
  
}



# ----- compute mapping to Training Data PCs -----x

# Data Snooping: use training mean & std for standardization
mns <- colMeans(X_train)
std <- apply(X_train, 2, sd)

# standardize
X_test_stan <- (X_test - mns)/std
X_val_stan  <- (X_val - mns)/std



# ----- Map to first 500 PCs ----x
X_train_PC <- X_train %*% psi[,1:500]
X_test_PC  <- X_test %*% psi[,1:500]
X_val_PC   <- X_val %*% psi[,1:500]





# ----- plot PCs ----x
par(mfrow=c(1,1))
plot_PCs(X_train_PC[,1:2], 'PCs Training Data', Y_train)
plot_PCs(X_test_PC[,1:2], 'PCs Testing Data', Y_test)
plot_PCs(X_val_PC[,1:2], 'PCs Validation Data', Y_val)






# --------- ---------------------------------- --------- SVM --------- ---------------------------------- ---------x


library(e1071)   


X_range <- 1:50

res <- c()
for (x in X_range) {
  X         <- X_train_PC[,1:x]
  data      <- data.frame(X=X, Y=factor(Y_train))
  svm_multi <- svm(Y~., data=data, method='C-classification', kernal='radial')
  
  # predict on Val set
  newdata <- data.frame(X=X_val_PC[,1:x], Y=factor(Y_val))
  preds <- predict(svm_multi, newdata)
  ac <- mean(Y_val == matrix(preds))
  
  # store 
  res <- rbind(res, cbind(dims=x, acc=ac, n_support_vectors=dim(svm_multi$SV)[1]))
  
}

par(mfrow=c(2,1))
plot(res[,1], res[,2], 'l', col='steelblue', frame=F, main='Validation Set Accuracy', font.main=1,
     ylab='Validation Accuracy', xlab='PCs (dimensionality)', lwd=2)
abline(v = which.max(res[,2]), lty=6, col='darkred', lwd=1.5)
abline(v = which.min(res[,3]), lty=5, col='darkred', lwd=1.5)
legend('bottomright', legend = c('no. SVs', 'min SVs', 'max Val Acc'),
       lty=c(1,6,5), col=c('steelblue', 'darkred', 'darkred'), box.lty = 2, box.lwd = 0.4, cex=0.5)


plot(res[,1], res[,3], 'l', col='steelblue', frame=F, main='No. Support Vectors', font.main=1,
     ylab='no. support vectors', xlab='PCs (dimensionality)', lwd=2)
abline(v = which.max(res[,2]), lty=6, col='darkred', lwd=1.5)
abline(v = which.min(res[,3]), lty=5, col='darkred', lwd=1.5)
legend('bottomright', legend = c('no. SVs', 'min SVs', 'max Val Acc'),
       lty=c(1,6,5), col=c('steelblue', 'darkred', 'darkred'), box.lty = 2, box.lwd = 0.4, cex=0.5)



# --- best SVM ---x
x <- res[which.min(res[,3]),][1]

X         <- X_train_PC[,1:x]
data      <- data.frame(X=X, Y=factor(Y_train))
svm_multi <- svm(Y~., data=data, method='C-classification', kernal='radial')


# Test set
newdata <- data.frame(X=X_test_PC[,1:x], Y=factor(Y_test))
preds <- predict(svm_multi, newdata)
ac <- mean(Y_test == matrix(preds))

print(paste0('acc on test set:', round(ac, 3)))


# visualize
plot(svm_multi, data)









# ----- compute acc per class -----x
accs <- c()
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
mean(c(0.8823529,  0.8148148, 0.962963 ))






# ------ ------------------------ tune gamma ------------------------  -----x
gam_range <- seq(0.01,5,length.out = 100)

res <- c()
for (g in gam_range) {
  X         <- X_train_PC[,1:x]
  data      <- data.frame(X=X, Y=factor(Y_train))
  svm_multi <- svm(Y~., data=data, method='C-classification', kernal='radial', gamma=g)
  
  # predict on Val set
  newdata <- data.frame(X=X_val_PC[,1:x], Y=factor(Y_val))
  preds <- predict(svm_multi, newdata)
  ac <- mean(Y_val == matrix(preds))
  
  # store 
  res <- rbind(res, cbind(gamma=g, acc=ac, n_support_vectors=dim(svm_multi$SV)[1]))
  
}



par(mfrow=c(2,1))
plot(res[,1], res[,2], 'l', col='steelblue', frame=F, main='Validation Set Accuracy', font.main=1,
     ylab='Validation Accuracy', xlab='Gamma', lwd=2)
abline(v = res[,1][which.max(res[,2])], lty=6, col='darkred', lwd=1.5)
abline(v = res[,1][which.min(res[,3])], lty=5, col='darkred', lwd=1.5)
legend('bottomright', legend = c('no. SVs', 'min SVs', 'max Val Acc'),
       lty=c(1,6,5), col=c('steelblue', 'darkred', 'darkred'), box.lty = 2, box.lwd = 0.4, cex=0.5)


plot(res[,1], res[,3], 'l', col='steelblue', frame=F, main='No. Support Vectors', font.main=1,
     ylab='no. support vectors', xlab='Gamma', lwd=2)
abline(v = res[,1][which.max(res[,2])], lty=6, col='darkred', lwd=1.5)
abline(v = res[,1][which.min(res[,3])], lty=5, col='darkred', lwd=1.5)
legend('bottomright', legend = c('no. SVs', 'min SVs', 'max Val Acc'),
       lty=c(1,6,5), col=c('steelblue', 'darkred', 'darkred'), box.lty = 2, box.lwd = 0.4, cex=0.5)




# --- gamma ----x
best <- res[which.min(res[,3]),]
g <- best[1]





# --- best SVM ---x
X         <- X_train_PC[,1:x]
data      <- data.frame(X=X, Y=factor(Y_train))
svm_multi <- svm(Y~., data=data, method='C-classification', kernal='radial', gamma=g)


# Test set
newdata <- data.frame(X=X_test_PC[,1:x], Y=factor(Y_test))
preds <- predict(svm_multi, newdata)
ac <- mean(Y_test == matrix(preds))

print(paste0('acc on test set:', round(ac, 3)))


# visualize
plot(svm_multi, data)




# ----- compute acc per class -----x
accs <- c()
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

# ---- confusion table ----x
xtab <- table(preds, Y_test)
xtab


