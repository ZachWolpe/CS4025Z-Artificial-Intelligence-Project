



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


# ----------------------------------------- Fourier Transform: Illustratiion -----------------------------------------x

# --- true inifinte space function ---x
ms <- function(s) exp(-abs(s))

# --- inner functions ---x
tau <- function(s) {
  if (cos_)  return(cos((r * pi * s)/L) * ms(s))
  if (!cos_) return(sin((r * pi * s)/L) * ms(s))
} 

# --- integrate ---x
a_r <- function() return(1/L * integrate(tau, -L, L)$value)


# --- specify params ---x
r=0; ms=ms; L=1; cos_=TRUE
a <- c()
b <- c()

for (i in 0:5) {
  r=i
  cos_=TRUE
  ai <- a_r()
  cos_=FALSE
  bi <- a_r()
  
  a <- c(a, ai); b <- c(b, bi)
}

# example 1 
# r=3; ms=ms; L=1; cos_=TRUE
x <- seq(-L,L,length.out = 100)
y <- ms(x)


s <- x
yhat_z <- c()
for (t in 2:6) {
  # many differnt fourier transforms
  yhat_z[[t-1]] <- a[1]/2 
    for (i in 2:t) {
      r=i-1
      yhat_z[[t-1]] <- yhat_z[[t-1]] + a[i] * cos((r * pi * s)/L)  + b[i] * sin((r * pi * s)/L)
    }
}

par(mfrow=c(1,1))
cols <- sample(colors(), 5)
plot(x,y, type='l', frame=F,tlty=2, main='Fourier Transform: fc = Fourier Coefficients', font.main=1, col='darkgreen', lwd=2)
for (i in 1:5) {
  lines(x,yhat_z[[i]],lty=4, col=cols[i], 'l', lwd=2)
}
legend('topright', legend = c('target', '1 fc', '2 fcs', '3 fcs', '4 fcs', '5 fcs'),
       col = c('darkgreen', cols),
       lty = c(1,4,4,4,4,4), box.lwd = 0.2)





# ----------------------------------------- LDA -----------------------------------------x


library(MASS)

data <- data.frame(X=X_train, Y=factor(Y_train))
samp <- sample(dim(data)[1], size = 10, replace = F)
data <- data[samp,] 

X_train_lda <- lda(Y~., data=data)


# ---- vis first 2 PCs ----x
par(mfrow=c(1,1))
plot(pca$x[,1], pca$x[,2], col=col_pal, 
     xlab='PC1',ylab='PC2', pch=20, lwd=0.1,
     main='Principal Component Analysis', font.main=1, frame=F)
legend('topright',legend = artists, col = cols, pch=20, box.lwd = 0.1)


# ---- 3 PCs ----x
library(scatterplot3d) 
scatterplot3d(pca$x[,1:3], color = col_pal)



# ---- 3D PCA Plot ----x
library(plotly)
fig <- plot_ly(data.frame(pca$x[,1:3]), x= ~PC1, y= ~PC2, z= ~PC3, color=col_pal, size = 1
               #colors = c('#BF382A', '#0C4B8E', '#31baba', '#f4c2c2', '#a10e58')
)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))
fig





# ----------------------------------------- Multinomial Logistic Regression -----------------------------------------x


library(tidyverse)
library(caret)
library(nnet)

X <- X_train_PC[,2]
data <- data.frame(X=X, Y=factor(Y_train))


model <- nnet::multinom(Y~., data = data)
summary(model)


test_data <- data.frame(X=X_test_PC[,2], Y=factor(Y_test))
predicted.classes <- model %>% predict(test_data)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test_data$Y)



# ----------------------------------------- Wavelet Transform: ART Data -----------------------------------------x
library(WaveletComp)

x = periodic.series(start.period = 50, length = 1000) 
x = x + 0.2*rnorm(1000) # add some noise

my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250, lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 10)

plot.ts(x)


wt.image(my.w, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))


reconstruct(my.w, plot.waves = FALSE, lwd = c(1,2), legend.coords = "bottomleft", ylim = c(-1.8, 1.8))




x <- X_train[1,]

# magick --> matrix
# x <- as.integer(img[[1]])

# vectorize 
x <- as.vector(x)

# matrix --> magick 
x <- array(x, c(224,224,3))      # reshape tensor
xim <- magick::image_read(x / 255) # (+ scale)



dwt.3d(x, wf, J=4, boundary="periodic")
idwt.3d(y)


y <- dwt.3d(x, 'haar', J=4, boundary="periodic")
i3d <- idwt.3d(y)




library(waveslim)

## Xbox image
data(xbox)
xbox.dwt <- dwt.2d(xbox, "haar", 3)
par(mfrow=c(1,1), pty="s")
plot.dwt.2d(xbox.dwt)
par(mfrow=c(2,2), pty="s")
image(1:dim(xbox)[1], 1:dim(xbox)[2], xbox, xlab="", ylab="",
      main="Original Image")
image(1:dim(xbox)[1], 1:dim(xbox)[2], idwt.2d(xbox.dwt), xlab="", ylab="",
      main="Wavelet Reconstruction")
image(1:dim(xbox)[1], 1:dim(xbox)[2], xbox - idwt.2d(xbox.dwt),
      xlab="", ylab="", main="Difference")
## Daubechies image
data(dau)
par(mfrow=c(1,1), pty="s")
image(dau, col=rainbow(128))
sum(dau^2)
dau.dwt <- dwt.2d(dau, "d4", 3)
plot.dwt.2d(dau.dwt)
sum(plot.dwt.2d(dau.dwt, plot=FALSE)^2)









