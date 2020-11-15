

# ----------------------------------------- PCA -----------------------------------------x

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






# ----- mean image: Rembrandt -----x
mean_std_image <- function(artist='Rembrandt') {
  
  # subset 
  sub <- X_train[Y_train == artist, ]
 
  # mean
  mean_pixels <- apply(sub, 2, mean)
  
  # magick
  x <- array(mean_pixels, c(224,224,3))    
  x <- magick::image_read(x / 255)
  
  # sd
  xs <- apply(sub,2,sd)
  xs <- array(xs, c(224,224,3))    
  xs <- magick::image_read(xs / 255) 
  
 return(list(mn=x, std=xs))
}


# call
im_rem <- mean_std_image('Rembrandt')
im_al <- mean_std_image('Albrecht_Dürer')
im_af <- mean_std_image('Alfred_Sisley')

par(mfrow=c(3,2))
par(mfrow=c(1,1))
plot(im_rem$mn)
plot(im_rem$std)
plot(im_al$mn)
plot(im_al$std)
plot(im_af$mn)
plot(im_af$std)




# -------- compute PCA -------- x
pca <- prcomp(X)



# ---- design col palette ----x
col_pal <- c()
cols <- c('steelblue', '#f08080', '#203650')
for (a in Y_train) {
  if (a ==artists[1]) col_pal <- c(col_pal, cols[1])
  if (a ==artists[2]) col_pal <- c(col_pal, cols[2])
  if (a ==artists[3]) col_pal <- c(col_pal, cols[3])
}

# ---- vis first 2 PCs ----x
par(mfrow=c(1,1))
plot(pca$x[,1], pca$x[,2], col=col_pal, 
     xlab='PC1',ylab='PC2', pch=20, lwd=0.1,
     main='Principal Component Analysis', font.main=1, frame=F)
legend('topright',legend = artists, col = cols, pch=20, box.lwd = 0.1)


# ---- scree plot ----x
library(factoextra)
fviz_eig(pca)

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







# ---- reconstruct images with q eigenvalues ----x
# ---- hyper-parameters ----x
m    <- dim(X_train)[2]
n    <- dim(X_train)[1]
dims <- c(3,224,224)

# ---- scale data ----x
X <- apply(X_train, 2, scale)
x <- X

# Calculate Eigenvalues: SVD method
N   = dim(x)[1]
SIG = 1/N*(x %*% t(x))
sv  = eigen(SIG)

# Construct components:

# Construct components (slide 12/18):
temp = t(x) %*% sv$vectors
phi  = temp
temp = temp*matrix(1/sqrt(sv$values), dim(X)[2], dim(X)[1], byrow = T)






# ----- How much information? ------x
par(mfrow=c(1,1))
plot(cumsum(sv$values)/sum(sv$values),type = 'l', main='Variation Explained', font.main=1, frame=F,
     col='#890000', ylab='variation', xlab='Eigen Vectors')
abline(h = 0.95, lty = 3, col='steelblue')
abline(v = ss,lty = 3, col='steelblue')


# take new sample
ii = sample(1:N, 1)
print(paste0('sample: ', ii))

# Compare construction to full image:
par(mfrow = c(2,5))
ss = c(10,50,100,250,500)
for(i in ss) {
  Xhat = (x[ii,] %*% temp[,1:i]) %*% t(temp[,1:i])

  xp <- array(Xhat, c(224,224,3))    
  xp <- magick::image_read(xp )
  plot(xp)
}



for( i in ss){
  xp <- array(X_train[ii,], c(224,224,3))    
  xp <- magick::image_read(xp/255)
  plot(xp)
  }







# ---- save all the processed data ----x
sav_loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/PCs"

library(icesTAF)
# create dirss
mkdir(sav_loc)
mkdir(paste0(sav_loc, '/train'))

# save 
saveRDS(temp, paste0(sav_loc, '/train/psi.RDS'))






