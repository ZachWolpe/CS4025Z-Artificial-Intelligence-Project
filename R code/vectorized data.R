

# ----------------------------------------- Resize, Flatten & Save Data -----------------------------------------x

setwd("~/Desktop/Artificial Intelligence/project")


library(jpeg)
library(magick)
library(raster)
library(Rmagic)

loc <- "~/Desktop/Artificial Intelligence/project/datasets/final"
loc_train <- "~/Desktop/Artificial Intelligence/pythonProject/Project/datasets/final_split/train"
loc_test  <- "~/Desktop/Artificial Intelligence/pythonProject/Project/datasets/final_split/test"
loc_val   <- "~/Desktop/Artificial Intelligence/pythonProject/Project/datasets/final_split/val"


# ---- create file paths ----x
paths <- list()
for (a in list.files(loc_train)) {
  paths[[a]] <- list.files(paste0(loc_train, '/', a))
}
print(names(paths))




# ------ create vectorized dataset: Training ------x
flattened_data <- c()
labels         <- c()
for (art in 1:length(paths)) {
  for (painting in paths[[art]]) {
    # labels 
    labels <- c(labels, names(paths)[art])
    
    # path
    path <- paste0(loc, '/', names(paths)[art], '/', painting)
    
    # load image
    img <- magick::image_read(path)
    img <- magick::image_scale(image = img, geometry = '226x226!')
    img <- magick::image_crop(image = img, '224x224!')
    
    # magick --> matrix
    x <- as.integer(img[[1]])
    
    # vectorize 
    x <- as.vector(x)
    
    # matrix --> magick 
    # x <- array(x, c(224,224,3))      # reshape tensor
    # x <- magick::image_read(x / 255) # (+ scale)
    
    # save
    flattened_data <- rbind(flattened_data, x)
  }
}

# ---- save ---x
sav_loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/vectorized images/train"
saveRDS(flattened_data, paste0(sav_loc, '/vec_train_data.rds'))
saveRDS(labels, paste0(sav_loc, '/vec_train_labels.rds'))





# ------ create vectorized dataset: Testing ------x

# ---- create file paths ----x
paths <- list()
for (a in list.files(loc_test)) {
  paths[[a]] <- list.files(paste0(loc_test, '/', a))
}
print(names(paths))

flattened_data <- c()
labels         <- c()
for (art in 1:length(paths)) {
  for (painting in paths[[art]]) {
    labels <- c(labels, names(paths)[art])
    path <- paste0(loc, '/', names(paths)[art], '/', painting)
    img <- magick::image_read(path)
    img <- magick::image_scale(image = img, geometry = '226x226!')
    img <- magick::image_crop(image = img, '224x224!')
    x <- as.integer(img[[1]])
    x <- as.vector(x)
    flattened_data <- rbind(flattened_data, x)
  }
}

# ---- save ---x
sav_loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/vectorized images/test"
saveRDS(flattened_data, paste0(sav_loc, '/vec_test_data.rds'))
saveRDS(labels, paste0(sav_loc, '/vec_test_labels.rds'))





# ------ create vectorized dataset: Validation ------x

# ---- create file paths ----x
paths <- list()
for (a in list.files(loc_val)) {
  paths[[a]] <- list.files(paste0(loc_val, '/', a))
}
print(names(paths))

flattened_data <- c()
labels         <- c()
for (art in 1:length(paths)) {
  for (painting in paths[[art]]) {
    labels <- c(labels, names(paths)[art])
    path <- paste0(loc, '/', names(paths)[art], '/', painting)
    img <- magick::image_read(path)
    img <- magick::image_scale(image = img, geometry = '226x226!')
    img <- magick::image_crop(image = img, '224x224!')
    x <- as.integer(img[[1]])
    x <- as.vector(x)
    flattened_data <- rbind(flattened_data, x)
  }
}

# ---- save ---x
sav_loc <- "~/Desktop/Artificial Intelligence/project/datasets/R datasets/vectorized images/val"
saveRDS(flattened_data, paste0(sav_loc, '/vec_val_data.rds'))
saveRDS(labels, paste0(sav_loc, '/vec_val_labels.rds'))





















