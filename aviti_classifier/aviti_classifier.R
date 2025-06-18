############################################################
# Script: aviti_classifier.R
# Purpose: Train a simple image classifier for Aviti images
#          with three classes ("ok", "overloaded", "underloaded")
#          and use it to classify new images.
# developed on M2 mac, not meant to work on a different arch
# Author: SP@NC; v1.0
# Date: 2025-06-18
############################################################

# ==============================
# 0. EDITABLE PATHS ----
# ==============================

# Define the paths to your training and classification images here:
train_images_dir    <- "train"
classify_images_dir <- "classify"

# ==============================
# 1. Environment Setup ----
# ==============================

## the conda env tf-2.6 should be built prior to running this code
#conda create --platform osx-64 --name tf-2.6 python=3.9
#conda activate tf-2.6
## For R integration (if using R via reticulate)
## conda install r-base r-essentials
## For Python packages (example: tensorflow, keras, pillow, etc.)
#conda install tensorflow=2.6 keras=2.6 pillow

CONDA_PREFIX <- "/opt/miniconda3"
library(reticulate)
use_condaenv("tf-2.6", required = TRUE)
use_python(file.path(CONDA_PREFIX, "envs", "tf-2.6", "bin", "python"), required = TRUE)
RETICULATE_PYTHON <- file.path(CONDA_PREFIX, "envs", "tf-2.6", "bin", "python")

# ==============================
# 2. Load Required Libraries ----
# ==============================

library(keras)
library(tensorflow)

# ==============================
# 3. Data Preparation ----
# ==============================

# Set image dimensions (adjust as needed for speed/accuracy)
hor <- 1024
ver <- 1024
# For faster training/testing, you can use smaller images:
# hor <- 128; ver <- 128

# Set batch size
batch.size <- 1
# For larger datasets, increase batch size (e.g., batch.size <- 16)

# Load training images from directory structure
train_ds <- image_dataset_from_directory(
  train_images_dir,
  labels = "inferred",
  label_mode = "categorical",
  color_mode = "rgb",
  batch_size = batch.size,
  image_size = c(hor, ver)
)

# ==============================
# 4. Model Definition ----
# ==============================

model <- keras_model_sequential(
  layers = list(
    layer_input(shape = c(hor, ver, 3)),
    layer_rescaling(scale = 1/255),  # Normalize pixel values to [0, 1]
    layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu'),
    layer_max_pooling_2d(pool_size = c(2,2)),
    layer_flatten(),
    layer_dense(units = 3, activation = 'softmax') # 3 classes
  )
)

model %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = 'accuracy'
)

# ==============================
# 5. Model Training ----
# ==============================

num.epochs <- 2
# For more data, increase epochs (e.g., num.epochs <- 10)
model %>% fit(train_ds, epochs = num.epochs)

# ==============================
# 6. Predict New Images ----
# ==============================

# List all images to classify
img_files <- list.files(classify_images_dir, full.names = TRUE)

# Preprocessing function for a single image
preprocess_image <- function(img_path, target_size = c(hor, ver)) {
  img <- image_load(img_path, target_size = target_size)
  x <- image_to_array(img)
  x <- array_reshape(x, c(1, dim(x)))  # Add batch dimension
  return(x)
}

# Get class names in order (alphabetical by folder)
class_names <- train_ds$class_names

# Loop over images, preprocess, and predict class
for (img_path in img_files) {
  x <- preprocess_image(img_path)
  preds <- model %>% predict(x)
  pred_index <- which.max(preds)  # 1-based index of highest probability
  pred_class <- class_names[pred_index]  # Map index to class name
  cat(basename(img_path), "predicted class:", pred_class, "\n")
}

# ==============================
# 7. End of Script ----
# ==============================

# Notes:
# - For best results, use more images per class.
# - Consider adding a validation split and/or data augmentation for real applications.
# - This script assumes class folders are named 'ok', 'overloaded', and 'underloaded'.
