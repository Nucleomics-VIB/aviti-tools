#!/usr/bin/env Rscript

# AVITI Polony Image QC and Summary Script with Adaptive Thresholding
# script: aviti_polony_qc_adaptive.R
# Author: SP@NC; nucleomics@vib.be
# Date: 2025-06-19
# Description: Analyzes pixels in an AVITI PNG image by color with adaptive thresholding

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}

pacman::p_load(
  imager,
  magrittr,
  dplyr,
  knitr,
  stats,
  ggplot2,
  gridExtra,
  grid
)

# ---- Argument Parsing ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  cat("Usage: ./aviti_polony_qc.R input_image.png\n")
  quit(status = 1)
}
# Keep hardcoded path for debugging as requested
imgfile <- args[1]
prefix <- sub("\\.png$", "", basename(imgfile))

# Check if file exists
if (!file.exists(imgfile)) {
  cat("Error: File not found:", imgfile, "\n")
  quit(status = 1)
}

# ---- Parameters ----
square_size <- 250

# Create output directory
output_dir <- "QC_results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Helper function to create output path
get_output_path <- function(filename) {
  file.path(output_dir, filename)
}

# Helper function to close plot devices without messages
quiet_dev_off <- function() {
  invisible(capture.output(dev.off()))
}

# ---- Load Full Image ----
tryCatch({
  full_im <- load.image(imgfile)
}, error = function(e) {
  cat("Error loading image:", e$message, "\n")
  quit(status = 1)
})

# Check dimensions and adjust square_size if needed
img_width <- dim(full_im)[1]
img_height <- dim(full_im)[2]
square_size <- min(square_size, img_width, img_height)

cat("Image dimensions:", img_width, "x", img_height, "\n")
cat("Using central square size:", square_size, "x", square_size, "\n")

# Calculate coordinates for the central square
x_center <- floor(img_width / 2)
y_center <- floor(img_height / 2)

# Calculate offsets that work for any square_size (odd or even)
# Left/top offset = floor(N/2)
# Right/bottom offset = N - 1 - floor(N/2)
left_offset <- floor(square_size / 2)
right_offset <- square_size - 1 - floor(square_size / 2)

# Calculate actual extraction coordinates
x_start <- max(1, x_center - left_offset)
x_end <- min(img_width, x_center + right_offset)
y_start <- max(1, y_center - left_offset)
y_end <- min(img_height, y_center + right_offset)

cat("Extracting central square from x =", x_start, "to", x_end, "and y =", y_start, "to", y_end, "\n")

# Extract the central region
im <- imsub(full_im, x %inr% c(x_start, x_end), y %inr% c(y_start, y_end))

# Verify the extracted dimensions
extracted_width <- dim(im)[1]
extracted_height <- dim(im)[2]
cat("Extracted region dimensions:", extracted_width, "x", extracted_height, "\n")

# Verification check
if(extracted_width != square_size || extracted_height != square_size) {
  cat("WARNING: Extracted region size does not match requested size!\n")
  cat("This may be due to image boundary constraints.\n")
}

# ---- Image Analysis for Adaptive Thresholding ----
img_array <- as.array(im)
img_dim <- dim(img_array)
cat("Image array dimensions:", paste(img_dim, collapse=" x "), "\n")

# Extract channel data
r_channel <- img_array[,,1,1]
g_channel <- img_array[,,1,2]
b_channel <- img_array[,,1,3]

# Calculate statistics for adaptive thresholding
calculate_adaptive_thresholds <- function(r_channel, g_channel, b_channel) {
  # Flatten the channels to vectors
  r_values <- as.vector(r_channel)
  g_values <- as.vector(g_channel)
  b_values <- as.vector(b_channel)
  
  # Calculate intensity distribution
  all_intensities <- c(r_values, g_values, b_values)
  median_intensity <- median(all_intensities)
  q3_intensity <- quantile(all_intensities, 0.75)
  
  # Calculate color-specific percentiles 
  r_q90 <- quantile(r_values, 0.9)
  g_q90 <- quantile(g_values, 0.9)
  b_q90 <- quantile(b_values, 0.9)
  
  # Calculate color differences for pixels where one channel dominates
  r_dominated <- which(r_values > g_values & r_values > b_values)
  g_dominated <- which(g_values > r_values & g_values > b_values)
  b_dominated <- which(b_values > r_values & b_values > g_values)
  
  # Calculate typical color differences in the dominated pixels
  if (length(r_dominated) > 0) {
    r_diff <- median((r_values[r_dominated] - pmax(g_values[r_dominated], b_values[r_dominated])) / 
                    r_values[r_dominated] * 100)
  } else {
    r_diff <- 15  # Default if no clear red pixels
  }
  
  if (length(g_dominated) > 0) {
    g_diff <- median((g_values[g_dominated] - pmax(r_values[g_dominated], b_values[g_dominated])) / 
                    g_values[g_dominated] * 100)
  } else {
    g_diff <- 15  # Default if no clear green pixels
  }
  
  if (length(b_dominated) > 0) {
    b_diff <- median((b_values[b_dominated] - pmax(r_values[b_dominated], g_values[b_dominated])) / 
                    b_values[b_dominated] * 100)
  } else {
    b_diff <- 15  # Default if no clear blue pixels
  }
  
  # Adaptive intensity threshold - use the 75th percentile as reference
  # but scale it to ensure it's sensitive enough (minimum 0.2, maximum 0.5)
  intensity_threshold <- max(0.2, min(0.5, q3_intensity * 0.8))
  
  # Determine the difference threshold based on the median of differences
  # but keep it within reasonable bounds
  diff_threshold <- max(10, min(40, median(c(r_diff, g_diff, b_diff)) * 0.9))
  
  return(list(
    intensity_threshold = intensity_threshold,
    diff_threshold = diff_threshold,
    r_threshold = max(0.15, min(0.4, r_q90 * 0.7)),
    g_threshold = max(0.15, min(0.4, g_q90 * 0.7)),
    b_threshold = max(0.15, min(0.4, b_q90 * 0.7)),
    stats = list(
      median_intensity = median_intensity,
      q3_intensity = q3_intensity,
      r_diff = r_diff,
      g_diff = g_diff,
      b_diff = b_diff
    )
  ))
}

# Get adaptive thresholds
thresholds <- calculate_adaptive_thresholds(r_channel, g_channel, b_channel)

cat("\nAdaptive Thresholding Parameters:\n")
cat("  Intensity threshold:", sprintf("%.3f", thresholds$intensity_threshold), "\n")
cat("  Difference threshold:", sprintf("%.3f", thresholds$diff_threshold), "\n")
cat("  Red channel threshold:", sprintf("%.3f", thresholds$r_threshold), "\n")
cat("  Green channel threshold:", sprintf("%.3f", thresholds$g_threshold), "\n")
cat("  Blue channel threshold:", sprintf("%.3f", thresholds$b_threshold), "\n")
cat("  Median intensity:", sprintf("%.3f", thresholds$stats$median_intensity), "\n")
cat("  Q3 intensity:", sprintf("%.3f", thresholds$stats$q3_intensity), "\n\n")

# ---- Adaptive Pixel Classification ----
# Function to classify a pixel by color using adaptive thresholds
classify_pixel_color_adaptive <- function(r, g, b, thresholds) {
  # Check if overall intensity is too low
  if (max(r, g, b) < thresholds$intensity_threshold) {
    return("Other")
  }
  
  max_val <- max(r, g, b)
  
  # Calculate relative differences
  r_g_diff <- (r - g) / max_val * 100
  r_b_diff <- (r - b) / max_val * 100
  g_b_diff <- (g - b) / max_val * 100
  
  # Classification with adaptive requirements
  if (r > g && r > b && 
      r_b_diff > thresholds$diff_threshold && 
      r >= thresholds$r_threshold) {
    if (g > 0.85 * r && g >= thresholds$g_threshold) return("Yellow")
    return("Red")
  }
  else if (g > r && g > b && 
           g_b_diff > thresholds$diff_threshold && 
           g >= thresholds$g_threshold) {
    return("Green")
  }
  else if (b > r && b > g && 
           (b - max(r, g)) / max_val * 100 > thresholds$diff_threshold && 
           b >= thresholds$b_threshold) {
    return("Blue")
  }
  else if (r > b && g > b && 
           abs(r - g) < 0.1 * max(r, g) && 
           min((r-b), (g-b)) / max_val * 100 > thresholds$diff_threshold &&
           r >= thresholds$r_threshold && 
           g >= thresholds$g_threshold) {
    return("Yellow")
  }
  
  return("Other")
}

# Create a classification array for each pixel
pixel_classes <- array("", dim=c(img_dim[1], img_dim[2]))
color_counts <- list(Red=0, Green=0, Blue=0, Yellow=0, Other=0)

for (i in 1:img_dim[1]) {
  for (j in 1:img_dim[2]) {
    r <- img_array[i, j, 1, 1]
    g <- img_array[i, j, 1, 2]
    b <- img_array[i, j, 1, 3]
    
    color <- classify_pixel_color_adaptive(r, g, b, thresholds)
    pixel_classes[i, j] <- color
    color_counts[[color]] <- color_counts[[color]] + 1
  }
}

# Calculate color percentages
total_pixels <- img_dim[1] * img_dim[2]
color_percentages <- lapply(color_counts, function(count) 100 * count / total_pixels)

cat("Pixel color distribution:\n")
for (color in names(color_counts)) {
  cat(sprintf("  %s: %d pixels (%.2f%%)\n", 
              color, color_counts[[color]], color_percentages[[color]]))
}

# ---- Color Distribution QC ----
# Simple check for color balance
color_balance_check <- function(percentages) {
  # Only consider actual colors (not "Other")
  color_vals <- unlist(percentages[c("Red", "Green", "Blue", "Yellow")])
  
  # If no colored pixels are found, return NA
  if (sum(color_vals) < 1) {
    return(list(score=NA, outcome="FAIL", message="Insufficient colored pixels"))
  }
  
  # Calculate coefficient of variation
  cv <- sd(color_vals) / mean(color_vals)
  
  # Lower CV is better (more balanced colors)
  if (cv < 0.5) {
    return(list(score=1-cv, outcome="PASS", message="Good color balance"))
  } else {
    return(list(score=1-cv, outcome="FAIL", message="Poor color balance"))
  }
}

# Overall signal-to-background ratio
signal_to_background <- function(percentages) {
  signal <- sum(unlist(percentages[c("Red", "Green", "Blue", "Yellow")]))
  background <- percentages[["Other"]]
  
  ratio <- signal / (background + 0.001)  # Avoid division by zero
  
  if (ratio > 0.3) {  # Adjusted threshold to be more realistic
    return(list(ratio=ratio, outcome="PASS", message="Good signal-to-background ratio"))
  } else {
    return(list(ratio=ratio, outcome="FAIL", message="Poor signal-to-background ratio"))
  }
}

# ---- Run QC Checks ----
qc_results <- list(
  color_balance = color_balance_check(color_percentages),
  signal_to_background = signal_to_background(color_percentages)
)

# ---- Create Side-by-Side Image Grid ----
create_image_grid <- function(im, pixel_classes, img_dim, prefix) {
  # Output file path
  grid_file <- get_output_path(sprintf("%s_comparison_grid.png", prefix))
  
  # Create a wider png for the grid
  png(grid_file, width=1600, height=800)
  
  # Set up 1×2 grid layout
  par(mfrow=c(1,2), oma=c(0,0,0,0))  # No outer margins needed
  
  # Plot original central square image
  plot(im, main="Original Central Square")
  
  # Plot color classification
  # Map each color to its RGB value
  color_map <- list(
    "Red" = c(1, 0, 0),
    "Green" = c(0, 1, 0),
    "Blue" = c(0, 0, 1),
    "Yellow" = c(1, 1, 0),
    "Other" = c(0.2, 0.2, 0.2)
  )
  
  # Create blank plot with same dimensions
  plot(1:img_dim[1], 1:img_dim[2], type="n", 
       xlab="", ylab="", main="Classified Polonies",
       asp=1)
  
  # Plot points one color at a time
  for (color_name in names(color_map)) {
    indices <- which(pixel_classes == color_name, arr.ind=TRUE)
    if (nrow(indices) > 0) {
      points(indices[,1], indices[,2], pch=15, col=rgb(color_map[[color_name]][1], 
                                                      color_map[[color_name]][2], 
                                                      color_map[[color_name]][3]), 
             cex=0.5)
    }
  }
  
  # No overall title
  
  quiet_dev_off()
  cat("Created side-by-side comparison grid at", grid_file, "\n")
  
  return(grid_file)
}

# Create the grid image
grid_file <- create_image_grid(im, pixel_classes, img_dim, prefix)

# ---- Percentage-Based Histogram ----
create_combined_histogram <- function(color_counts, color_percentages, prefix) {
  color_levels <- c("Red", "Green", "Blue", "Yellow", "Other")
  bar_colors <- c("red", "green", "blue", "yellow", "gray")
  percentage_values <- sapply(color_levels, function(c) color_percentages[[c]])
  names(percentage_values) <- color_levels
  
  # Output file path
  hist_file <- get_output_path(sprintf("%s_pixel_counts.png", prefix))
  
  # Create histogram with percentage values (20% smaller than original)
  png(hist_file, width=640, height=400)
  
  # Standard margins - no need for extra space on right side
  par(mar=c(5, 4, 2, 2) + 0.1)
  
  # Plot the percentage bars directly
  bp <- barplot(percentage_values,
                col=bar_colors,
                main="",
                xlab="",
                ylab="Percent of Image",
                names.arg=color_levels,
                ylim=c(0, max(percentage_values) * 1.1))
  
  # Add percentage values above the bars
  text(bp, percentage_values + max(percentage_values) * 0.03, 
       labels=sprintf("%.1f%%", percentage_values),
       cex=0.8, pos=3)
  
  quiet_dev_off()
  cat("Created percentage histogram at", hist_file, "\n")
  
  return(hist_file)
}

# Create the combined histogram
hist_file <- create_combined_histogram(color_counts, color_percentages, prefix)

# ---- Summarize QC Table ----
# Calculate total colored pixels and percentage
total_colored_pixels <- sum(unlist(color_counts[c("Red", "Green", "Blue", "Yellow")]))
colored_percentage <- 100 * total_colored_pixels / total_pixels

# Create the QC table with additional summary rows
qc_table <- data.frame(
  Metric = c(
    "Total Colored Pixels", "Colored Pixel Percentage",
    "Color Balance Score", "Color Balance", 
    "Signal-to-Background Ratio", "Signal-to-Background",
    "Adaptive Intensity Threshold", "Adaptive Difference Threshold",
    "Red Channel Threshold", "Green Channel Threshold", "Blue Channel Threshold"
  ),
  Value = c(
    format(total_colored_pixels, big.mark=","),
    sprintf("%.2f%%", colored_percentage),
    ifelse(is.na(qc_results$color_balance$score), "NA", 
           sprintf("%.3f", qc_results$color_balance$score)),
    qc_results$color_balance$outcome,
    sprintf("%.3f", qc_results$signal_to_background$ratio),
    qc_results$signal_to_background$outcome,
    sprintf("%.3f", thresholds$intensity_threshold),
    sprintf("%.3f", thresholds$diff_threshold),
    sprintf("%.3f", thresholds$r_threshold),
    sprintf("%.3f", thresholds$g_threshold),
    sprintf("%.3f", thresholds$b_threshold)
  )
)

# ---- Markdown Report ----
report_file <- get_output_path(sprintf("%s_report.md", prefix))

# Write each line separately - updating title format
cat("## AVITI Pixel Color Analysis Report with Adaptive Thresholding\n", file=report_file)
cat("\n", file=report_file, append=TRUE)
cat(sprintf("#### %s\n", basename(imgfile)), file=report_file, append=TRUE)
cat("\n", file=report_file, append=TRUE)
cat(paste0("Analysis of central square region (", img_dim[1], "x", img_dim[2], " pixels)\n"), file=report_file, append=TRUE)
cat("\n", file=report_file, append=TRUE)
cat("### QC Metrics\n", file=report_file, append=TRUE)
cat("\n", file=report_file, append=TRUE)

# Generate table and append to file
table_markdown <- knitr::kable(qc_table, format="markdown")
con <- file(report_file, "a")
writeLines(table_markdown, con)
close(con)

# Add pixel counts by color section
cat("\n\n", file=report_file, append=TRUE)
cat("### Pixel Counts by Color\n", file=report_file, append=TRUE)
cat("\n", file=report_file, append=TRUE)
cat(paste0("- **", names(color_counts), "**: ", format(color_counts, big.mark=","), 
         " (", sprintf("%.2f%%", color_percentages), ")", collapse="\n"), 
    file=report_file, append=TRUE)

# Create integrated QC score
integrated_score <- if (qc_results$color_balance$outcome == "PASS" && 
                         qc_results$signal_to_background$outcome == "PASS") {
  "PASS"
} else {
  "FAIL"
}

# Add final assessment BEFORE plots
cat("\n\n", file=report_file, append=TRUE)
cat("### Final QC Assessment\n", file=report_file, append=TRUE)
cat("\n", file=report_file, append=TRUE)
cat(sprintf("**Integrated QC Score:** %s\n", integrated_score), file=report_file, append=TRUE)

# Add plots section after the assessment - with updated image references
cat("\n\n", file=report_file, append=TRUE)
cat("### Plots\n", file=report_file, append=TRUE)
cat("\n", file=report_file, append=TRUE)
cat(sprintf("- ![Polony Detection Comparison](%s_comparison_grid.png)\n", prefix), file=report_file, append=TRUE)
cat(sprintf("- ![Pixel Counts and Percentages](%s_pixel_counts.png)\n", prefix), file=report_file, append=TRUE)

cat(sprintf("Analysis complete. Report and plots saved to directory '%s'.\n", output_dir))