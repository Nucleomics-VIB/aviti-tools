#!/usr/bin/env Rscript

# AVITI Batch Polony Analysis with Adaptive Thresholding
# script: aviti_batch_polony_adaptive_classify.R
# Author: SP@NC; nucleomics@vib.be
# Date: 2025-06-19
# Description: Batch processing of AVITI PNG images with adaptive thresholding
# Usage: ./aviti_batch_polony_adaptive_classify.R input_folder [options]

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}

pacman::p_load(
  imager,
  magrittr,
  dplyr,
  knitr,
  ggplot2,
  parallel,
  optparse,
  tidyr,
  stats,
  gridExtra,
  grid
)

# ---- Configuration Parameters ----
default_params <- list(
  square_size = 250,
  intensity_min = 0.2,
  intensity_max = 0.5,
  diff_min = 10,
  diff_max = 40,
  color_cv_threshold = 0.5,
  signal_bg_ratio_threshold = 0.3,
  cores = 1   # <-- use 'cores' here to match the option name
)

# ---- Argument Parsing ----
option_list <- list(
  optparse::make_option(c("-s", "--square_size"), type="integer", default=default_params$square_size,
                help="Size of central square to analyze [default=%default]"),
  optparse::make_option(c("-i", "--intensity_min"), type="double", default=default_params$intensity_min,
                help="Minimum intensity threshold [default=%default]"),
  optparse::make_option(c("-I", "--intensity_max"), type="double", default=default_params$intensity_max,
                help="Maximum intensity threshold [default=%default]"),
  optparse::make_option(c("-d", "--diff_min"), type="double", default=default_params$diff_min,
                help="Minimum color difference threshold [default=%default]"),
  optparse::make_option(c("-D", "--diff_max"), type="double", default=default_params$diff_max,
                help="Maximum color difference threshold [default=%default]"),
  optparse::make_option(c("-c", "--cores"), type="integer", default=default_params$cores,
                help="Number of cores for parallel processing [default=%default]")
)

parser <- optparse::OptionParser(
  usage = "usage: %prog [options] input_folder",
  option_list = option_list,
  description = "Process AVITI images with adaptive thresholding"
)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  optparse::print_help(parser)
  quit(status = 1)
}

tryCatch({
  parsed_args <- optparse::parse_args(parser, args = args, positional_arguments = 1)
}, error = function(e) {
  cat("Error parsing arguments:", e$message, "\n")
  optparse::print_help(parser)
  quit(status = 1)
})

# Extract options and input folder
opts <- parsed_args$options
img_folder <- parsed_args$args[1]

# Update parameters with command line options
params <- modifyList(default_params, as.list(opts))

# Print number of cores for confirmation
cat(sprintf("Using %d cores for parallel processing\n", params$cores))

# Check if folder exists
if (!dir.exists(img_folder)) {
  stop("Input folder does not exist: ", img_folder)
}

# Find PNG files
img_files <- list.files(img_folder, pattern="\\.png$", full.names=TRUE)
if (length(img_files) == 0) {
  stop("No PNG files found in ", img_folder)
}

# ---- Helper Functions ----
# Helper function to close plot devices without messages
quiet_dev_off <- function() {
  invisible(capture.output(dev.off()))
}

# ---- Adaptive Thresholding Functions ----
calculate_adaptive_thresholds <- function(r_channel, g_channel, b_channel, params) {
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
  # but scale it to ensure it's sensitive enough (within configurable bounds)
  intensity_threshold <- max(params$intensity_min, min(params$intensity_max, q3_intensity * 0.8))
  
  # Determine the difference threshold based on the median of differences
  # but keep it within reasonable bounds
  diff_threshold <- max(params$diff_min, min(params$diff_max, median(c(r_diff, g_diff, b_diff)) * 0.9))
  
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

# ---- QC Functions ----
# Simple check for color balance
color_balance_check <- function(percentages, threshold) {
  # Only consider actual colors (not "Other")
  color_vals <- unlist(percentages[c("Red", "Green", "Blue", "Yellow")])
  
  # If no colored pixels are found, return NA
  if (sum(color_vals) < 1) {
    return(list(score=NA, outcome="FAIL", message="Insufficient colored pixels"))
  }
  
  # Calculate coefficient of variation
  cv <- sd(color_vals) / mean(color_vals)
  
  # Lower CV is better (more balanced colors)
  if (cv < threshold) {
    return(list(score=1-cv, outcome="PASS", message="Good color balance"))
  } else {
    return(list(score=1-cv, outcome="FAIL", message="Poor color balance"))
  }
}

# Overall signal-to-background ratio
signal_to_background <- function(percentages, threshold) {
  signal <- sum(unlist(percentages[c("Red", "Green", "Blue", "Yellow")]))
  background <- percentages[["Other"]]
  
  ratio <- signal / (background + 0.001)  # Avoid division by zero
  
  if (ratio > threshold) {
    return(list(ratio=ratio, outcome="PASS", message="Good signal-to-background ratio"))
  } else {
    return(list(ratio=ratio, outcome="FAIL", message="Poor signal-to-background ratio"))
  }
}

# ---- Visualization Functions ----
create_image_grid <- function(im, pixel_classes, img_dim, output_file) {
  # Create a wider png for the grid
  png(output_file, width=1600, height=800)
  
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
  
  quiet_dev_off()
  return(output_file)
}

create_color_histogram <- function(color_percentages, output_file) {
  color_levels <- c("Red", "Green", "Blue", "Yellow", "Other")
  bar_colors <- c("red", "green", "blue", "yellow", "gray")
  percentage_values <- sapply(color_levels, function(c) color_percentages[[c]])
  names(percentage_values) <- color_levels
  
  # Create histogram with percentage values
  png(output_file, width=640, height=400)
  
  # Standard margins
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
  return(output_file)
}

# ---- Per-Image Processing Function ----
process_aviti_image <- function(imgfile, outdir, params) {
  tryCatch({
    # Setup output paths
    prefix <- file.path(outdir, sub("\\.png$", "", basename(imgfile)))
    
    # Load the full image
    full_im <- load.image(imgfile)
    
    # Get image dimensions and calculate central square
    img_width <- dim(full_im)[1]
    img_height <- dim(full_im)[2]
    square_size <- min(params$square_size, img_width, img_height)
    
    # Calculate coordinates for the central square
    x_center <- floor(img_width / 2)
    y_center <- floor(img_height / 2)
    
    # Calculate offsets for any square_size (odd or even)
    left_offset <- floor(square_size / 2)
    right_offset <- square_size - 1 - floor(square_size / 2)
    
    # Calculate actual extraction coordinates
    x_start <- max(1, x_center - left_offset)
    x_end <- min(img_width, x_center + right_offset)
    y_start <- max(1, y_center - left_offset)
    y_end <- min(img_height, y_center + right_offset)
    
    # Extract the central region
    im <- imsub(full_im, x %inr% c(x_start, x_end), y %inr% c(y_start, y_end))
    
    # Get dimensions of extracted region
    img_array <- as.array(im)
    img_dim <- dim(img_array)
    
    # Extract channel data
    r_channel <- img_array[,,1,1]
    g_channel <- img_array[,,1,2]
    b_channel <- img_array[,,1,3]
    
    # Calculate adaptive thresholds
    thresholds <- calculate_adaptive_thresholds(r_channel, g_channel, b_channel, params)
    
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
    
    # Run QC checks
    qc_results <- list(
      color_balance = color_balance_check(color_percentages, params$color_cv_threshold),
      signal_to_background = signal_to_background(color_percentages, params$signal_bg_ratio_threshold)
    )
    
    # Create integrated QC score
    integrated_score <- if (qc_results$color_balance$outcome == "PASS" && 
                            qc_results$signal_to_background$outcome == "PASS") {
      "PASS"
    } else {
      "FAIL"
    }
    
    # Generate visualizations
    overlay_file <- sprintf("%s_overlay.png", prefix)
    create_image_grid(im, pixel_classes, img_dim, overlay_file)
    
    hist_file <- sprintf("%s_color_hist.png", prefix)
    create_color_histogram(color_percentages, hist_file)
    
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
    
    # Create Markdown report
    report_file <- sprintf("%s_report.md", prefix)
    
    cat("## AVITI Pixel Color Analysis Report with Adaptive Thresholding\n", file=report_file)
    cat("\n", file=report_file, append=TRUE)
    cat(sprintf("#### %s\n", basename(imgfile)), file=report_file, append=TRUE)
    cat("\n", file=report_file, append=TRUE)
    cat(paste0("Analysis of central square region (", img_dim[1], "x", img_dim[2], " pixels)\n"), 
        file=report_file, append=TRUE)
    cat("\n", file=report_file, append=TRUE)
    cat("### QC Metrics\n", file=report_file, append=TRUE)
    cat("\n", file=report_file, append=TRUE)
    
    # Table
    table_md <- knitr::kable(qc_table, format = "markdown")
    cat(table_md, sep = "\n", file = report_file, append = TRUE)
    
    # Add pixel counts by color section
    cat("\n\n", file=report_file, append=TRUE)
    cat("### Pixel Counts by Color\n", file=report_file, append=TRUE)
    cat("\n", file=report_file, append=TRUE)
    cat(paste0("- **", names(color_counts), "**: ", format(color_counts, big.mark=","), 
             " (", sprintf("%.2f%%", color_percentages), ")", collapse="\n"), 
        file=report_file, append=TRUE)
    
    # Add final assessment
    cat("\n\n", file=report_file, append=TRUE)
    cat("### Final QC Assessment\n", file=report_file, append=TRUE)
    cat("\n", file=report_file, append=TRUE)
    cat(sprintf("**Integrated QC Score:** %s\n", integrated_score), file=report_file, append=TRUE)
    
    # Add plots section
    cat("\n\n", file=report_file, append=TRUE)
    cat("### Plots\n", file=report_file, append=TRUE)
    cat("\n", file=report_file, append=TRUE)
    cat(sprintf("- ![Polony Detection Comparison](%s_overlay.png)\n", basename(prefix)), 
        file=report_file, append=TRUE)
    cat(sprintf("- ![Pixel Counts and Percentages](%s_color_hist.png)\n", basename(prefix)), 
        file=report_file, append=TRUE)
    
    # Return metrics for summary
    return(list(
      image = basename(imgfile),
      total_pixels = total_pixels,
      colored_pixels = total_colored_pixels,
      colored_percentage = colored_percentage,
      color_balance_score = qc_results$color_balance$score,
      color_balance_outcome = qc_results$color_balance$outcome,
      signal_bg_ratio = qc_results$signal_to_background$ratio,
      signal_bg_outcome = qc_results$signal_to_background$outcome,
      integrated_score = integrated_score,
      red_percent = color_percentages$Red,
      green_percent = color_percentages$Green,
      blue_percent = color_percentages$Blue,
      yellow_percent = color_percentages$Yellow,
      other_percent = color_percentages$Other,
      intensity_threshold = thresholds$intensity_threshold,
      diff_threshold = thresholds$diff_threshold
    ))
  }, error = function(e) {
    return(list(
      image = basename(imgfile),
      error = paste0("Error: ", e$message)
    ))
  })
}

# ---- Create Output Directory ----
outdir <- file.path(img_folder, "QC_Results")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ---- Batch Processing with Parallel Support ----
cat("Processing", length(img_files), "images...\n")

if (params$cores > 1) {
  cl <- parallel::makeCluster(min(params$cores, parallel::detectCores()))
  parallel::clusterExport(cl, varlist = c(
    "params", "outdir", 
    "calculate_adaptive_thresholds", "classify_pixel_color_adaptive", 
    "color_balance_check", "signal_to_background",
    "create_image_grid", "create_color_histogram", 
    "process_aviti_image", "quiet_dev_off"
  ), envir = environment())
  parallel::clusterEvalQ(cl, {
    library(imager)
    library(magrittr)
    library(dplyr)
    library(knitr)
    library(ggplot2)
    library(stats)
    TRUE
  })
  all_results <- parallel::parLapplyLB(cl, img_files, function(imgfile) {
    process_aviti_image(imgfile, outdir, params)
  })
  parallel::stopCluster(cl)
  cat("Parallel processing complete.\n")
} else {
  all_results <- lapply(img_files, function(imgfile) {
    cat("Processing:", basename(imgfile), "\n")
    process_aviti_image(imgfile, outdir, params)
  })
}

# ---- Process Results ----
# Convert list of results to data frame
all_results_df <- do.call(rbind, lapply(all_results, function(x) {
  # Ensure all expected columns are present
  if (is.null(x$error)) x$error <- NA
  as.data.frame(x, stringsAsFactors = FALSE)
}))

# Ensure 'error' column exists
if (!"error" %in% colnames(all_results_df)) {
  all_results_df$error <- NA
}

# Check for errors
error_images <- all_results_df %>% filter(!is.na(error))
if (nrow(error_images) > 0) {
  cat("The following images had errors during processing:\n")
  print(error_images[, c("image", "error")])
}

# Remove error entries for analysis
valid_results_df <- all_results_df %>% filter(is.na(error))

# Save full results
write.csv(valid_results_df, file=file.path(outdir, "all_image_metrics.csv"), row.names=FALSE)

# ---- Create Summary Report ----
summary_report <- file.path(outdir, "batch_summary_report.md")

cat(
  "# AVITI Batch Analysis Summary Report with Adaptive Thresholding\n\n",
  sprintf("**Date:** %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("**Input Folder:** %s\n\n", img_folder),
  sprintf("**Number of Images Processed:** %d\n\n", length(img_files)),
  sprintf("**Number of Images with Complete Data:** %d\n\n", nrow(valid_results_df)),
  file=summary_report
)

if (nrow(valid_results_df) > 0) {
  # Overall summary stats
  cat(
    "## Overall QC Statistics\n\n",
    file=summary_report, append=TRUE
  )
  
  # Summary table of key metrics
  summary_stats <- valid_results_df %>%
    summarize(
      Avg_Colored_Percentage = mean(colored_percentage, na.rm=TRUE),
      Avg_Color_Balance_Score = mean(color_balance_score, na.rm=TRUE),
      Avg_Signal_BG_Ratio = mean(signal_bg_ratio, na.rm=TRUE),
      Pct_Passed_Color_Balance = 100 * sum(color_balance_outcome == "PASS", na.rm=TRUE) / n(),
      Pct_Passed_Signal_BG = 100 * sum(signal_bg_outcome == "PASS", na.rm=TRUE) / n(),
      Pct_Passed_Overall = 100 * sum(integrated_score == "PASS", na.rm=TRUE) / n()
    )
  
  table_md <- knitr::kable(summary_stats, format = "markdown", digits = 2)
  cat(table_md, sep = "\n", file = summary_report, append = TRUE)
  
  # Average color distribution
  cat(
    "\n\n## Average Color Distribution\n\n",
    file=summary_report, append=TRUE
  )
  
  color_stats <- valid_results_df %>%
    summarize(
      Red = mean(red_percent, na.rm=TRUE),
      Green = mean(green_percent, na.rm=TRUE),
      Blue = mean(blue_percent, na.rm=TRUE),
      Yellow = mean(yellow_percent, na.rm=TRUE),
      Other = mean(other_percent, na.rm=TRUE)
    )
  
  table_md <- knitr::kable(color_stats, format = "markdown", digits = 2)
  cat(table_md, sep = "\n", file = summary_report, append = TRUE)
  
  # Create a summary plot of color distributions
  if (nrow(valid_results_df) > 1) {
    # Generate a summary plot showing color distributions across all images
    color_summary_plot <- file.path(outdir, "color_distribution_summary.png")
    
    # Prepare data for plotting
    plot_data <- valid_results_df %>%
      select(image, red_percent, green_percent, blue_percent, yellow_percent, other_percent) %>%
      tidyr::pivot_longer(cols = c(other_percent, red_percent, green_percent, blue_percent, yellow_percent), # 'Other' first
                        names_to = "color", values_to = "percent")
    
    # Clean up color names
    plot_data$color <- gsub("_percent", "", plot_data$color)
    plot_data$color <- tools::toTitleCase(plot_data$color)
    
    # Set color factor levels to control stacking order ('Other' at bottom)
    plot_data$color <- factor(plot_data$color, levels = c("Red", "Green", "Blue", "Yellow", "Other"))
    
    # Create the color palette
    color_palette <- c("Red" = "red", "Green" = "green", "Blue" = "blue", 
                      "Yellow" = "yellow", "Other" = "gray")
    
    # Determine x-axis text size and visibility
    n_images <- length(unique(plot_data$image))
    if (n_images > 20) {
      axis_text_x <- element_blank()
    } else if (n_images > 10) {
      axis_text_x <- element_text(angle = 45, hjust = 1, size = 8)
    } else {
      axis_text_x <- element_text(angle = 45, hjust = 1, size = 10)
    }
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = image, y = percent, fill = color)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = color_palette) +
      theme_minimal() +
      theme(axis.text.x = axis_text_x) +
      labs(title = "Color Distribution Across Images",
           x = "Image", y = "Percentage", fill = "Color")
    
    # Save the plot
    ggsave(color_summary_plot, p, width = 10, height = 6)
    
    # Add reference to the plot in the report
    cat(
      "\n\n## Color Distribution Summary\n\n",
      "![Color Distribution](color_distribution_summary.png)\n\n",
      file=summary_report, append=TRUE
    )
  }
  
  # Add table of all image outcomes
  cat(
    "\n\n## Image QC Outcomes\n\n",
    file = summary_report, append = TRUE
  )
  image_summary <- valid_results_df %>%
    select(image, colored_percent, color_balance_outcome, signal_bg_outcome, overall_outcome)
  colnames(image_summary) <- c("Image", "Colored %", "Color Balance", "Signal-BG Ratio", "Overall")
  table_md <- knitr::kable(image_summary, format = "markdown", digits = 2)
  cat(table_md, sep = "\n", file = summary_report, append = TRUE)

  # Add table of failed images (FAILED in one of the last two columns)
  failed_images <- image_summary %>%
    filter(`Signal-BG Ratio` == "FAIL" | Overall == "FAIL")
  if (nrow(failed_images) > 0) {
    cat(
      "\n\n## Images with QC Failures\n\n",
      file = summary_report, append = TRUE
    )
    failed_table_md <- knitr::kable(failed_images, format = "markdown", digits = 2)
    cat(failed_table_md, sep = "\n", file = summary_report, append = TRUE)
  }
}
#