#!/usr/bin/env Rscript

# AVITI QC Data PCA Visualization
# script: plot_aviti_QC_data.R
# Author: SP@NC; nucleomics@vib.be
# Date: 2025-06-20
# Description: Loads the all_image_metrics.csv file from AVITI batch QC, preprocesses the data,
#              removes invariant columns/rows, performs PCA, and plots the first two principal components.
# Usage: ./plot_aviti_QC_data.R path/to/all_image_metrics.csv [output_plot.png]

# ---- Package Management ----
suppressPackageStartupMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman", repos = "https://cloud.r-project.org")
  }
  pacman::p_load(ggplot2, dplyr, tidyr, optparse, readr, gridExtra)
})

# ---- Argument Parsing ----
option_list <- list(
  optparse::make_option(c("-o", "--output"), type="character", default="PCA_plot.png",
                        help="Output PNG file for PCA plot [default=%default]")
)
parser <- optparse::OptionParser(
  usage = "usage: %prog [options] all_image_metrics.csv",
  option_list = option_list,
  description = "Plot PCA of AVITI QC metrics"
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  optparse::print_help(parser)
  quit(status = 1)
}
parsed_args <- optparse::parse_args(parser, args = args, positional_arguments = 1)
csv_file <- parsed_args$args[1]
output_file <- parsed_args$options$output

# ---- Load Data ----
if (!file.exists(csv_file)) stop("File not found: ", csv_file)
qc_data <- readr::read_csv(csv_file, show_col_types = FALSE)

# ---- Preprocess Data for PCA ----
# Remove columns that are not numeric or are identifiers/errors
id_cols <- c("image", "error", "integrated_score", "color_balance_outcome", "signal_bg_outcome")
num_data <- qc_data %>%
  select(where(is.numeric)) %>%
  select(where(~sd(., na.rm=TRUE) > 0)) %>%
  filter(if_all(everything(), ~!is.na(.)))

if (nrow(num_data) < 2 || ncol(num_data) < 2) {
  stop("Not enough variable data for PCA after filtering.")
}

# ---- PCA ----
pca <- prcomp(num_data, scale. = TRUE, center = TRUE)

# ---- Prepare Data for Plotting ----
plot_df <- data.frame(
  image = qc_data$image[as.numeric(rownames(num_data))],
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  PC3 = pca$x[,3],
  integrated_score = qc_data$integrated_score[as.numeric(rownames(num_data))]
)

# ---- User Color Settings ----
PASS_COLOR <- "lightblue"
FAIL_COLOR <- "orange"

# ---- Create Three PCA Plots ----
p1 <- ggplot(plot_df, aes(x = PC1, y = PC2, color = integrated_score, label = image)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(hjust = 1.1, vjust = 1.1, size = 2.5, check_overlap = TRUE) +
  scale_color_manual(values = c("PASS" = PASS_COLOR, "FAIL" = FAIL_COLOR), na.value = "gray") +
  theme_minimal() +
  labs(
    title = sprintf("PCA: PC1 vs PC2 (%.1f%%, %.1f%% var)", 
                    100 * summary(pca)$importance[2,1], 
                    100 * summary(pca)$importance[2,2]),
    x = sprintf("PC1 (%.1f%% var)", 100 * summary(pca)$importance[2,1]),
    y = sprintf("PC2 (%.1f%% var)", 100 * summary(pca)$importance[2,2]),
    color = "QC Outcome"
  )

p2 <- ggplot(plot_df, aes(x = PC1, y = PC3, color = integrated_score, label = image)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(hjust = 1.1, vjust = 1.1, size = 2.5, check_overlap = TRUE) +
  scale_color_manual(values = c("PASS" = PASS_COLOR, "FAIL" = FAIL_COLOR), na.value = "gray") +
  theme_minimal() +
  labs(
    title = sprintf("PCA: PC1 vs PC3 (%.1f%%, %.1f%% var)", 
                    100 * summary(pca)$importance[2,1], 
                    100 * summary(pca)$importance[2,3]),
    x = sprintf("PC1 (%.1f%% var)", 100 * summary(pca)$importance[2,1]),
    y = sprintf("PC3 (%.1f%% var)", 100 * summary(pca)$importance[2,3]),
    color = "QC Outcome"
  )

p3 <- ggplot(plot_df, aes(x = PC2, y = PC3, color = integrated_score, label = image)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(hjust = 1.1, vjust = 1.1, size = 2.5, check_overlap = TRUE) +
  scale_color_manual(values = c("PASS" = PASS_COLOR, "FAIL" = FAIL_COLOR), na.value = "gray") +
  theme_minimal() +
  labs(
    title = sprintf("PCA: PC2 vs PC3 (%.1f%%, %.1f%% var)", 
                    100 * summary(pca)$importance[2,2], 
                    100 * summary(pca)$importance[2,3]),
    x = sprintf("PC2 (%.1f%% var)", 100 * summary(pca)$importance[2,2]),
    y = sprintf("PC3 (%.1f%% var)", 100 * summary(pca)$importance[2,3]),
    color = "QC Outcome"
  )

# ---- Arrange Plots in 2x2 Grid ----
# Save all three plots in a 2x2 grid (bottom-right empty)
suppressMessages({
  png(output_file, width = 1600, height = 1200)
  grid.arrange(
    p1, p2, p3, 
    ncol = 2, nrow = 2,
    layout_matrix = rbind(c(1,2), c(3,NA))
  )
  invisible(dev.off())
})
cat("PCA plots saved to", output_file, "\n")