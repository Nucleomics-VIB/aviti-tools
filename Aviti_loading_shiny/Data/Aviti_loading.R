if (!requireNamespace("png", quietly = TRUE)) install.packages("png")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
library(png)
library(ggplot2)
library(zoo)

# --- PARAMETERS ---
img_files <- c(
  "~/Downloads/4768_1_OK.png",
  "~/Downloads/4770_1_underloaded.png",
  "~/Downloads/4778_1_overloaded.png",
  "~/Downloads/20250107_AV224503_4822_4911_1/ThumbnailLane2.png",
  "~/Downloads/20250107_AV224503_4822_4911_1/ThumbnailLane1.png",
  "~/Downloads/20250107_AV224503_4917_1/ThumbnailLane1.png",
  "~/Downloads/20250107_AV224503_4917_1/ThumbnailLane2.png"
)
labels <- c("4768_OK", "4770_under", "4778_over", "4822_4911_1_l2", "4822_4911_1_l1", "4917_1_l1", "4917_1_l2")

# Vector to select which datasets to plot (1=plot, 0=skip)
to_plot <- c(1, 1, 1, 1, 1, 1, 1)  # Example: plot 4768, 4770, 4778, 4917_1_l1

cutoff1 <- 10
cutoff2 <- 50
smooth_steps <- 0  # Number of values before and after x to include in the average

# --- SUBSET FILES AND LABELS BASED ON to_plot ---
img_files_sel <- img_files[as.logical(to_plot)]
labels_sel <- labels[as.logical(to_plot)]

# --- FUNCTION TO COMPUTE HISTOGRAM, PERCENTS, AND N50 ---
get_rgb_hist <- function(img_path, label, cutoff1 = 10, cutoff2 = 50, smooth_steps = 4) {
  img <- readPNG(img_path)
  if (dim(img)[3] == 4) img <- img[,,1:3]
  if (max(img) <= 1) img <- img * 255
  all_rgb_vals <- c(as.integer(img[,,1]), as.integer(img[,,2]), as.integer(img[,,3]))
  df <- as.data.frame(table(factor(all_rgb_vals, levels = 0:255)))
  colnames(df) <- c("RGB_value", "Pixel_count")
  df$RGB_value <- as.integer(as.character(df$RGB_value))
  df$Pixel_count <- as.integer(df$Pixel_count)
  df$Image <- label
  # Percentages and N50 from original data
  total <- sum(df$Pixel_count)
  pct_low <- sum(df$Pixel_count[df$RGB_value < cutoff1]) / total * 100
  pct_high <- sum(df$Pixel_count[df$RGB_value > cutoff2]) / total * 100
  cumulative <- cumsum(df$Pixel_count)
  idx <- which(cumulative >= total/2)[1]
  n50_value <- df$RGB_value[idx]
  # Smoothing: moving average with window size (2*smooth_steps+1)
  window_size <- 2 * smooth_steps + 1
  df$Smoothed <- zoo::rollmean(df$Pixel_count, k = window_size, fill = NA, align = "center")
  # For edges, fill with original values
  df$Smoothed[is.na(df$Smoothed)] <- df$Pixel_count[is.na(df$Smoothed)]
  list(hist = df, pct_low = pct_low, pct_high = pct_high, n50 = n50_value)
}

# --- GATHER DATA ---
hist_list <- mapply(get_rgb_hist, img_files_sel, labels_sel,
                    MoreArgs = list(cutoff1 = cutoff1, cutoff2 = cutoff2, smooth_steps = smooth_steps), SIMPLIFY = FALSE)
hist_all <- do.call(rbind, lapply(hist_list, function(x) x$hist))

# Prepare annotation text for each image, including N50
ann_text <- mapply(function(lbl, pl, ph, n50) {
  sprintf("%s:\n<%d: %.1f%%\n>%d: %.1f%%\nN50: %d", lbl, cutoff1, pl, cutoff2, ph, n50)
}, labels_sel, sapply(hist_list, function(x) x$pct_low),
sapply(hist_list, function(x) x$pct_high),
sapply(hist_list, function(x) x$n50))

# --- PLOT ---
ggplot(hist_all, aes(x = RGB_value, y = Smoothed, color = Image)) +
  geom_point(size = 1.5, alpha = 0.8) +
  labs(title = sprintf("Overlayed Smoothed Histogram (window=%d)", 2*smooth_steps+1),
       x = "RGB Decimal Value (0-255)",
       y = "Smoothed Pixel Count",
       color = "Image") +
  theme_minimal() +
  annotate("text",
           x = Inf, y = Inf,
           label = paste(ann_text, collapse = "\n\n"),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black")
