#!/usr/bin/env python3
"""
Script Name:    measure_thumbnails.py
Description:    Analyze images in a folder for quasi-black pixel content,
                sort images into low/medium/high folders based on percentile
                cutoffs (most black → low, least black → high), generate
                histogram and line plot with percentile markers, and
                save a TSV file with sorted counts and filenames (with header).
Author:         SP@NC (+AI)
Required: conda env python_img with plotting packages
Date:           2025-06-18
"""

import os
import shutil
from PIL import Image
import numpy as np
import matplotlib.pyplot as plt

# === Editable variables ===
directory = 'aviti_thumbnails'
quasi_black_threshold = 50    # Set to your preferred quasi-black pixel value threshold
percentile_cutoff = 20        # Set to your preferred percentile cutoff (e.g., 20 for 20th/80th)

# Prepare subfolders
low_dir = os.path.join(directory, 'low')
medium_dir = os.path.join(directory, 'medium')
high_dir = os.path.join(directory, 'high')
os.makedirs(low_dir, exist_ok=True)
os.makedirs(medium_dir, exist_ok=True)
os.makedirs(high_dir, exist_ok=True)

# Analyze images and collect counts
file_counts = []
# , '.jpg', '.jpeg', '.bmp', '.tiff'
for filename in os.listdir(directory):
    if filename.lower().endswith(('.png')):
        path = os.path.join(directory, filename)
        img = Image.open(path).convert('L')
        arr = np.array(img)
        quasi_black_count = np.sum(arr <= quasi_black_threshold)
        file_counts.append((filename, quasi_black_count))

# Sort results for plotting and processing
file_counts.sort(key=lambda x: x[1])
counts = [count for _, count in file_counts]

# Calculate percentiles based on cutoff
p_low = np.percentile(counts, percentile_cutoff)
p_high = np.percentile(counts, 100 - percentile_cutoff)

# --- Plot and save histogram with vertical lines at percentiles ---
fig1, ax1 = plt.subplots(figsize=(10, 6))
n, bins, patches = ax1.hist(counts, bins=30, color='skyblue', edgecolor='black')
ax1.axvline(p_low, color='red', linestyle='--', label=f'{percentile_cutoff}th percentile ({int(p_low)})')
ax1.axvline(p_high, color='green', linestyle='--', label=f'{100 - percentile_cutoff}th percentile ({int(p_high)})')
ax1.set_title('Distribution of Quasi-Black Pixel Counts')
ax1.set_xlabel('Number of Quasi-Black Pixels')
ax1.set_ylabel('Number of Images')
ax1.legend()
fig1.tight_layout()
fig1.savefig('quasi_black_pixel_histogram.png', dpi=300, bbox_inches='tight')
plt.close(fig1)

# --- Plot and save line plot with vertical lines at percentiles ---
sorted_counts = sorted(counts)
fig2, ax2 = plt.subplots(figsize=(10, 6))
ax2.plot(sorted_counts, marker='o')
idx_p_low = np.searchsorted(sorted_counts, p_low)
idx_p_high = np.searchsorted(sorted_counts, p_high)
ax2.axvline(x=idx_p_low, color='red', linestyle='--', label=f'{percentile_cutoff}th percentile ({int(p_low)})')
ax2.axvline(x=idx_p_high, color='green', linestyle='--', label=f'{100 - percentile_cutoff}th percentile ({int(p_high)})')
ax2.set_title('Quasi-Black Pixel Counts per Image (Sorted)')
ax2.set_xlabel('Image Index (sorted)')
ax2.set_ylabel('Quasi-Black Pixel Count')
ax2.legend()
fig2.tight_layout()
fig2.savefig('quasi_black_pixel_lineplot.png', dpi=300, bbox_inches='tight')
plt.close(fig2)

# --- Move images to low/medium/high folders based on inverted percentiles logic ---
low_count = 0
medium_count = 0
high_count = 0

for filename, count in file_counts:
    src_path = os.path.join(directory, filename)
    if count >= p_high:
        dst_path = os.path.join(low_dir, filename)
        low_count += 1
    elif count <= p_low:
        dst_path = os.path.join(high_dir, filename)
        high_count += 1
    else:
        dst_path = os.path.join(medium_dir, filename)
        medium_count += 1
    shutil.move(src_path, dst_path)
    print(f"Moved {filename} to {'low' if count >= p_high else 'high' if count <= p_low else 'medium'} ({count} quasi-black pixels)")

print(f"\nNumber of images in low (≥ {100 - percentile_cutoff}th percentile, most black): {low_count}")
print(f"Number of images in medium (between {percentile_cutoff}th and {100 - percentile_cutoff}th percentile): {medium_count}")
print(f"Number of images in high (≤ {percentile_cutoff}th percentile, least black): {high_count}")

# --- Save sorted counts and filenames to a TSV file with header ---
tsv_filename = 'quasi_black_pixel_counts_sorted.tsv'
with open(tsv_filename, 'w') as tsvfile:
    tsvfile.write("count\tfilename\n")
    for filename, count in file_counts:
        tsvfile.write(f"{count}\t{filename}\n")

