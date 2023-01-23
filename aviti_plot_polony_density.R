#!/usr/bin/env Rscript

# script: aviti_plot_polony_density.R
# Stéphane Plaisance - VIB-Nucleomics Core - 2023-01-23 v1.0

library("ComplexHeatmap")
library("ggplot2")

# produce the two input files with:
# aviti_count_tiles.sh 1 > tiles_lane1.txt
# aviti_count_tiles.sh 2 > tiles_lane2.txt

# setwd("/data/analyses/aviti_bases2fastq/20230117_AV224503_4353-4354_20230117_AVITI_A_fastq_sp_notrim")

lane1 <- read.table("tiles_lane1.txt", quote="\"", comment.char="")
colnames(lane1) <- c("count", "tile")

l1.data <- matrix(lane1$count, 53, byrow=TRUE)

hm1 <- ComplexHeatmap::Heatmap(l1.data, 
               name="lane1",
               cluster_rows = FALSE,
               cluster_columns = FALSE)

lane2 <- read.table("tiles_lane2.txt", quote="\"", comment.char="")
colnames(lane2) <- c("count", "tile")

l2.data <- matrix(lane2$count, 53, byrow=TRUE)

hm2 <- ComplexHeatmap::Heatmap(l2.data, 
                               name="lane2",
                               cluster_rows = FALSE,
                               cluster_columns = FALSE)

ht_list = hm1 + hm2

pdf(file="aviti_loading_density.pdf",width = 5, height = 10, bg = "white")

draw(ht_list, 
     row_title = "Aviti polonies", 
     row_title_gp = gpar(col = "red"),
     column_title = "Lanes", 
     column_title_side = "bottom")

dev.off()
