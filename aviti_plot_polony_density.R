#!/usr/bin/env Rscript

# script: aviti_plot_polony_density.R
# Stéphane Plaisance - VIB-Nucleomics Core - 2023-01-23 v1.01

suppressPackageStartupMessages(library("ComplexHeatmap"))
suppressPackageStartupMessages(library("grid"))

# produce the two input files with:
# aviti_count_tiles.sh 1 > tiles_lane1.txt
# aviti_count_tiles.sh 2 > tiles_lane2.txt

# setwd("/data/analyses/aviti_bases2fastq/20230117_AV224503_4353-4354_20230117_AVITI_A_fastq_sp_notrim")

# load Lane1 data
lane1 <- read.table("tiles_lane1.txt", quote="\"", comment.char="")
colnames(lane1) <- c("count", "tile")

# add 3 NA where no data present
l1.data <- matrix(c(NA, lane1$count[c(1:3)], NA, lane1$count[-c(1:3)], NA), 43, byrow=TRUE)

hm1 <- ComplexHeatmap::Heatmap(l1.data, 
                               name="lane1",
                               cluster_rows = FALSE,
                               cluster_columns = FALSE)

# load Lane1 data
lane2 <- read.table("tiles_lane2.txt", quote="\"", comment.char="")
colnames(lane2) <- c("count", "tile")

# add 3 NA where no data present
l2.data <- matrix(c(NA, lane2$count[c(1:3)], NA, lane2$count[-c(1:3)], NA), 43, byrow=TRUE)

hm2 <- ComplexHeatmap::Heatmap(l2.data, 
                               name="lane2",
                               cluster_rows = FALSE,
                               cluster_columns = FALSE)

# assemble heatmaps and produce plot
ht_list = hm1 + hm2

pdf(file="aviti_loading_density.pdf",width = 5, height = 10, bg = "white")

ComplexHeatmap::draw(ht_list, 
     row_title = "Aviti polonies", 
     row_title_gp = gpar(col = "red"),
     column_title = "Lanes", 
     column_title_side = "bottom")

null <- dev.off()
