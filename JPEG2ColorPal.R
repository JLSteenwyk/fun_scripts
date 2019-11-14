#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## Test if there is one arguments. Quit if there isn't. Save argument is there is only one
if (length(args) != 2) {
  cat("\n")
  cat("-----------------\n")
  cat("| JPEG2ColorPal |\n")
  cat("-----------------\n")
  cat("author: Jacob L. Steenwyk\n")
  cat("citation: NA\n\n\n")
  cat("This script takes a JPEG as input. Using a second require argument, this script will.\n")
  cat("reduce the image into n number of colors. Thus, any JPEG image can be turned into a\n")
  cat("color palette.")
  cat("Usage:\n")
  cat("Rscript JPEG2ColorPal.R file.jpeg n\n\n")
  cat("• file.jpeg is a jpeg file\n")
  cat("• n is the number of colors in the resulting color palette.\n")
  cat("\n")
  quit()
} else if (length(args) == 2) {
  jpegFile  = args[1]
  numColors = args[2]
}

## test if packages are downloaded. If not, install
if (!require("jpeg")) {
install.packages("jpeg", dependencies = TRUE)
library(jpeg)
}
if (!require("ggplot2")) {
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
}

## read in JPEG
jpegArr <- readJPEG(jpegFile)

## extract rgb values from JPEG array
dimensions <- dim(jpegArr)
jpeg_rgb <- data.frame(
  x = rep(1:dimensions[2], each = dimensions[1]),
  y = rep(dimensions[1]:1, dimensions[2]),
  R = as.vector(jpegArr[,,1]), #slicing our array into three
  G = as.vector(jpegArr[,,2]),
  B = as.vector(jpegArr[,,3])
)

## Cluster RGB values
jpegClustered <- kmeans(jpeg_rgb[,c("R","G","B")], centers = numColors, iter.max = 50)
Centers<-as.data.frame(jpegClustered$centers)

## create color palette
colorPal<-qplot(x=1:nrow(jpegClustered$centers), y = 1, fill=factor(1:nrow(jpegClustered$centers)), geom="tile") +
  scale_fill_manual(values = rgb(Centers[order(Centers$R),])) +
  theme_void()+
  theme(legend.position="none")

## export colorpalette
pdf("ColorPal.pdf")
colorPal
dev.off()




