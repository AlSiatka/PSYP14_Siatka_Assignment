## GP ASSIGNMENT ##
# clean workspace #
graphics.off()
rm(list=ls(all=TRUE))

# packages #
library(psych)
library(tidyverse)
library(MASS)
library(MVA)
library(smacof)

## GP Assignment Part 1 ##
# importing file #
my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)
  #import edited excel file #
# reviewing dataset and removing the NA data #
sum(is.na(my_data))
clean_data <- na.omit(my_data)

# PCA #
data_PCA1 <- prcomp(clean_data, center = TRUE,scale. = TRUE)

summary(data_PCA1) #

# plotting PCA #
windows()
biplot(data_PCA1)
#
library(factoextra)
windows()
fviz_eig(data_PCA1)
#
windows()
fviz_pca_ind(data_PCA1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#
windows()
fviz_pca_var(data_PCA1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#
windows()
fviz_pca_biplot(data_PCA1, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

## GP Assignment Part 2 ##
# reverse code the data so larger number = more dissimiliar #
my_nation_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

# multidimensional scaling #
MDS_nation<- isoMDS(my_nation_data)

dis2 <- dist(my_nation_data)
dim2nation <- isoMDS(d, k=2) 
dim2nation 

# plot solution
windows()
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = colnames(my_nation_data), cex=.7) 

# 3 dimensions
dis3 <- dist(my_nation_data)
dim3nation <- isoMDS(d, k=3) 
dim3nation 

# plot solution
windows()
x <- dim3nation$points[,1]
y <- dim3nation$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = colnames(my_nation_data), cex=.7) 

# 