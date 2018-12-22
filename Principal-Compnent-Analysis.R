setwd("D:/EDA/MAIL/case studies")
pca  = read.csv("PCA_MARKS.csv")
pca
summary(pca)
var(pca)
#library(ggplot2)
#library(gmodels)
#plot(pca)
#boxplot(pca)
mean(pca$Hours)
mean(pca$Marks)
sd(pca$Hours)
sd(pca$Marks)
var(pca$Hours)
var(pca$Marks)
#calculate hmod
pca$Hours_mod = (pca$Hours - mean(pca$Hours))/sd(pca$Hours)
pca$Hours_mod
#calculate mmod
pca$Marks_mod = (pca$Marks - mean(pca$Marks))/sd(pca$Marks)
pca$Marks_mod
pca
#storing h mod and m mod in pca 1
pca1=pca[3:4]
pca1
#cvm is co-variance matrix
cvm = var(pca1)
#finding Eigen vectors and eigen values
eigen(cvm)
