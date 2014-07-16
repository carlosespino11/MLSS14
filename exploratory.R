library(R.matlab)
library(caret)
library(data.table)
library(ggplot2)
library(lda)
library(knn)

setwd("~/Dropbox/handout")

x = readMat("data/xTrain.mat")
y = readMat("data/yTrain.mat")
h = readMat("data/h.mat")
# write.csv(as.matrix(h$h), "h.csv", row.names = FALSE, col.names = FALSE)
n = dim(x$XTrain)[1]
n_train = floor(dim(x$XTrain)[1]*.6)
set.seed(7)
r_index = sample(seq(1, n), n)

xtrain = x$XTrain[r_index[1:n_train],]
ytrain = y$yTrain[r_index[1:n_train],]

xtest = x$XTrain[r_index[(n_train+1):n],]
ytest = y$yTrain[r_index[(n_train+1):n],]

row_counts = apply(ytrain, 1, sum)
row_counts = data.frame(count = row_counts, category = 1:length(row_counts))

column_counts = apply(ytrain, 2, sum)
# column_counts = data.frame(count = column_counts, category = as.factor(1:length(column_counts)))

ggplot(data = column_counts)  + geom_histogram(aes(y= count, x = category), stat="identity", fill="red", alpha =0.5)
ggplot(data = data.frame(count = row_counts) ) + geom_histogram(aes(x= count), fill="blue", alpha =0.7) + scale_x_discrete(limits=c(1:7))+
  labs(x="Number of classes") 


