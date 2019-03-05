library(MASS)
library(FactoMineR)
library(ggplot2)
data= read.csv("https://raw.githubusercontent.com/gastonstat/CreditScoring/master/CleanCreditScoring.csv", header=TRUE, stringsAsFactors=TRUE)
View(data)
dim(data)
str(data)
head(data)
summary(data)
### Clustering Part
datacat = subset(data, select=c(seniorityR, timeR, ageR, expensesR, incomeR,
                                assetsR, debtR, amountR, priceR, finratR, savingsR, Status))

## MCA
mca = MCA(datacat, ncp=40, quali.sup=12, graph=FALSE)
#get the eigenvalues
eig = mca$eig
eigs=eig[,"eigenvalue"]
#calculate significant dimension in MCA
dimmca = sum(eigs>1/length(eigs))
#get factorial coordinates of individuals
coord = mca$ind$coord
coord

#K-means clustering on factorical coordinates from MCA results
#The idea is to take the extracted dimensions
#from the MCA in order to perform a k-means cluster analysis on them The first approach is to apply K-means
#on factorial coordinates Let’s try k=5 groups
k1 = kmeans(coord, 5)
#what does k1 contain?
attributes(k1)
# size of clusters
k1$size
k1$withinss
# centers coordinates
k1$centers
# between clusters sum of squares
sq = sum(rowSums(k1$centers^2) * k1$size)
sq

# within clusters sum of squares
Wss = sum(k1$withinss)
Wss

# total sum of squares
Tss = sum(rowSums(coord^2))
Tss

sq + Wss
# let's calculate the decomposition of inertia
Ib1 = 100 * sq / (sq + Wss)
Ib1

# let's repeat kmeans, again with k=5
k2 = kmeans(coord, 5)
# between clusters sum of squares
sq = sum(rowSums(k2$centers^2) * k2$size)
Wss = sum(k2$withinss)
# total sum of squares
Tss = sum(rowSums(coord^2))
sq + Wss # Tss = sq + Wss

# let's calculate the decomposition of inertia
Ib2 = 100 * sq / (sq + Wss)
Ib2

# why are we obtaining different results? (Ib1 != Ib2)
# you can keep playing with different values for k
# let's repeat kmeans, again with k=8
k3 = kmeans(coord, 8)
# between clusters sum of squares
sq = sum(rowSums(k3$centers^2) * k3$size)
Wss = sum(k3$withinss)
# total sum of squares
Tss = sum(rowSums(coord^2))
sq + Wss # Tss = sq + Wss

# let's calculate the decomposition of inertia
Ib3 = 100 * sq / (sq + Wss)
Ib3

# Now, let's apply a hierarchical clustering
# first we calculate a distance matrix between individuals
idist = dist(coord)

# then we apply hclust with method="ward"
# notice the computation cost! (it takes a while to finish)
h1 = hclust(idist, method="ward")
h1
# plot dendrogram
plot(h1, labels=FALSE)

# after checking the dendrogram, how many groups would you choose?
# where would you cut the dendrogram?
# let's try 8 clusters
nc = 8
# let's cut the tree and see the size of clusters
c1 = cutree(h1, nc)
table(c1)

# prepare data frame for ggplot
df1 = data.frame(Status=data$Status, mca$ind$coord[,1:2], cluster=as.factor(c1))
# visualize clusters using the first two factorial coordinates
ggplot(data=df1, aes(x=Dim.1, y=Dim.2)) +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_point(aes(colour=cluster), alpha=0.5) +
  labs(x="Dim 1", y="Dim 2") +
  ggtitle("MCA plot with clusters of individuals")

# centers of gravity of the clusters
cog = aggregate(as.data.frame(coord), list(c1), mean)[,-1]
# what's the quality of the hierarchical partition?
sq = sum(rowSums(cog^2) * table(c1))
Ib4 = 100 * sq / Tss
Ib4

# let's consolidate the partition
# we'll apply k-means using the cog's from the hierarchical clustering
k5 = kmeans(coord, center=cog)
k5$size

sq = sum(rowSums(k5$centers^2) * k5$size)
Wss = sum(k5$withinss)
Ib5 = 100 * sq / (sq + Wss)
Ib5

# clustering of large data sets
# first 2 kmeans with k=14
n1 = 14
km1 = kmeans(coord, n1)
km2 = kmeans(coord, n1)
# what's the overlapping between clusters?
table(km2$cluster, km1$cluster)

clas = (k2$cluster - 1)*n1 + k1$cluster
freq = table(clas)
freq[1:10]

# what do we have in freq?
cogclas <- aggregate(as.data.frame(coord), list(clas), mean)[,2:(ncol(coord)+1)]
# perform a hierarchical clustering using cogclas
# compare the computational cost (this is way much faster!)
d2 = dist(cogclas)
h2 = hclust(d2, method="ward", members=freq)

# dendrogram
plot(h2)
#barplot
barplot(h2$height)
# cut tree in nc=8 groups
c2 <- cutree(h2, nc)
# load package mclust
library(mclust)
# check the computational cost
emc <- Mclust(coord, G=7:9)
print(emc)
attributes(emc)
# In this case, we have a probability for each individual
emc$z[1:10,]
# let's see the membership for every individual
emc$classification[1:10]

table(emc$classification)

# what's the quality of the partition?
cog <- aggregate(as.data.frame(coord), list(emc$classification), mean)[,2:(ncol(coord)+1)]
sq <- sum(rowSums(cog^2)*as.numeric(table(c1)))
Ib7 <- 100*sq/Tss
Ib7
# adata emc classification to data frame
df1$EMC.class = as.factor(emc$classification)
# visualize clusters using the first two factorial coordinates
ggplot(data=df1, aes(x=Dim.1, y=Dim.2)) +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_point(aes(colour=EMC.class), alpha=0.5) +
  labs(x="Dim 1", y="Dim 2") +
  ggtitle("MCA plot with clusters of individuals")


require(ggplot2)
require(rpart)

# Let's obtain a decision tree with the function 'rpart'
# using all the variables (both continuous and categorical)
ct = rpart(Status ~ ., data=data)
# let's see how the output looks like
ct

# it's much easier to read a tree with a graphic
plot(ct, margin=0.05, compress=TRUE, main="Decision Tree")
text(ct, use.n=TRUE, pretty=1, all=TRUE, cex=0.7)

n = nrow(data)
learn = sample(1:n, size=round(0.67 * n))
nlearn = length(learn)
ntest = n - nlearn

# selection of model by crossvalidation
# first we need a maximal tree with low value of cp
# and quiprobability of classes
ct1 = rpart(Status ~ ., data = data[learn,], method="class",
            parms = list(prior=c(0.50, 0.50), split='gini'),
            control = rpart.control(cp=0.001, xval=10, maxdepth=15))

# check results of the complexity parameter table
ct1$cptable

# we can use the function 'plotcp' to see the results
# the 'best' tree is the one with the lowest xerror
plotcp(ct1, las=2, cex.axis=0.8)

# what is the minimum XERROR?
min(ct1$cptable[,4])

min.xe = which(ct1$cptable[,4] == min(ct1$cptable[,4]))
# the optimal tree corresponds to a cp=0.003
ct1$cptable[min.xe,]

ct2 = rpart(Status ~ .,
            data = data[learn,],
            parms = list(prior=c(0.50, 0.50), split='gini'),
            control = rpart.control(cp=0.00285, xval=0, maxdepth=15))

# plot
par(mar = c(1,1,2,0.5))
plot(ct2, margin=0.05, compress=TRUE, main="Decision Tree")
text(ct2, use.n=TRUE, pretty=1, all=TRUE, cex=0.5)
summary(ct2)

# calculate error rate in the learning sample
# (this will give a matrix)
ct2.learn = predict(ct2, data=data[learn,])
# create a vector with predicted status
ct2.learnp = rep("", nlearn)
ct2.learnp[ct2.learn[,1] < 0.5] = "pred_neg"
ct2.learnp[ct2.learn[,1] >= 0.5] = "pred_pos"
# let's make a table
status_learn = table(data$Status[learn], ct2.learnp)
# classification error
100 * sum(diag(status_learn)) / nlearn

# calculate error rate in the testing sample
# (this will give a matrix)
ct2.test = predict(ct2, newdata=data[-learn,])
# create a vector with predicted status
ct2.testp = rep("", ntest)
ct2.testp[ct2.test[,1] < 0.5] = "pred_neg"
ct2.testp[ct2.test[,1] >= 0.5] = "pred_pos"
# let's make a table
status_test = table(data$Status[-learn], ct2.testp)
# classification error
100 * sum(diag(status_test)) / ntest

# we'll repeat the same but changing the cp=0.002
ct3 = rpart(Status ~ .,
            data = data[learn,],
            parms = list(prior=c(0.50, 0.50), split='gini'),
            control = rpart.control(cp=0.002, xval=0, maxdepth=15))

par(mar = c(1,1,2,0.5))
plot(ct3, margin=0.05, compress=TRUE, main="Decision Tree")
text(ct3, use.n=TRUE, all=TRUE, cex=0.5)

# calculate error rate in the learning sample
# (this will give a matrix)
ct3.learn = predict(ct3, data=data[learn,])
# create a vector with predicted status
ct3.learnp = rep("", nlearn)
ct3.learnp[ct3.learn[,1] < 0.5] = "pred_neg"
ct3.learnp[ct3.learn[,1] >= 0.5] = "pred_pos"
# let's make a table
table(data$Status[learn], ct3.learnp)
# classification error
100 * sum(diag(table(data$Status[learn], ct3.learnp))) / nlearn

# calculate error rate in the testing sample
# (this will give a matrix)
ct3.test = predict(ct3, newdata=data[-learn,])
# create a vector with predicted status
ct3.testp = rep("", ntest)
ct3.testp[ct3.test[,1] < 0.5] = "pred_neg"
ct3.testp[ct3.test[,1] >= 0.5] = "pred_pos"
# let's make a table
table(data$Status[-learn], ct3.testp)

# classification error
100 * sum(diag(table(data$Status[-learn], ct3.testp))) / ntest

# concentration curve
# the positive predictions on the test sample
pred.test = ct2.test[,1]
# the number of individuals in each value
totn = table(-pred.test) / ntest
ac_totn = 100 * cumsum(as.numeric(totn))
# ranking the predictions
rank_pred.test = rank(pred.test)
# how many positive are in each leave?
Status.test = data$Status[-learn]
table(Status.test)
npos = table(Status.test)[1]

tapply(Status.test == "good", rank_pred.test, sum)

ac_true.pos = 100 * cumsum(rev(as.numeric(totn))) / npos

# load package FactoMineR and ggplot2
require(FactoMineR)
require(ggplot2)

n = nrow(data)
learn = sample(1:n, size=round(0.67 * n))
nlearn = length(learn)
ntest = n - nlearn

# "whole enchilada" logistic regression model
gl1 = glm(Status ~ ., data=data[learn,], family=binomial)

# use the 'summary' function to obtain more details on the logistic model
# (pay attention to the signicance of the coefficients)
summary(gl1)

# let's use the 'anova' function to get a sequential analysis of
# variance of the model fit (ie importance of variables in the model)
anova(gl1)

step(gl1)
# new model
glf <- glm(formula = Status ~ Seniority + Age + Income + Debt + Amount + Finrat +
             
             seniorityR + expensesR + assetsR + priceR + savingsR + Home + Marital + Records +
             Job, family = binomial, data = data[learn, ])

# check summary
summary(glf)

# check coefficients
exp(glf$coefficients)

# re-expressed fitted values
glf$fitted.values = 1 - glf$fitted.values
# create vector for predictions
glfpred = rep(NA, length(glf$fitted.values))
glfpred[glf$fitted.values < 0.5] = 0
glfpred[glf$fitted.values >= 0.5] = 1
# how is the prediction? (confusion matrix)
table(data$Status[learn], glfpred)
# error rate
error_rate.learn = 100*sum(diag(table(data$Status[learn], glfpred))) / nlearn
error_rate.learn

# let's use the test data to get predictions
glft = predict(glf, newdata=data[-learn,])
pt = 1 / (1 + exp(-glft))
pt = 1 - pt

# vector of predicted values
glftpred = rep(NA, length(pt))
glftpred[pt < 0.5] = 0
glftpred[pt >= 0.5] = 1

# confusion matrix
table(data$Status[-learn], glftpred)

error_rate.test = 100*sum(diag(table(data$Status[-learn], glftpred))) / ntest
error_rate.test
ac_tot = 100*(1:ntest) / ntest
pt.ord = order(pt, decreasing=T)
Status_test = data$Status[-learn]
npos = table(Status_test)[2]
ac_pos_test = 100*cumsum(Status_test[pt.ord] == "good") / npos
plot(ac_tot, ac_pos_test, type="l", lwd=2,
     main="Concentration Curve")
lines(ac_tot, ac_tot, col="gray70", lwd=2)

nneg = ntest - npos
ac_neg_test = 100*cumsum(Status_test[pt.ord]=="bad") / nneg
plot(ac_neg_test, ac_pos_test, type="l", lwd=2, main="ROC Curve", col="blue")
lines(ac_neg_test, ac_neg_test, col="grey70", lwd=2)

