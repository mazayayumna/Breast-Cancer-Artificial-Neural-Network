install.packages("caret") #data preparation
library(caret)
install.packages("neuralnet") #building the model
library(neuralnet)
install.packages("vcd") #data visualization
library(vcd)
library(MASS) #data

bcancer=read.table("D:\\KULIAHAN\\sem 7\\DSS\\Project 1\\breastcancer.txt",header=T)
str(bcancer)
table(bcancer$diagnosis)
dummies = dummyVars(diagnosis~.,bcancer, fullRank=TRUE)
dummies
bcancer.2 = as.data.frame(predict(dummies, newdata=bcancer))
bcancer.2$diagnosis = ifelse(bcancer$diagnosis=="B",1,0)
set.seed(123)
trainIndex= createDataPartition(bcancer.2$diagnosis, p = .7, list = FALSE, times = 1)
bcancerTrain= bcancer.2[trainIndex,]
bcancerTest= bcancer.2[-trainIndex,]
n = names(bcancerTrain)
form <-as.formula(paste("diagnosis ~", paste(n[!n %in% "use"], collapse = "+")))
Form
fit = neuralnet(form, data=bcancerTrain, err.fct="ce", linear.output=FALSE)
fit$result.matrix
plot(fit)
res = compute(fit, bcancerTrain[,1:31])
predTrain= res$net.result
predTrain= ifelse(predTrain>=0.5,1,0)
table(predTrain, bcancerTrain$diagnosis)
res2 = compute(fit, bcancerTest[,1:31])
predTest= res2$net.result
predTest= ifelse(predTest>=0.5,1,0)
table(predTest, bcancerTest$diagnosis)
which(predTest==0 & bcancerTest$diagnosis==1)
bcancerTest[58,]
bcancerTest[59,]
fit2 = neuralnet(form, data=bcancerTrain, hidden=c(3,2), err.fct="ce", linear.output=FALSE)
plot(fit2)
res = compute(fit2, bcancerTrain[,1:31])
predTrain= res$net.result
predTrain= ifelse(predTrain>=0.5,1,0)
table(predTrain, bcancerTrain$diagnosis)
res2 = compute(fit2, bcancerTest[,1:31])
predTest= res2$net.result
predTest= ifelse(predTest>=0.5,1,0)
table(predTest, bcancerTest$diagnosis)
which(predTest==0 & bcancerTest$diagnosis==1)
(73+97)/170
(97/(73+97))	#TPR
(0/(0+0))	#FPR
library(pROC)
roc_df <- data.frame(
	TPR=(97/(73+97)), #TPR
	FPR=(0/(0+0))) #FPR
rectangle <- function(x, y, width, height, density=12, angle=-45, ...) 
  polygon(c(x,x,x+width,x+width), c(y,y+height,y+height,y), 
          density=density, angle=angle, ...)
roc_df <- transform(roc_df, 
  dFPR = c(diff(FPR), 0),
  dTPR = c(diff(TPR), 0))
plot(0:10/10, 0:10/10, type='n', xlab="FPR", ylab="TPR")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")
with(roc_df, {
  mapply(rectangle, x=FPR, y=0,   
         width=dFPR, height=TPR, col="green", lwd=2)
  mapply(rectangle, x=FPR, y=TPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  lines(FPR, TPR, type='b', lwd=3, col="red")
})