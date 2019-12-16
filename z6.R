iris <- read.table("C:/Users/errry/Documents/R skrypty/iris.txt", header=TRUE, sep=",")
colnames(iris)

##(a)
install.packages("caTools")
library(caTools)

require(caTools)

set.seed(123)   
sample = sample.split(iris,SplitRatio = 0.75) 
train = subset(iris,sample ==TRUE)
test = subset(iris,sample==FALSE)

library(rpart)

fit <- rpart(class ~ sepal.length+sepal.width+petal.length+petal.width, data=train,
		method="class")

printcp(fit)
plotcp(fit)

# Wyœwietliæ jak wygl¹da wytrenowane drzewo w formie tekstowej i graficznej
install.packages("rpart.plot")
library(rpart.plot)
rpart.rules(fit)

plot(fit, uniform=TRUE,
   main="Classification Tree for Iris genre")
text(fit, use.n=TRUE, all=TRUE, cex=.6)

plot(iris$petal.width, iris$petal.length, col=iris$class)
legend(0, 7, fill=c("black","red","green"), 
	legend=c("setosa","versicolor","virginica"))
abline(h=2.5)
abline(v=1.65)
##(d)


pred <- predict(fit, test, type = "class")
pred
table(pred, test$class)



