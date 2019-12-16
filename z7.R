iris <- read.table("C:/Users/errry/Documents/R skrypty/iris.txt", header=TRUE, sep=",")

##(a)

#znormalizowaæ dane liczbowe
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
iris.norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))

#podzieliæ na zbiór treningowy

library(caTools)
require(caTools)

set.seed(123)   
sample = sample.split(iris.norm,SplitRatio = 0.75) 
train = subset(iris.norm,sample ==TRUE)
train_class = subset(iris$class, sample==TRUE)
test = subset(iris.norm,sample==FALSE)
test_class = subset(iris$class, sample==FALSE)

#algorytm 3-najbli¿szych s¹siadów

library(class)
train

nn3 <- knn(train, test, cl=train_class, k=3)
nn3
tab <- table(nn3, test_class)

nn7 <- knn(train, test, cl=train_class, k=7)
nn7
table(nn7, test_class)

#ewaluacja klasyfikatora
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


