install.packages("MASS")
library(MASS)
data(package="MASS")

##(a)

cars=as.data.frame(Cars93)
colnames(cars) 
cars

# wybraæ kolumny  Min.Price, MPG.city,MPG.highway, Weight, Origin, Type

myvars <- c("Min.Price", "MPG.city", "MPG.highway", "Weight", "Origin", "Type")
cars2 <- cars[myvars]
cars2

#utworzyæ nowe zmienne

cars$Lp100km.city <- ((100*3.8)/(1.6*cars$MPG.city)) #zu¿ycie paliwa litr/100km w mieœcie
cars$Lp100km.highway <- ((100*3.8)/(1.6*cars$MPG.highway)) #zu¿ycie paliwa litr/100km na autostradzie
cars$Weight.kg <- cars$Weight * 0.4536 #waga samochodu w kg
cars$Min.Price.PLN <- cars$Min.Price * 3.35 #cena samochodu w tys. PLN

myvars <- c("Min.Price", "Min.Price.PLN", "MPG.city", "Lp100km.city", "MPG.highway", "Lp100km.highway", "Weight", "Weight.kg", "Origin", "Type")
cars2 <- cars[myvars]
cars2 

##(b)

qua95 <- quantile(cars$Min.Price.PLN, .95)
qua95
x <- cars[c("Make", "Min.Price.PLN")]
x2 <- x[x$Min.Price.PLN > qua95,]
x2

##(e)

install.packages("plyr")
library(plyr)
x <- count(cars, "Type")
x
barplot(x$freq, main="Car types", names.arg=x$Type) #wykres s³upkowy
pie(x$freq, labels=x$Type, main="Car types") #wykres ko³owy

##(f)

amer <- cars[cars$Origin=="USA",]
non_amer <- cars[cars$Origin=="non-USA",]

boxplot(amer$Lp100km.city,  non_amer$Lp100km.city, 
		main="Fuel usage in city (litres per 100 km)",
		 col = c("blue","red"))
legend(0.5, 6, fill=c("blue","red"), legend=c("USA","non-USA"), horiz=TRUE)

##(g)

par(mfrow=c(1,2))
plot(cars$MPG.city, cars$Min.Price,
	main="Price to MPG in city", 
	xlab="Miles per Gallon in city", ylab="Car price",
	col="red")
text(30,44,sprintf("r = %s",round(cor(cars$MPG.city, cars$Min.Price),digits=4)))
plot(cars$MPG.highway, cars$MPG.city,
	main="MPG in city to MPG on highway", 
	xlab="Miles per Gallon on highway", ylab="Miles per Gallon in city",
	col="darkblue")
text(35,45,sprintf("r = %s",round(cor(cars$MPG.highway, cars$MPG.city),digits=4)))

summary(cars$MPG.highway/cars$MPG.city)

##(h)

hist(cars$Weight, main="Histogram of Weight", xlab="Weight in pounds", col="lightblue")
summary(cars)