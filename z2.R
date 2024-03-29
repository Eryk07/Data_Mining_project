airpol <- read.table("C:/Users/errry/Documents/R skrypty/airpollution.txt", header=TRUE, sep="\t")
usedvars <- c("Mortality", "Education", "X.NonWhite", "income", "JanTemp", "JulyTemp", "NOx")
airpol <- airpol[usedvars]
airpol

##(a)

summary(airpol)
boxplot(airpol$NOx, 
		main="NOx",
		 col = c("blue"), horizontal=T)
lmMortality <- lm(airpol$Mortality ~ airpol$NOx, data=airpol) #dopasowa� model liniowy

##(b)

lmMortality #wsp�czynnik nachylenia prostej 
summary(lmMortality) # i jego b��d standardowy

plot(airpol$NOx, airpol$Mortality, col="green") #sprawdzenie jak wygl�da dopasowany model
abline(lmMortality, col="red")

##(c)

lmMortality2 <- lm(airpol$Mortality ~ log(airpol$NOx), data=airpol) #dopasowa� model liniowy z log(NOx)

lmMortality2 #wsp�czynnik nachylenia prostej 
summary(lmMortality2) # i jego b��d standardowy

plot(airpol$NOx, airpol$Mortality, col="green") #sprawdzenie jak wygl�da dopasowany model
abline(lmMortality2, col="red")

##(d)

new <- data.frame(NOx = airpol$NOx)
predicted <- predict(lmMortality2, newdata=log(new))
airpol2 <- airpol

#rezydua studentyzowane:
airpol2$d <- rstudent(lmMortality2)
airpol2$d

#histogram dla oszacowania rozk�ad rezydu�w:
hist(airpol2$d, main="Histogram of rstudent", xlab="r", col="pink")
#po analize histogramu przyjmuj� �e du�e rezydua to te spoza zakresu [-1;1]
airpol3 <- airpol2[(airpol2$d > -1 & airpol2$d < 1),]

ols_plot_resid_stud(lmMortality2)

lmMortality3 <- lm(airpol3$Mortality ~ log(airpol3$NOx), data=airpol3) #dopasowa� model liniowy z log(NOx)
summary(lmMortality3) # R^2, b��d standardowy 

plot(airpol3$NOx, airpol3$Mortality, col="green", xlim=c(-2,330)) #sprawdzenie jak wygl�da dopasowany model
abline(lmMortality3, col="red")

