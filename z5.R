gala <- read.table("C:/Users/errry/Documents/R skrypty/gala_data.txt", header=TRUE, sep="\t")
gala

##(a)
lmSpecies <- lm(Species ~ Area+Elevation+Nearest+Scruz+Adjacent, data=gala)
summary(lmSpecies)

ols_plot_resid_stand(lmSpecies)

leverage <- ols_leverage(lmSpecies)
plot(lmSpecies)

ols_plot_resid_stud(lmSpecies)

plot(predict(lmSpecies,gala),	#gala$Species,
	resid(lmSpecies), 
     ylab="Residuals", xlab="Predicted species", 
     main="Residuals and predictions") 
abline(0, 0)   

##(b)
gala$Species2 <- sqrt(gala$Species)

lmSpecies2 <- lm(Species2 ~ Area+Elevation+Nearest+Scruz+Adjacent, data=gala)
summary(lmSpecies2)

plot(predict(lmSpecies2,gala),	#gala$Species,
	resid(lmSpecies2), 
     ylab="Residuals", xlab="Predicted species", 
     main="Residuals and predictions in sqrt(Species) model",
	ylim=c(-100,150)) 
abline(0, 0)   

lmSpecies3 <- lm(Species2 ~ Area+Elavation+Scruz+Adjacent, data=gala)
summary(lmSpecies3)


