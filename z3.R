savings <- read.table("C:/Users/errry/Documents/R skrypty/savings.txt", header=TRUE, sep="\t")
savings

##(a)

summary(savings)
lmSavings <- lm(savings$Savings ~ savings$dpi+savings$ddpi+savings$Pop15+savings$Pop75,
			data=savings)
summary(lmSavings)

##(b)

library(olsrr)

ols_plot_resid_stand(lmSavings)
##(c)

leverage <- ols_leverage(lmSavings)
plot(leverage)

rstudent(lmSavings)
ols_plot_resid_stud(lmSavings)
qqnorm(lmSavings$res)
qqline(lmSavings$res)

##(d)

dffits(lmSavings)
ols_plot_dffits(lmSavings)

dfbetas(lmSavings)
ols_plot_dfbetas(lmSavings

cd <- cooks.distance(lmSavings)
ols_plot_cooksd_bar(lmSavings)


##(e)

savings2 <- savings[(cd != max(cd)),]
savings2
lmSavings2 <- lm(savings2$Savings ~ savings2$dpi+savings2$ddpi+savings2$Pop15+savings2$Pop75,
			data=savings2)

summary(lmSavings)
summary(lmSavings2)

anova(lmSavings, lmSavings2)

savings3 <- savings[-c(23, 46, 49),]
savings3
lmSavings3 <- lm(savings3$Savings ~ savings3$dpi+savings3$ddpi+savings3$Pop15+savings3$Pop75,
			data=savings3)
summary(lmSavings3)

##(f)

dfbetas(lmSavings2)
ols_plot_dfbetas(lmSavings2)






