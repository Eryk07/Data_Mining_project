houses <- read.table("C:/Users/errry/Documents/R skrypty/realest.txt", header=TRUE, sep="\t")
colnames(houses)

##(a)

lmHouses <- lm(Price ~ Bedroom+Space+Room+Lot+Tax+Bathroom+Garage+Condition, data=houses)
summary(lmHouses)
plot(lmHouses)
houses2 <- houses
houses2$Bedroom <- houses2[,2] + 1

original = predict(lmHouses, houses)
extra.bedroom = predict(lmHouses, houses2)
df = data.frame(original,extra.bedroom,extra.bedroom - original)
df

lmHousesB <- lm(Price ~ Bedroom, data=houses)
summary(lmHousesB)

plot(houses$Bedroom, houses$Price, col="green")
abline(lmHousesB, col="red")

predicted.by.bedrooms = predict(lmHousesB, houses)
predicted.by.bedrooms.extra = predict(lmHousesB, houses2)
df = data.frame(original,extra.bedroom,predicted.by.bedrooms,
			predicted.by.bedrooms.extra)
df

##(b)

predict(lmHouses, data.frame(Bedroom=3,Space=1500,Room=8,Lot=40,
					Tax=1000,Bathroom=5,Garage=1,Condition=0))

x <- count(houses, "Condition")
x
pie(x$freq)

summary(houses)
houses
