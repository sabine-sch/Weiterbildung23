
## lesen Sie die Daten ein. Bitte überprüfen Sie, dass der Pfad stimmt
dataLungCap <- read.table("src/LungCapData2.csv", sep = ",", header = TRUE)
## do it balanced

str(dataLungCap)

dim(dataLungCap)

boxplot(LungCap ~ Smoke, data = dataLungCap, xlab = "Raucher", ylab = "Lungenkapazität")
t.test(LungCap ~ Smoke, data = dataLungCap, alternative = "two.sided", var.equal = TRUE)

mod <- lm(LungCap ~ Smoke, data = dataLungCap)
summary(mod)$coef

par(mfrow = c(2,2))
plot(modUnadj)
par(mfrow= c(1,1))

