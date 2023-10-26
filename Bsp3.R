## datne falls Sie auf einer Online-shell arbeiten:
dat <- data.frame(nfails.field = c(0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 2,
                                   0, 0, 0, 0, 0, 0, 2, 0, 1),
                     temp = c(18.9, 21.1, 20.6, 20.0, 19.4, 22.2, 22.8, 21.1,
                              13.9, 17.2, 21.1, 25.6, 19.4, 11.7, 19.4, 23.9,
                              21.1, 27.2, 24.4, 26.1, 23.9, 24.4, 14.4))


## laden Sie das Datenset
dat <- read.table("src/Bsp3.txt", header = TRUE)

## schauen Sie Sich die Daten an
dim(dat)
str(dat)
head(dat)

## machen Sie "date" y = zu einem Datum-objekt
dat$date <- as.Date(dat$date, format = "%d/%m/%y")
## bitte nur ausführen, wenn Sie die Daten aus dem txt -file ausgelesen haben.

# schauen Sie die Struktur nochmals an
str(dat)

subset <- dat[dat$fail.field == 1, ]

# machen Sie den plot
plot(nfails.field ~ temp, data = subset)

# achtung gewisse Punkte liegen ubereinander => jittern hilft um alle Punkte zu sehen 
plot(jitter(nfails.field, 0.5) ~ temp, data = subset)

# und jetzt schreiben wir es noch huebsch an
plot(jitter(nfails.field, 0.5) ~ temp, data = subset, 
     ylab = "Anzahl Versagen", xlab = "Temperatur in Celsius", main = "Challenger Daten")

# welche Fehlüberlegung wurde gemacht?
# wie sollten die Daten statessen dargestellt werden?
# die Temperatur am Start war -0.6 Grad, wie Wahrscheinlich ist ein O-Ring Versagen?


library(ggplot2)
ggplot(dat, aes(x = temp, y = nfails.field, col = as.factor(nfails.field))) + 
  #geom_point() +
  geom_jitter() + theme_bw()


## wenn man die Wahrscheinlichkeit für ein O-Ring versagen berechnen will: 
## eine option ist ein logistisches Modell

## Konstruieren Sie eine binäre Variable für Versagen 0 = nein, 1 = ja
dat$event <- ifelse(dat$nfails.field == 0, 0, 1)
table(dat[, c("nfails.field", "event")])

## die "family" bezeichnet der Typ des glm.
## "biomial" hat als default den logit-link
mod.logit <- glm(event ~ temp, data = dat, family = "binomial")
summary(mod.logit)

## die estimates müssen noch zurücktransponiert werden
exp(summary(mod.logit)$coef["temp", "Estimate"])

## BTW: der default-wert ist "gaussian" (mit identity link),
## also ein normales lm 
?glm


## wenn Sie den geschätzten Wert für ein O-Ring Versagen herausfinden wollen:
predict(mod.logit, newdata = data.frame(temp = 0), "response") 
## wahrscheinlichkeit für mid ein O-Ring versagen

## Für alle, denen es langweilig ist:
## 1. machen Sie einen Plot mit den predikteten Werten in Abhängigkeit der Temperaturen
## 2. Ergänzen Sie die Messwerte in den Plot aus 1
## 3. Konzipieren Sie eine Schulstunde mit den Daten für eine gewünschte Schulstufe
##    Welche Konzepte würden sie einführen? Welche auslassen? Warum
## 4. Schreiben Sie eine shiny-app zu den Challenger-Daten
        