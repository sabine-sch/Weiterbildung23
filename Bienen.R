
# laden Sie bitte folgende Packages
library(survival)
library(survminer) 
library(ggplot2)

## Lesen Sie die daten ein. Achten Sie, dass der Datenpfad stimmt
## wenn Sie das dirctory nicht wissen, auf welchem Sie arbeiten: 
getwd()
exp5 <- read.table("src/Data_Experiment5.csv", sep = ";", header = TRUE)

## wenn sie Plobleme haben mit den Hochladen der Daten oder auf einer online-shell arbeiten
exp5 <- data.frame(Treatment = c(rep("Control", 49),
                                 rep("Roundup No Glyphosate", 50)),
                   Time = c(rep(1440, 49), 0, rep(10, 3), 1440, rep(0, 3),                             rep(10, 6), 1440, 0, rep(10,3), 1440,
                            rep(0,3), 10, 10, 0, 10, 10, 1440, 1440,                                   rep(10,4), 20, rep(0, 4), 
                            10, rep(0, 4), 10, 0, 0, rep(10,3)),
                  Event = c(rep(0, 49), rep(1,4), 0, 
                            rep(1, 14), 0, rep(1, 30)))
                          


                        
colnames(exp5)

# Experiment 5: Reporduzieren Sie die Kaplan-Meier Kurve und den Chi-square test

# Mit er Surv() funktion erstellt man ein Survival Objekt
exp5$Surv <- Surv(exp5$Time, exp5$Event)

exp5[, c("Time", "Event", "Surv")]
## was bedeutet das + am Ende des Surv Objekts?


model5 <- survfit(Surv(Time, Event) ~ Treatment, data = exp5)

## reproduzieren Sie Figur aus Paper
plot(model5)

## oder etwas hübscher:
ggsurvplot(model5,legend= "right", title= "Experiment 5") 

## Erstellen Sie eine Tabelle (Behandlung vs events) 
tbl5 <- table(exp5$Treatment, exp1$Event)
tbl5 

## reproduzieren Sie chi-square test
cs5 <- chisq.test(tbl5) 
cs5
## 

## Experiment 1:
exp1 <- read.table("src/Data_Experiment1.csv", sep = ";", header = TRUE)

exp1 <- exp1[!is.na(exp1$Event), ]



exp1 <- data.frame(Treatment = c(rep("Control", 54), 
                                  rep("Roundup Ready to Use 100%", 53),
                                  rep("Roundup ProActive", 54)),
                    Time = c(rep(1440, 54), rep(0, 5), 10, rep(0, 3), 
                             10, 10, 20, rep(0,4), 10, 30, rep(0, 20),
                             rep(10, 3), rep(0, 3), 10, 1440, 1440, 0, 10, 20, 
                             rep(1440, 3), 20, rep(1440, 23), rep(10, 3), 
                             rep(1440, 3), 10, rep(1440, 11), 20, rep(1440, 5),
                             0, 0, rep(1440, 4)),
                    Event = c(1, rep(0, 5), 1, rep(0, 47), rep(1, 50), 0, 0, 0, 
                              1, rep(0, 11), 1, rep(0, 11), 1, 1, 1, 
                              0, 0, 0, 1, 1, 1, rep(0, 9), rep(1, 5), 
                              0, 1,1,1,0,0,0)
                              
                    )

## Experiment 4:

#########################################
## Cox-ph regression model Experiment 1

cox <- coxph(Surv(Time, Event) ~ Treatment, data = exp1)
summary(cox)
## Warum ist 
summary(cox)$coef["TreatmentRoundup No Glyphosate", "exp(coef)"]
## die gesuchte hazard rate? 
## Warum ist es eine Rate? 

## Tip: das cox-modell hat einen logit-link 

