library(rsq)
library(ltm)
library(polycor)
library(rcompanion)
library(xlsx)
library(ggplot2)
library(dplyr)
heart <- read_excel("heart2.xlsx")
heart$sex<-as.factor(heart$sex)
heart$cp<-as.factor(heart$cp)
heart$fbs<-as.factor(heart$fbs)
heart$restecg<-as.factor(heart$restecg)
heart$exang<-as.factor(heart$exang)
heart$slope<-as.factor(heart$slope)
heart$ca<-as.factor(heart$ca)
heart$thal<-as.factor(heart$thal)
heart$target<-as.factor(heart$target)

#regresja logistyczna
reg_log <- glm(target ~ age +sex+chol +cp+trestbps + fbs + restecg + thalach + exang + oldpeak + slope + ca +thal , data = heart, family = "binomial")
summary(reg_log)
logLik(reg_log)
-2*logLik(reg_log)
with(reg_log, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
heart$log_odds <- predict(reg_log)
heart$prawdopodobienstwo <- exp(heart$log_odds)/(1+exp(heart$log_odds))

#R^2
rsq(reg_log)

#zmienna ciągła i kateogryczna
biserial.cor(heart$age, heart$target)
biserial.cor(heart$trestbps, heart$target)
biserial.cor(heart$chol, heart$target)
biserial.cor(heart$thalach, heart$target)
biserial.cor(heart$oldpeak, heart$target)

#kategoryczna i kategorcyzna

fisher.test(heart$sex, heart$target)
cramerV(heart$sex, heart$target)
cramerV(heart$cp, heart$target)
cramerV(heart$fbs, heart$target)
cramerV(heart$restecg, heart$target)
cramerV(heart$exang, heart$target)
cramerV(heart$slope, heart$target)
cramerV(heart$ca, heart$target)
cramerV(heart$thal, heart$target)

write.xlsx2(heart, "chorobyserca.xlsx" , sheetName = "heart",
            col.names = TRUE, row.names = TRUE, append = FALSE)

ggplot(heart, aes(x=oldpeak, y=target)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

ggplot(heart, aes(x=sex)) + geom_bar(aes(fill = target), position = "dodge")


#Wspolczynnik zmiennosci

odch<-sqrt(var(heart$oldpeak))
odch
srednia<-mean(heart$oldpeak)
srednia
zmiennosci  = odch/srednia
zmiennosci

