library(regclass)
library(mltools)
library(data.table)
library(rsq)
library(DescTools)
library(blorr)
library(ltm)
library(polycor)
library(rcompanion)
library(xlsx)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(ROCR)
heart <- heart2
heart <- read_excel("heart2.xlsx")
#define variables
heart$sex<-as.factor(heart$sex)
heart$cp<-as.factor(heart$cp)
heart$fbs<-as.factor(heart$fbs)
heart$restecg<-as.factor(heart$restecg)
heart$exang<-as.factor(heart$exang)
heart$slope<-as.factor(heart$slope)
heart$ca<-as.factor(heart$ca)
heart$thal<-as.factor(heart$thal)
heart$target<-as.factor(heart$target)
#remove outliers
heart<-heart[-c(55,56,70,394,527,614,834,159,193,465),]
#devide test and train
smp_siz = floor(0.7*nrow(heart)) 
smp_siz  
set.seed(123) 
train_ind = sample(seq_len(nrow(heart)),size = smp_siz)  
train =heart[train_ind,] 
test=heart[-train_ind,]  

#encoding
df = train
df = one_hot(as.data.table(df))
#create a model
reg_log <- glm(target_1 ~ .-target_0  - thal_3 - ca_4 - slope_2 - sex_1 - cp_3 - fbs_1 - restecg_2 - restecg_1 - exang_1 - thal_0 - fbs_0-cp_2 -ca_0 -chol -slope_0 , data = df, family = "binomial")
summary(reg_log)
plot(reg_log)
#logistic regression
logLik(reg_log)
-2*logLik(reg_log)
#p value compare with model 0 variables
with(reg_log, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
train$log_odds <- predict(reg_log)
train$prawdopodobienstwo <- exp(train$log_odds)/(1+exp(train$log_odds))

#VIF
VIF(reg_log)
check_regression(reg_log,seed=F)

#BIC
BIC(reg_log)

#R^2 McFadden
rsq(reg_log)
PseudoR2(reg_log, which = "McFaddenAdj")
blr_rsq_mcfadden(reg_log)
blr_rsq_mcfadden_adj(reg_log)

#Accuracy
fitted.results <- predict(reg_log,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$target)
print(paste('Accuracy',1-misClasificError))
misClasificError

#ROC
p <- predict(reg_log, test, type="response")
pr <- prediction(p, test$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
0.8647186

#correlation
#categorical and numeric
biserial.cor(heart$age, heart$target)
biserial.cor(heart$trestbps, heart$target)
biserial.cor(heart$chol, heart$target)
biserial.cor(heart$thalach, heart$target)
biserial.cor(heart$oldpeak, heart$target)

#categorical and categorical
fisher.test(heart$sex, heart$target)
cramerV(heart$sex, heart$target)
cramerV(heart$cp, heart$target)
cramerV(heart$fbs, heart$target)
cramerV(heart$restecg, heart$target)
cramerV(heart$exang, heart$target)
cramerV(heart$slope, heart$target)
cramerV(heart$ca, heart$target)
cramerV(heart$thal, heart$target)

#write to csv
write.xlsx2(heart, "chorobyserca.xlsx" , sheetName = "heart",
            col.names = TRUE, row.names = TRUE, append = FALSE)
#boxplots
ggplot(heart, aes(x=thalach, y=target)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + ggtitle("Wykres pudełkowy dla zmiennej thalach")
#geom_bar 
ggplot(heart, aes(x=sex)) + geom_bar(aes(fill = target), position = "dodge") 


#coefficient zmienności
odch<-sqrt(var(heart$oldpeak))
odch
srednia<-mean(heart$oldpeak)
srednia
zmiennosci  = odch/srednia
zmiennosci


#min i max
minmax <-function(x)
{
  max = max(x)
  min = min(x)
  print(max)
  print(min) 
}  

minmax(heart$age)
minmax(heart$trestbps)
minmax(heart$chol)
minmax(heart$thalach)
minmax(heart$oldpeak)


#histogram and distribution
ggplot(heart, aes(x=chol)) + 
geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
binwidth=.3, colour="black", fill="white") +
geom_density(alpha=.2, fill="#FF6666") + ggtitle("Histogram i wykres gęstości dla zmiennej cholesterolu")


#test Rossnera
library(EnvStats)
test <- rosnerTest(heart$ca, k = 10)
test


probabilities <- predict(reg_log, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

mydata <- train %>%
  dplyr::select_if(is.numeric) 
mydata= mydata %>% select(trestbps, chol, thalach, oldpeak, age )

library(tidyr)
predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictValue", -logit)

ggplot(mydata, aes(logit, predictValue))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

reg_log.data <- augment(reg_log) %>% 
  mutate(index = 1:n()) 
reg_log.data %>% top_n(3, .cooksd)
ggplot(reg_log.data, aes(index, .std.resid)) + 
  geom_point(aes(color = target), alpha = .5) +
  theme_bw()


#normality test of residuals 
library(tseries)
jarque.bera.test(s$resid)
shapiro.test(s$resid)


#QQ plots of residuals
qqnorm(s$resid,main="QQ plot of normal data",pch=19)
qqline(s$resid)


#histogram normalności składników resztowych
hist( s$resid, breaks=20)



#wiarygodnosci
with(reg_log, null.deviance - deviance)
with(reg_log, df.null - df.residual)
with(reg_log, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(reg_log)

#linearity
fitted = predict(reg_log)
resid = resid(reg_log, type = 'deviance')
s=data.frame(fitted, resid)


  s %>% ggplot(aes(fitted, resid))+
  geom_point()

  install.packages("DHARMa")
  library(DHARMa)
  citation("DHARMa")  
  testDispersion(simulationOutput)
  simulationOutput <- simulateResiduals(fittedModel = reg_log, plot = T)
  e = residuals(simulationOutput)
  plotQQunif(simulationOutput) 
  plotResiduals(simulationOutput)
  hist(simulationOutput)
  testResiduals(simulationOutput)
  testZeroInflation(simulationOutput)  
  plot(simulationOutput)  
  
  
  simulationOutput <- simulateResiduals(fittedModel = reg_log)
  plot(simulationOutput)
  
  res <- cor(df)
  round(res, 2)
  library(corrplot)
  corrplot(res, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  
  
  testZeroInflation(simulationOutput)
  plot(simulationOutput)  
  testUniformity(simulationOutput)  
  

