library(readr)
library(tidyr)
library(stats)
library(dplyr)
library(haven)
library(tidyverse)
library(caret)
library(MASS)
library(randomForest)
library(ROCR)
library(ROSE)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(ggcorrplot)
library(ROCR)

#wgranie danych, dane pochodzą z 2021 BRFSS Survey https://www.cdc.gov/brfss/annual_data/annual_2021.html 

df <- read_xpt("dane_sas/2021.xpt", col_select = c(DIABETE4, SEXVAR, GENHLTH, PHYSHLTH,
                                                   MEDCOST1, EXERANY2,"_RFHYPE6" ,"_RFCHOL3" , CVDSTRK3, "_MICHD", ASTHMA3,  ADDEPEV3,
                                                  EDUCA, INCOME3, DIFFWALK, "_SMOKER3","_URBSTAT", "_IMPRACE","_HLTHPLN","_AGEG5YR",
                                                   "_BMI5CAT","_RFDRHV7", "_FRTLT1A", "_VEGLT1A", "FRNCHDA_"))

#usunięcie braków danych 

df <- as.data.frame(df)
df <- na.omit(df)

#zmiana nazwy kolumn 

colnames(df)[colnames(df)=="DIABETE4"] = "diabetes"
colnames(df)[colnames(df)=="SEXVAR"] = "sex"
colnames(df)[colnames(df)=="GENHLTH"] = "genhealth"
colnames(df)[colnames(df)=="PHYSHLTH"] = "physhealth"
colnames(df)[colnames(df)=="MEDCOST1"] = "medcost"
colnames(df)[colnames(df)=="EXERANY2"] = "physact"
colnames(df)[colnames(df)=="_RFHYPE6"] = "highBP"
colnames(df)[colnames(df)=="_RFCHOL3"] = "highchol"
colnames(df)[colnames(df)=="CVDSTRK3"] = "stroke"
colnames(df)[colnames(df)=="_MICHD"] = "heartdis"
colnames(df)[colnames(df)=="ASTHMA3"] = "asthma"
colnames(df)[colnames(df)=="ADDEPEV3"] = "depression"
colnames(df)[colnames(df)=="EDUCA"] = "education"
colnames(df)[colnames(df)=="INCOME3"] = "income"
colnames(df)[colnames(df)=="DIFFWALK"] = "diffwalk"
colnames(df)[colnames(df)=="_SMOKER3"] = "smoker"
colnames(df)[colnames(df)=="_URBSTAT"] = "urban"
colnames(df)[colnames(df)=="_IMPRACE"] = "race"
colnames(df)[colnames(df)=="_HLTHPLN"] = "insurance"
colnames(df)[colnames(df)=="_AGEG5YR"] = "age"
colnames(df)[colnames(df)=="_BMI5CAT"] = "bmi"
colnames(df)[colnames(df)=="_RFDRHV7"] = "alco"
colnames(df)[colnames(df)=="_FRTLT1A"] = "fruit"
colnames(df)[colnames(df)=="_VEGLT1A"] = "veggie"
colnames(df)[colnames(df)=="FRNCHDA_"] = "fries"

##PRZYGOTOWANIE DANYCH

#cukrzyca
df <- df[df$diabetes !=7,] #dont know
df <- df[df$diabetes !=9,] #refused
df <- df[df$diabetes !=4,] #prediabetes 
df$diabetes[df$diabetes == 3] <- 0 #in pregnancy
df$diabetes[df$diabetes == 2] <- 0 #no diabetes
summary(df$diabetes)

#płeć
df$sex[df$sex == 2] <- 0 #kobieta
df <- df[df$sex !=7,] #dont know
df <- df[df$sex !=9,] #refused
summary(df$sex)
df$sex <-as.numeric(df$sex)

#generalny stan zdrowia 1 - good 5 - bad 
df <- df[df$genhealth !=7,] #dont know
df <- df[df$genhealth !=9,] #refused
summary(df$genhealth)

#zdrowie fizyczne - ilość dni ze złym stanem
df$physhealth[df$physhealth == 88] <- 0 #no days
df <- df[df$physhealth !=77,] #dont know
df <- df[df$physhealth !=99,] #refused
df$physhealth[df$physhealth != 0] <- 1
summary(df$physhealth)

#rezygnacja z opieki zdrowotnej 
df$medcost[df$medcost == 2] <- 0 #no access
df <- df[df$medcost !=7,] #dont know
df <- df[df$medcost !=9,] #refused
summary(df$medcost)

#aktywność fizyczna 
df$physact[df$physact== 2] <- 0 #not
df <- df[df$physact !=7,] #dont know
df <- df[df$physact!=9,] #refused
summary(df$physact)

#nadciśnienie
df$highBP[df$highBP == 1] <- 0 
df$highBP[df$highBP == 2] <- 1 
df <- df[df$highBP !=9,] #refused
summary(df$highBP)

#cholesterol - tylko osoby które miały badany cholesterol 
df$highchol[df$highchol == 1] <- 0
df$highchol[df$highchol == 2] <- 1 #not high cholesterol
df <- df[df$highchol !=9,] #refused
summary(df$highchol)

#wylew
df$stroke[df$stroke == 2] <- 0 #no stroke
df <- df[df$stroke !=7,] #dont know
df <- df[df$stroke !=9,] #refused
summary(df$stroke)

#choroba serca lub zawal
df$heartdis[df$heartdis== 2] <- 0 #not
df <- df[df$heartdis !=7,] #dont know
df <- df[df$heartdis!=9,] #refused
summary(df$heartdis)

#astma
df$asthma[df$asthma== 2] <- 0 #not
df <- df[df$asthma !=7,] #dont know
df <- df[df$asthma !=9,] #refused
summary(df$asthma)

#depresja
df$depression[df$depression == 2] <- 0 #no
df <- df[df$depression !=7,] #dont know
df <- df[df$depression !=9,] #refused
summary(df$depression)

#edukacja
df <- df[df$education !=9,] #refused
summary(df$education)

#zarobki
df <- df[df$income !=77,] #dont know
df <- df[df$income !=99,] #refused
summary(df$income)

#trudność w chodzeniu po schodach
df$diffwalk[df$diffwalk == 2] <- 0 #no
df <- df[df$diffwalk !=7,] #dont know
df <- df[df$diffwalk !=9,] #refused
summary(df$diffwalk)

#palenie papierosów
df <- df[df$smoker !=9,] #refused
summary(df$smoker)

#miasto czy wieś
df$urban[df$urban == 2] <- 0 #no
summary(df$urban)

#rasa
df$race[df$race == 1] <- "white"
df$race[df$race == 2] <- "black"
df$race[df$race == 3] <- "asian"
df$race[df$race == 4] <- "native"
df$race[df$race == 5] <- "hispanic"
df$race[df$race == 6] <- "other"
df$race <- as.factor(df$race)

#ubezpieczenie zdrowotne
df$insurance[df$insurance == 2] <- 0 #no
df <- df[df$insurance !=9,] #refused
summary(df$insurance)
    
#wiek
df <- df[df$age != 14,] #missing
    
#bmi
summary(df$bmi)
    
#alkohol
df$alco[df$alco == 1] <- 0 #no
df$alco[df$alco == 2] <- 1 #no
df <- df[df$alco !=9,] #refused
summary(df$alco)

#owocki 
df$fruit[df$fruit == 2] <- 0 #no
df <- df[df$fruit !=9,] #refused
summary(df$fruit)
    
#warzywa 
df$veggie[df$veggie == 2] <- 0 #no
df <- df[df$veggie !=9,] #refused
summary(df$veggie)
    
#frytki
df$fries <- df$fries/100
df$fries[df$fries < 0.25] <- 0
df$fries[df$fries >= 0.25] <- 1
summary(df$fries)

##BUDOWA MODELI

#podział na zbiór treningowy i testowy 
set.seed(3)
df$diabetes <- as.factor(df$diabetes)
inTraining <- createDataPartition(df$diabetes, p = .25, list = FALSE)
training <- df[ inTraining,] 
testing  <- df[-inTraining,]
summary(training)

#pięciokrotna walidacja krzyzowa 
fitControl <- trainControl(
  method = "cv",
  number = 5)

barplot(prop.table(table(df$diabetes)),
        col = rainbow(4), 
        ylim = c(0, 1),
        main = "Rozkład zmiennej objaśnianej")

#model logitowy 
model_log <- glm(diabetes~., data = training, family = 'binomial')
summary(model_log)

#model logitowy + stepwise 
model_step <- stepAIC(model_log, direction="both", trace = FALSE)
summary(model_step)

#drzewo 
tree <- train(diabetes ~ ., data = training, 
              method = "rpart", 
              trControl = fitControl)
rpart.plot(tree$finalModel)

#las losowy 
rf <- randomForest(diabetes ~., 
                   data = training)
varImpPlot(rf)

#macirrz korelacji 
df2 <- dplyr::select_if(df, is.numeric)
r <- cor(df2, use="complete.obs")
ggcorrplot(r, hc.order = TRUE, type = "lower", lab = TRUE)

##EWALUACJA MODELI

#macierze pomyłek 
cm1 <- table(ifelse(predict(model_log, newdata = testing, type = "response") > 0.5, 1, 0), testing$diabetes)
confusionMatrix(cm1)
cm2 <- table(ifelse(predict(model_step, newdata = testing, type = "response") > 0.5, 1, 0),testing$diabetes)
confusionMatrix(cm2)
### Drzewa
cm3 <- table(predict(tree, new = testing, type = "raw"), testing$diabetes)
confusionMatrix(cm3)
### Las
cm4 <- table(predict(rf, new = testing, type = "class"), testing$diabetes)
confusionMatrix(cm4)

#ROC
roc_log <- predict(model_log, newdata = testing, type = "response")
roc_log <- as.vector(roc_log)

roc_step <- predict(model_step, newdata = testing, type = "response")
roc_step <- as.vector(roc_step)

roc_tree <- predict(tree, newdata = testing, type = "prob")
roc_tree <- as.vector(roc_tree[,2])

roc_rf <- predict(rf, newdata = testing, type = "prob")
roc_rf <- as.vector(roc_rf[,2])

#krzywa ROC wizualizacja 
pred <- prediction(roc_step, testing$diabetes)
perf <- performance(pred ,"tpr","fpr")
plot(perf, lwd=2, col ="blue")

pred2 <- prediction(roc_log, testing$diabetes)
perf2 <- performance(pred2 ,"tpr","fpr")
plot(perf2, lwd = 2, col ="green", add = TRUE)

pred3 <- prediction(roc_tree, testing$diabetes)
perf3 <- performance(pred3 ,"tpr","fpr")
plot(perf3, lwd = 2, col ="red", add = TRUE)

pred4 <- prediction(roc_rf, testing$diabetes)
perf4 <- performance(pred4 ,"tpr","fpr")
plot(perf4, lwd = 2, col ="violet", add = TRUE)

auc_log <- (performance(prediction(roc_log, testing$diabetes), "auc")@y.values[[1]])
auc_step <- (performance(prediction(roc_step, testing$diabetes), "auc")@y.values[[1]])
auc_tree <- (performance(prediction(roc_tree, testing$diabetes), "auc")@y.values[[1]])
auc_rf <- (performance(prediction(roc_rf, testing$diabetes), "auc")@y.values[[1]])

##BALANSOWANIE ZBIORU 

df_balanced <- ovun.sample(diabetes~., data=df, method = "both",
                           p = 0.5,
                           seed = 222)$data

#podzial na zbior uczacy i testowy
set.seed(3)
df_balanced$diabetes <- as.factor(df_balanced$diabetes)
inTraining <- createDataPartition(df_balanced$diabetes, p = .25, list = FALSE)
training2 <- df_balanced[ inTraining,] 
fitControl <- trainControl(
  method = "cv",
  number = 5)
testing2  <- df_balanced[-inTraining,]
summary(training2)

#wykres zmiennej objaśnianej
barplot(prop.table(table(training2$diabetes)),
        col = rainbow(4), 
        ylim = c(0, 1),
        main = "Rozkład zmiennej objaśnianej")

#model logitowy 
model_log <- glm(diabetes~., data = training2, family = 'binomial')
summary(model_log)

#model logitowy + stepwise 
model_step <- stepAIC(model_log, direction="both", trace = FALSE)
summary(model_step)

#drzewo 
tree <- train(diabetes ~ ., data = training2, 
              method = "rpart", 
              trControl = fitControl)
rpart.plot(tree$finalModel)

#las losowy 
rf <- randomForest(diabetes ~., 
                   data = training2)
varImpPlot(rf)

##WALIDACJA WYNIKOW DLA ZBIORU ZBALANSOWANEGO

#macierze bledow

cm1 <- table(ifelse(predict(model_log, newdata = testing2, type = "response") > 0.5, 1, 0), testing2$diabetes)
confusionMatrix(cm1)
cm2 <- table(ifelse(predict(model_step, newdata = testing2, type = "response") > 0.5, 1, 0),testing2$diabetes)
confusionMatrix(cm2)
cm3 <- table(predict(tree, new = testing2, type = "raw"), testing2$diabetes)
confusionMatrix(cm3)
cm4 <- table(predict(rf, new = testing2, type = "class"), testing2$diabetes)
confusionMatrix(cm4)

#ROC
roc_log <- predict(model_log, newdata = testing2, type = "response")
roc_log <- as.vector(roc_log)

roc_step <- predict(model_step, newdata = testing2, type = "response")
roc_step <- as.vector(roc_step)

roc_tree <- predict(tree, newdata = testing2, type = "prob")
roc_tree <- as.vector(roc_tree[,2])

roc_rf <- predict(rf, newdata = testing2, type = "prob")
roc_rf <- as.vector(roc_rf[,2])

pred <- prediction(roc_step, testing2$diabetes)
perf <- performance(pred ,"tpr","fpr")
plot(perf, lwd=2, col ="blue")

pred2 <- prediction(roc_log, testing2$diabetes)
perf2 <- performance(pred2 ,"tpr","fpr")
plot(perf2, lwd = 2, col ="green", add = TRUE)

pred3 <- prediction(roc_tree, testing2$diabetes)
perf3 <- performance(pred3 ,"tpr","fpr")
plot(perf3, lwd = 2, col ="red", add = TRUE)

pred4 <- prediction(roc_rf, testing2$diabetes)
perf4 <- performance(pred4 ,"tpr","fpr")
plot(perf4, lwd = 2, col ="violet", add = TRUE)

#AUC
auc_log <- (performance(prediction(roc_log, testing2$diabetes), "auc")@y.values[[1]])
auc_step <- (performance(prediction(roc_step, testing2$diabetes), "auc")@y.values[[1]])
auc_tree <- (performance(prediction(roc_tree, testing2$diabetes), "auc")@y.values[[1]])
auc_rf <- (performance(prediction(roc_rf, testing2$diabetes), "auc")@y.values[[1]])
ci
