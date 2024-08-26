#Instalar y cargar los siguientes paquetes
#Seleccionarlos todos y correr varias veces
#Si todo va bien, aparecer?n todos en azul en la consola
#http://www.scielo.org.mx/scielo.php?script=sci_arttext&pid=S2007-40182011000200007
#https://www.curveexpert.net/support/documentation/

###################################################################################

if(!require(readxl)){install.packages("readxl")}#Para bs (efectos de variables)
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(AICcmodavg)){install.packages("AICcmodavg")}#AICC
if(!require(AICcmodavg)){install.packages("AICcmodavg")}#AICC
if(!require(mosaic)){install.packages("mosaic")}#regresi?n no lineal
if(!require(nlstools)){install.packages("nlstools")}#funciones:confint2
if(!require(broom)){install.packages("broom")}#function glance
if(!require(qpcR)){install.packages("qpcR")}#funciones RSS y residuos
if(!require(randtests)){install.packages("randtests")}#funci?n runs.test datos continuos
if(!require(investr)){install.packages("investr")}#funci?n predFit modelo no lineal
if(!require(cvTools)){install.packages("cvTools")}#funciones: mspe, rmspe, mape, tmspe, rtmspe
if(!require(nlraa)){install.packages("nlraa")}#funciones: SSquadp,simulate_nls, boot_nls, IC_tab
if(!require(reshape2)){install.packages("reshape2")}#funci?n "melt"
if(!require(rcompanion)){install.packages("rcompanion")}#funci?n "accuracy"
if (!require(boot)) install.packages("boot")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(MuMIn)) install.packages("MuMIn")
if (!require(gridExtra)) install.packages("gridExtra")

if (!require(gridExtra)) install.packages("gridExtra")

library("readxl")
library(openxlsx)
library(nlstools)
library(boot)
library(ggplot2)
library(MuMIn)
library(gridExtra)

###################################################################################
options (scipen = 999, digits = 5)#Para evitar notaci?n cient?fica 
geone<-par(no.readonly=TRUE)#Se guardan los ajustes de graficos originales

teca <- read_excel("C:/Users/jolum/teca.xlsx", sheet = "promedios 2023" ) #base de datos 

Clean_BD_teca <- read_excel("C:/Users/jolum/Clean_BD_teca.xlsx", sheet = "Sheet 1" )


names(teca)
#Renombrar las columnas
teca <- teca %>%
  rename(
    NS_med = `NS. de Medición`,
    `Edad_a`= `Edad en Años` ,
    `Altura_m` = `Altura Total (m)`
    
  )




#depuracion de la base de datos 

#clasificaci?n pares e impares (para entrenamiento y validaci?n)
teca <- teca%>%
  mutate(Fil_model_1 = ifelse(NS_med %in% c( "6"), "Grupo A", "Grupo B"))

# Guardar el objeto en un archivo XLSX
write.xlsx(teca, file = "teca.xlsx", rowNames = TRUE)

Clean_BD_teca <- teca %>%
  filter(
    !(ID_parcela %in% c("I92_00015_01", "I93_10_001", "I93_10_002", "I93_10_003","I91_00024_002","I91_00024_001","I91_043_1","I91_00001_001","I92_00020_01","I92_00018_01")) &
      !(ID_parcela_med %in% c("I91_23_02_5","I91_00012_1_5","I91_00012_2_4","I91_00012_2_3","I92_00013_001_7", "I92_00013_001_10", "I92_00013_005_10", "I92_00013_006_8",
                              "I92_00013_006_10", "I92_00013_006_11", "I92_00013_007_8", "I92_00013_007_10",
                              "I92_00013_007_11", "I92_00014_01_2", "I92_00020_01_6", "I93_200_1",
                              "I93_200_2", "I93_200_3", "I93_200_4", "I93_200_5", "I93_200_6", "I93_200_7",
                              "I93_5_002_8", "I93_5_002_10", "I93_5_002_11", "I93_5_002_12", "I93_5_002_13",
                              "I93_5_002_14", "I93_5_002_15", "I93_5_002_16", "I93_5_002_17", "I93_5_003_1",
                              "I93_5_005_9", "I93_5_005_10", "I91_00012_2_3", "I93_6_1_1", "I93_6_1_6", "I93_6_1_7","I91_00012_2_2","I91_23_01_5","I91_043_1_3")) &
      Fil_model_1 == "Grupo B")


#write xlsx
Data_model_excel <- "C:/Users/jolum/Clean_BD_teca.xlsx"
# Guardar el objeto en un archivo XLSX
write.xlsx(Clean_BD_teca, file = Data_model_excel, rowNames = TRUE)
    



    
#____ Diagrama de disperci?n_____



ggplot(Clean_BD_teca, aes(x = Edad_a, y = Altura_m)) +
  geom_point(size = 2, alpha = 0.7) +
  labs( x = "Edad (años)",
       y = "Altura (m)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

 #}_____________modelo chapman richards_____   
    
ChapRi <- nls(Altura_m ~ b_0 * (1 - exp(-0.13356136601835 * Edad_a)^b_2), 
              data = Clean_BD_teca, 
              start = c(b_0 = 26.95365117421616, b_2 = 1.004983520766939))   
summary(ChapRi)
    
ChapRi <- nls(Altura_m ~ b_0 * (1 - exp(-b_1* Edad_a)^1.004983520766939), 
              data = Clean_BD_teca, 
              start = c(b_0 = 26.95365117421616, b_1 =0.13356136601835))  

#____________Grafico_________

ggplot(Clean_BD_teca, aes(x = Edad_a, y = Altura_m)) +
  geom_point() +
  geom_line(aes(y = predict(ChapRi, newdata = Clean_BD_teca)), color = "blue") +
  labs(title = "Modelo Chapman-Richards",
       x = "Edad (años)",
       y = "Altura Dominante (m)") +
  theme_classic()


#Intervalos de confianza
x<-confint2(ChapRi)#md
Y<-confint2(ChapRi)#md
out<-list(ChapRi=x,ChapRi=Y);out

  

summary(nlsJack(ChapRi));plot(nlsJack(ChapRi))
summary(nlsBoot(ChapRi))#Se utiliza bootstrapping no param?trico (forma de imitar la repetici?n de un experimento)
glance(ChapRi)#deviance en lugar de R2
tidy(ChapRi)
augment(ChapRi)
logLik(ChapRi)
logLik(ChapRi, REML = FALSE)
AICc(ChapRi)
AIC(ChapRi)
n<-nrow(Clean_BD_teca);k<-1; SC<-deviance(ChapRi)#k=n?mero de variables regresoras, deviance=SC=RSS=Suma de cuadrados de residuos
AIC<-n*log(SC/n)+2*k+(2*k*(k+1))/(n-k-1);AIC#CurveExpert Professional

no <- length(Clean_BD_teca$Altura_m)# N?mero de observaciones
p <- 2

#___ no. de parametros del modelo___________

##___- capacidad predictiva____

errors_squared <- (residuals(ChapRi))^2# Calcula los errores al cuadrado
RMSE <- sqrt(sum(errors_squared) / (n - p))# Calcula el RMSE
print(RMSE)# Imprime el RMSE ajustado
CME <- (sum(errors_squared) / (n - p)) # Calcula el cuadrado medio del error
print(CME)# Imprime el CME ajustado
EMC <- (sum(errors_squared) / (n))# Calcula el Error medio cuadratico
print(EMC)# Imprime el EMC ajustado
R2_squared<-(1-(1-0.8328 )*((no-1)/(no-p)))#r cudrado ajustado__________sustitur el R2 por resultado de las metricas para cada modelo__
print(R2_squared)# Imprime el R^2 ajustado

#Suma del cuadrado de los residuos (SSR, devianza)
deviance(ChapRi)#RSS
RSS(ChapRi)

#Error cuadr?tico medio de predicci?n
mspe(Altura_m, predict(ChapRi),includeSE=FALSE)
crovSchu<-cvFit(ChapRi, x=Edad_a, y=Altura_m, cost=mspe, K=5, R=10, data=Clean_BD_teca);crovSchu


models<-list(ChapRi)
accuracy(models, digits=4)#R2 de efron mide cu?nto mejor se vuelve un ajuste en comparaci?n con una linea recta horizontal a trav?s de los puntos.
overview (ChapRi)#Suma del cuadrado de los residuos
test.nlsResiduals(nlsResiduals(ChapRi))

plot(nlsResiduals(ChapRi))

resChap<- nlsResiduals(ChapRi)



# _____CUMPLIMIENTO DE SUPUESTOS ___

install.packages("car")

library(ggplot2)
library(nlstools)
library(lmtest)
library(car)



#prueba de corridas de Wald-Wolfowitz: aleatoridad de datos (independencia)
vresd<-as.vector(residuals(ChapRi))
runs.test(vresd, alternative="two.sided", pvalue="normal", plot=TRUE)
library(lmtest)
bptest(ChapRi)

#____independencia de los residuos 



stats::acf(residuals(ChapRi))



# Prueba de Bartlett para homogeneidad de varianzas
bartlett.test(Altura_m ~ Edad en Meses, data = Clean_BD_teca)

# Prueba de Durbin-Watson para autocorrelación
dwtest(ChapRi)
# Prueba de Durbin-Watson para autocorrelación calculada manualmente
durbinWatson <- function(residuals) {
  n <- length(residuals)
  numerator <- sum(diff(residuals)^2)
  denominator <- sum(residuals^2)
  dw <- numerator / denominator
  return(dw)
}

dw_stat <- durbinWatson(residuals(ChapRi))
print(dw_stat)
plot(dw_stat)
plot(durbinWatson())


#MATRIZ DE VARIANZAS Y COVARIANZAS DE LOS PAR?METROS ESTIMADOS
mavc<-vcov(ChapRi)#sigma2*(X'X)^(-1)
mavc
(mcep<-mavc/(summary(ChapRi)$sigma^2))# dividido por la varianza, es la matriz que muestra CEPRO
det(mavc)#Se espera que sea cercano a cero si no existe colinealidad



#}_____________modelo Schumacher_____   

schuma<-nls(Altura_m~b_0*exp(-b_1/Edad_a), data=Clean_BD_teca,
            start=c(b_0=29.837935467404, b_1=4.032758184137));coef(schuma)
summary(schuma)


#____________Grafico_________

ggplot(Clean_BD_teca, aes(x = Edad_a, y = Altura_m)) +
  geom_point() +
  geom_line(aes(y = predict(schuma, newdata = Clean_BD_teca)), color = "blue") +
  labs(title = "Modelo Schumacher",
       x = "Edad (años)",
       y = "Altura Dominante (m)") +
  theme_classic()


#Intervalos de confianza
x<-confint2(schuma)#md
Y<-confint2(schuma)#md
out<-list(schuma=x,schuma=Y);out


summary(nlsJack(schuma));plot(nlsJack(schuma))
summary(nlsBoot(schuma))#Se utiliza bootstrapping no param?trico (forma de imitar la repetici?n de un experimento)
glance(schuma)#deviance en lugar de R2
tidy(schuma)
augment(schuma)
logLik(schuma)
logLik(schuma, REML = FALSE)
AICc(schuma)
AIC(schuma)
n<-nrow(Clean_BD_teca);k<-1; SC<-deviance(schuma)#k=n?mero de variables regresoras, deviance=SC=RSS=Suma de cuadrados de residuos
AIC<-n*log(SC/n)+2*k+(2*k*(k+1))/(n-k-1);AIC#CurveExpert Professional

no <- length(Clean_BD_teca$Altura_m)# N?mero de observaciones
p <- 2 #___ no. de parametros del modelo___________

##___- capacidad predictiva____

errors_squared <- (residuals(schuma))^2# Calcula los errores al cuadrado
RMSE <- sqrt(sum(errors_squared) / (n - p))# Calcula el RMSE
print(RMSE)# Imprime el RMSE ajustado
CME <- (sum(errors_squared) / (n - p)) # Calcula el cuadrado medio del error
print(CME)# Imprime el CME ajustado
EMC <- (sum(errors_squared) / (n))# Calcula el Error medio cuadratico
print(EMC)# Imprime el EMC ajustado
R2_squared<-(1-(1-0.8125  )*((no-1)/(no-p)))#r cudrado ajustado__________sustitur el R2 por resultado de las metricas para cada modelo__
print(R2_squared)# Imprime el R^2 ajustado

#Suma del cuadrado de los residuos (SSR, devianza)
deviance(schuma)#RSS
RSS(schuma)

#Error cuadr?tico medio de predicci?n
mspe(altura_m, predict(schuma),includeSE=FALSE)
crovSchu<-cvFit(schuma, x=Edad_a, y=Altura_m, cost=mspe, K=5, R=10, data=Clean_BD_teca);crovSchu


models<-list(schuma)
accuracy(schuma, digits=4)#R2 de efron mide cu?nto mejor se vuelve un ajuste en comparaci?n con una linea recta horizontal a trav?s de los puntos.
overview (schuma)#Suma del cuadrado de los residuos
test.nlsResiduals(nlsResiduals(schuma))

NLS_schuma <- nlsResiduals(schuma)
SHCHU_A<- plot(NLS_schuma, which=2)
SHCHU_B<- plot(NLS_schuma, which=4)
SHCHU_C<- plot(NLS_schuma, which=6)



# _____CUMPLIMIENTO DE SUPUESTOS ___
#prueba de corridas de Wald-Wolfowitz: aleatoridad de datos (independencia)
vresd<-as.vector(residuals(schuma))
runs.test(vresd, alternative="two.sided", pvalue="normal", plot=TRUE)
#two.sided, left.sided,right.sided

par(cex.axis = 1.5, cex.lab = 1.5)

stats::acf(residuals(schuma))


#MATRIZ DE VARIANZAS Y COVARIANZAS DE LOS PAR?METROS ESTIMADOS
mavc<-vcov(schuma)#sigma2*(X'X)^(-1)
mavc
(mcep<-mavc/(summary(schuma)$sigma^2))# dividido por la varianza, es la matriz que muestra CEPRO
det(mavc)#Se espera que sea cercano a cero si no existe colinealidad

#}_____________modelo weibull_______  

Weibull <- nls (Altura_m ~ b_0 * (1 - exp(-0.1317061863111375 * Edad_a^b_2)), 
                data = Clean_BD_teca, 
                start = c(b_0 = 26.84659117766349, b_2 = 1.008685008405085))   
summary(Weibull)


Weibull <- nls (Altura_m ~ b_0 * (1 - exp(-b_1 * Edad_a^1.008685008405085)), 
                data = Clean_BD_teca, 
                start = c(b_0 = 26.84659117766349, b_1 = 0.1317061863111375))  
#____________Grafico_________

ggplot(Clean_BD_teca, aes(x = Edad_a, y = Altura_m)) +
  geom_point() +
  geom_line(aes(y = predict(Weibull, newdata = Clean_BD_teca)), color = "blue") +
  labs(
       x = "Edad (años)",
       y = "Altura Dominante((m)") +
  theme_classic()


#Intervalos de confianza
x<-confint2(Weibull)#md
Y<-confint2(Weibull)#md
out<-list(Weibull=x,Weibull=Y);out


#Bondad de ajuste y metricas____________
summary(nlsJack(Weibull));plot(nlsJack(Weibull))
summary(nlsBoot(Weibull))#Se utiliza bootstrapping no param?trico (forma de imitar la repetici?n de un experimento)
glance(Weibull)#deviance en lugar de R2
tidy(Weibull)
augment(Weibull)
logLik(Weibull)
logLik(Weibull, REML = FALSE)
AICc(Weibull)
AIC(Weibull)
n<-nrow(Clean_BD_teca);k<-1; SC<-deviance(Weibull)#k=n?mero de variables regresoras, deviance=SC=RSS=Suma de cuadrados de residuos
AIC<-n*log(SC/n)+2*k+(2*k*(k+1))/(n-k-1);AIC#CurveExpert Professional

no <- length(Clean_BD_teca$Altura_m)# N?mero de observaciones
p <- 3 #___ no. de parametros del modelo___________

##___- capacidad predictiva____

errors_squared <- (residuals(Weibull))^2# Calcula los errores al cuadrado
RMSE <- sqrt(sum(errors_squared) / (n - p))# Calcula el RMSE
print(RMSE)# Imprime el RMSE ajustado
CME <- (sum(errors_squared) / (n - p)) # Calcula el cuadrado medio del error
print(CME)# Imprime el CME ajustado
EMC <- (sum(errors_squared) / (n))# Calcula el Error medio cuadratico
print(EMC)# Imprime el EMC ajustado
R2_squared<-(1-(1-0.8125  )*((no-1)/(no-p)))#r cudrado ajustado__________sustitur el R2 por resultado de las metricas para cada modelo__
print(R2_squared)# Imprime el R^2 ajustado

#Suma del cuadrado de los residuos (SSR, devianza)
deviance(Weibull)#RSS
RSS(Weibull)

#Error cuadr?tico medio de predicci?n
mspe(Altura_m, predict(Weibull),includeSE=FALSE)
crovWeibull<-cvFit(Weibull, x=Edad_a, y=Altura_m, cost=mspe, K=5, R=10, data=Clean_BD_teca);crovWeibull


models<-list(Weibull)
accuracy(Weibull, digits=4)#R2 de efron mide cu?nto mejor se vuelve un ajuste en comparaci?n con una linea recta horizontal a trav?s de los puntos.
overview (Weibull)#Suma del cuadrado de los residuos
test.nlsResiduals(nlsResiduals(Weibull))
plot(nlsResiduals(Weibull))





# _____CUMPLIMIENTO DE SUPUESTOS ___
#prueba de corridas de Wald-Wolfowitz: aleatoridad de datos (independencia)
vresd<-as.vector(residuals(Weibull))
runs.test(vresd, alternative="two.sided", pvalue="normal", plot=TRUE)
#two.sided, left.sided,right.sided


stats::acf(residuals(Weibull))




#MATRIZ DE VARIANZAS Y COVARIANZAS DE LOS PAR?METROS ESTIMADOS
mavc<-vcov(Weibull)#sigma2*(X'X)^(-1)
mavc
(mcep<-mavc/(summary(Weibull)$sigma^2))# dividido por la varianza, es la matriz que muestra CEPRO
det(mavc)#Se espera que sea cercano a cero si no existe colineali

#____ para generar graficos de residuos para cada modelo

NLS_ChapRi <- nlsResiduals(ChapRi)
NLS_schuma <- nlsResiduals(schuma)
NLS_Weibull <- nlsResiduals(Weibull)


CHAP_A <- plot(NLS_ChapRi, which = 2)
CHAP_B <- plot(NLS_ChapRi, which = 4)
CHAP_C <- plot(NLS_ChapRi, which = 6)

# Schumacher
SCHU_A <- plot(NLS_schuma, which = 2)
SCHU_B <- plot(NLS_schuma, which = 4)
SCHU_C <- plot(NLS_schuma, which = 6)

# Weibull
WEIB_A <- plot(NLS_Weibull, which = 2)
WEIB_B <- plot(NLS_Weibull, which = 4)
WEIB_C <- plot(NLS_Weibull, which = 6)



