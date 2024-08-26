# Cargar el paquete necesario  
library(ISLR2)  



Clean_BD_teca <- read_excel("C:/Users/jolum/Clean_BD_teca.xlsx", sheet = "Sheet 1" )

# Cargar el paquete   
library(ISLR2)  

  #___________chapman________________

#  función para calcular el error cuadrático medio  
mse <- function(actual, predicted) {  
  return(mean((actual - predicted)^2))  
}  

# Validación Cruzada K-Fold  
set.seed(1)  
k <- 5  # dividir la data en 5  
folds <- sample(1:k, nrow(Clean_BD_teca), replace = TRUE)  

cv_errors <- rep(0, k)  

for (j in 1:k) {  
  #  conjuntos de entrenamiento y prueba  
  train_set <- Clean_BD_teca[folds != j, ]  
  test_set <- Clean_BD_teca[folds == j, ]  
  
  # Ajustar el modelo   
  ChapRi <- nls(Altura_m ~ b_0 * (1 - exp(-0.13356136601835 * Edad_a)^b_2), 
                data = train_set, 
                start = c(b_0 = 26.95365117421616, b_2 = 1.004983520766939)) 
  
  # Predecir en el conjunto de prueba  
  predictions <- predict(ChapRi, newdata = test_set)  
  
  # Calcular el error cuadrático medio para  j  
  cv_errors[j] <- mse(test_set$Altura_m, predictions)  
}  

# Calcular el error cuadrático medio promedio  
mean_cv_error <- mean(cv_errors)  
cat("Error cuadrático medio (K-Fold):", mean_cv_error, "\n")  


# modelo schumacher ___________________
cv_errors_schuma <- rep(0, k)  

for (j in 1:k) {  
  # Crear conjuntos de entrenamiento y prueba  
  train_set <- Clean_BD_teca[folds != j, ]  
  test_set <- Clean_BD_teca[folds == j, ]  
  
  # Ajustar el modelo Schuma  
  schuma <- nls(Altura_m ~ b_0 * exp(-b_1 / Edad_a), 
                data = train_set, 
                start = c(b_0 = 29.837935467404, b_1 = 4.032758184137)) 
  
  # Predecir en el conjunto de prueba  
  predictions <- predict(schuma, newdata = test_set)  
  
  # Calcular el error cuadrático medio para el pliegue j  
  cv_errors_schuma[j] <- mse(test_set$Altura_m, predictions)  
}  

# Calcular el error cuadrático medio promedio
mean_cv_error_schuma <- mean(cv_errors_schuma)  
cat("Error cuadrático medio (K-Fold) para el modelo Schuma:", mean_cv_error_schuma, "\n")


#______modelo weibull____

cv_errors_weibull <- rep(0, k)  

for (j in 1:k) {  
  # Crear conjuntos de entrenamiento y prueba  
  train_set <- Clean_BD_teca[folds != j, ]  
  test_set <- Clean_BD_teca[folds == j, ]  
  
  # Ajustar el modelo Weibull  
  Weibull <- nls(Altura_m ~ b_0 * (1 - exp(-0.1317061863111375 * Edad_a^b_2)), 
                 data = train_set, 
                 start = c(b_0 = 26.84659117766349, b_2 = 1.008685008405085)) 
  
  # Predecir en el conjunto de prueba  
  predictions <- predict(Weibull, newdata = test_set)  
  
  # Calcular el error cuadrático medio para el pliegue j  
  cv_errors_weibull[j] <- mse(test_set$Altura_m, predictions)  
}  

# Calcular el error cuadrático medio promedio
mean_cv_error_weibull <- mean(cv_errors_weibull)  
cat("Error cuadrático medio (K-Fold) para el modelo Weibull:", mean_cv_error_weibull, "\n")




#____ grafico de correlacion 

# Cargar los paquetes necesarios
library(readxl)
library(car)
library(ggplot2)

# Leer el archivo de datos
Clean_BD_teca <- read_excel("C:/Users/jolum/Clean_BD_teca.xlsx", sheet = "Sheet 1")

# Ajustar el modelo no lineal a todo el conjunto de datos
ChapRi <- nls(Altura_m ~ b_0 * (1 - exp(-0.13356136601835 * Edad_a)^b_2), 
              data = Clean_BD_teca, 
              start = c(b_0 = 26.95365117421616, b_2 = 1.004983520766939))


residuals <- resid(ChapRi)

# Calcular el estadístico de Durbin-Watson manualmente
dw_statistic <- sum(diff(residuals)^2) / sum(residuals^2)

# Imprimir el estadístico de Durbin-Watson
cat("Estadístico de Durbin-Watson:", dw_statistic, "\n")

# Graficar los residuos en función del tiempo (o del índice de la observación)
ggplot(data = data.frame(Index = 1:length(residuals), Residuals = residuals), aes(x = Index, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Índice de la Observación", y = "Residuos", title = "Residuos vs. Índice de la Observación") +
  theme_minimal()

# Calcular los valores ajustados
fitted_values <- fitted(ChapRi)

# Graficar los residuos en función de los valores ajustados
ggplot(data = data.frame(Fitted = fitted_values, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Valores Ajustados", y = "Residuos", title = "Residuos vs. Valores Ajustados") +
  theme_minimal()

