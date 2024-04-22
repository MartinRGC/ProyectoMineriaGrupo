
#install.packages("readxl")
library(readxl)
#C:/Users/51972/Documents/GitHub/ProyectoMineriaGrupo/Bank_Personal_Loan_Modelling.xlsx
#C:/Mineria Datos/ProyectoMineriaGrupo/Bank_Personal_Loan_Modelling.xlsx
DataBank <- read_excel("C:/Mineria Datos/ProyectoMineriaGrupo/Bank_Personal_Loan_Modelling.xlsx", sheet = "Data")

head(DataBank)

Databank <- subset(DataBank, select = -c(ID, `ZIP Code`))

# Resumen descriptivo
summary(DataBank)
# Variable respuesta: Personal Loan -- Cualitativa (préstamo Personal: ¿Aceptó este cliente el préstamo personal ofrecido en la última campaña?)
# Modelo de clasificación (Binaria) --- variable dummy: toma valores de 0 y 1
summary(DataBank$`Personal Loan`)


#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   0.096   0.000   1.000 


var(DataBank$`Personal Loan`) #Varianza: 0.08680136

sd(DataBank$`Personal Loan`) # Desviación Standar: 0.2946207


#La desviación estándar es relativamente baja (0.2946207), 
#esto sugiere que la mayoría de los valores de Personal Loan están cerca de la media.
#Esto generalmente significa que la mayoría de los valores son 0 (No aceptó el préstamo personal) 
#y una menor proporción son 1 (Aceptó el préstamo personal).


# Cargamos el paquete corrplot
library(corrplot)

# Calculamos la matriz de correlación de las variables cuantitativas
correlation_matrix <- cor(DataBank[, c("Age", "Experience", "Income", "Family", "CCAvg", "Mortgage")], use = "complete.obs")

# Creamos el gráfico de correlación
corrplot(correlation_matrix, method = "circle")

# Las variables Age, Experience e Income presentan una fuerte correlación positiva entre si
# Conclusión: A medida que la edad y la experiencia de un cliente aumentan, también lo hace su ingreso.


# Correlación entre los Ingresos y el gasto promedio en tarjetas de crédito
corInc_Gastp <- cor(DataBank$Income, DataBank$CCAvg, use = "complete.obs") # 0.6459926
print(corInc_Gastp)


#Ambas variables presentan una correlación positiva moderada.
#Es decir, a medida que los ingresos de un cliente 
#aumentan, también lo hace su gasto promedio en tarjetas de crédito, y viceversa. 


# Asegúrate de tener tus datos en un dataframe llamado 'niveles'

# Seleccionamos solo las columnas cuantitativas
databank_num <- DataBank[, sapply(DataBank, is.numeric)]

# Calculamos la matriz de correlación
correlation_matrix <- cor(databank_num , use = "complete.obs")

# Imprime la matriz de correlación de las variables cuantitativas
print(correlation_matrix)

#Edad y Experiencia tienen una alta correlación positiva de aproximadamente 0.994, 
#lo que indica que a medida que aumenta la edad, la experiencia también tiende a aumentar.

#Ingresos y CCAvg (gasto promedio en tarjetas de crédito por mes) también tienen una 
#alta correlación positiva de aproximadamente 0.646, lo que sugiere que las personas 
#con mayores ingresos tienden a gastar más en sus tarjetas de crédito.

#Ingresos y Préstamo personal tienen una correlación positiva de aproximadamente 0.502,
#lo que indica que las personas con mayores ingresos tienen más probabilidades de tener
#un préstamo personal.

#Cuenta de CD (Cuenta de Certificado de Depósito) y Préstamo personal tienen una 
#correlación positiva de aproximadamente 0.316, lo que sugiere que las personas con 
#una cuenta de CD tienen más probabilidades de tener un préstamo personal.

#Familia e Ingresos tienen una correlación negativa de aproximadamente -0.157, 
#lo que sugiere que a medida que aumenta el tamaño de la familia, los ingresos tienden
#a disminuir.


# Identificación de valores atípicos

# Boxplot de la variable Age para identificar valores atípicos
boxplot(DataBank$Age, 
        xlab = "", ylab = "Age",
        main = "Boxplot de Age")

# Boxplot de la variable Experience para identificar valores atípicos
boxplot(DataBank$Experience , 
        xlab = "", ylab = "Experience ",
        main = "Boxplot de Experience ")

# Inconsistencia de los datos: Existen años de experiencia negativos

# Boxplot de la variable Income para identificar valores atípicos
boxplot(DataBank$Income, 
        xlab = "", ylab = "Income",
        main = "Boxplot de Income") # presenta outliyers


# Boxplot de la variable Family para identificar valores atípicos
boxplot(DataBank$Family, 
        xlab = "", ylab = "Family",
        main = "Boxplot de Family")

# Boxplot de la variable CCAvg para identificar valores atípicos
boxplot(DataBank$CCAvg, 
        xlab = "", ylab = "CCAvg",
        main = "Boxplot de CCAvg") # presenta outlyers


# Boxplot de la variable Mortgage para identificar valores atípicos
boxplot(DataBank$Mortgage, 
        xlab = "", ylab = "Mortgage",
        main = "Boxplot de Mortgage") # presenta outlyers


# Histogramas para conocer la distribución

hist(DataBank$Age, main = "Histograma var Age")

hist(DataBank$Experience, main = "Histograma var Experience")

hist(DataBank$Income, main = "Histograma var Income")

hist(DataBank$Family, main = "Histograma var Family")

hist(DataBank$CCAvg, main = "Histograma var CCAvg")

hist(DataBank$Mortgage, main = "Histograma var Mortgage")


library(naniar)

# Crear la visualización de valores faltantes
vis_miss(DataBank)

# no hay valores faltantes ni duplicados

duplicated(DataBank)


# Tratamiento de outlyers


#Tratamiento de datos outliers permite identificar y remover los datos outlier sin hacer cortes
# Función para extracción de datos outliers

tratamiento_outliers <- function(data, column, removeNA = TRUE) {
  x <- data[[column]]
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x < quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x > quantiles[2]] <- median(x, na.rm = removeNA)
  data[[column]] <- x
  data
}

# Indicamos que variable con datos outliers "pressure_height"
data_tratada1 <- tratamiento_outliers(DataBank, "Income")


#Gráfico con outliers y sin outliers
par(mfrow = c(1,2))

boxplot(DataBank$Income, main = "Income con outliers",
        col = 3)
boxplot(data_tratada1$Income, main = "Income sin outliers",col=2)

#---------


# Indicamos que variable con datos outliers "CCAvg"
data_tratada2 <- tratamiento_outliers(DataBank, "CCAvg")


#Gráfico con outliers y sin outliers
par(mfrow = c(1,2))

boxplot(DataBank$CCAvg, main = "CCAvg con outliers",
        col = 3)
boxplot(data_tratada2$CCAvg, main = "CCAvg sin outliers",col=2)

#---------


# Indicamos que variable con datos outliers "Mortgage"
data_tratada3 <- tratamiento_outliers(DataBank, "Mortgage")


#Gráfico con outliers y sin outliers
par(mfrow = c(1,2))

boxplot(DataBank$Mortgage, main = "Mortgage con outliers",
        col = 3)
boxplot(data_tratada3$Mortgage, main = "Mortgage sin outliers",col=2)




# Transformación de variables que no presentan una distribución normal

# Transformaciones
#- Raiz cuadrada
height_sqrt <- sqrt(DataBank$Income)

# -Scale
height_scale <- scale(DataBank$Income)

#- Log(10)
height_ln <- log(DataBank$Income)
#- Log(2)
height_log2 <- log(DataBank$Income, base=2)
#- Log(5)
height_log5 <- log(DataBank$Income, base=5)


# Grafico Comparación trasnformaciones
par(mfrow=c(3,2))
hist(DataBank$Income, main = "Sin trasnformar")
hist(height_sqrt, main = "transformación Sqrt")
hist(height_scale, main = "transformación Scale")
hist(height_ln, main = "transformación ln")
hist(height_log2, main = "transformación long2")
hist(height_log5, main = "transformación long10")


#-------------
  
# Transformaciones
#- Raiz cuadrada
height_sqrt <- sqrt(DataBank$CCAvg)

# -Scale
height_scale <- scale(DataBank$CCAvg)

#- Log(10)
height_ln <- log(DataBank$CCAvg)
#- Log(2)
height_log2 <- log(DataBank$CCAvg, base=2)
#- Log(5)
height_log5 <- log(DataBank$CCAvg, base=5)


# Grafico Comparación trasnformaciones
par(mfrow=c(3,2))
hist(DataBank$CCAvg, main = "Sin trasnformar")
hist(height_sqrt, main = "transformación Sqrt")
hist(height_scale, main = "transformación Scale")
hist(height_ln, main = "transformación ln")
hist(height_log2, main = "transformación long2")
hist(height_log5, main = "transformación long10")

#---------------

# Transformaciones
#- Raiz cuadrada
height_sqrt <- sqrt(DataBank$Mortgage)

# -Scale
height_scale <- scale(DataBank$Mortgage)

#- Log(10)
height_ln <- log(DataBank$Mortgage)
#- Log(2)
height_log2 <- log(DataBank$Mortgage, base=2)
#- Log(5)
height_log5 <- log(DataBank$Mortgage, base=5)


# Grafico Comparación trasnformaciones
par(mfrow=c(3,2))
hist(DataBank$Mortgage, main = "Sin trasnformar")
hist(height_sqrt, main = "transformación Sqrt")
hist(height_scale, main = "transformación Scale")
hist(height_ln, main = "transformación ln")
hist(height_log2, main = "transformación long2")
hist(height_log5, main = "transformación long10")


summary(DataBank$CCAvg)

# Discretización de variable CCAvg 

library(dplyr) 

variables_discretizadas <- data_tratada2 %>%
  select(CCAvg) %>%
  mutate(
    CCAvg_discretizada = cut(CCAvg, breaks = c(-Inf, 1, 5, 10, Inf), labels = c("Muy Bajo", "Bajo", "Medio", "Alto")))

data_discretizada <- data_tratada2 #creamos nuevo objeto una copia de la data

data_discretizada

data_discretizada$CCAvg <- variables_discretizadas$CCAvg_discretizada

#Validando la discretización (¿Hizo la conversión a discreta?)
levels_values <- levels(data_discretizada$CCAvg)
print(levels_values)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Seleccionamos algunos nombrs de las columnas 
numeric_cols <- c("Age", "Experience", "Income", "Family","CCAvg", "Mortgage")

data_Normalizada <- data_tratada1
# Normalizar las columnas numéricas
data_Normalizada[numeric_cols] <- lapply(data_tratada1[numeric_cols], normalize)
# Verificar los primeros registros del conjunto de datos transformado
head(data_Normalizada)



