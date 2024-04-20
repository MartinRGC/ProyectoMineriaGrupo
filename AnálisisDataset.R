
#install.packages("readxl")
library(readxl)

DataBank <- read_excel("C:/Users/51972/Documents/GitHub/ProyectoMineriaGrupo/Bank_Personal_Loan_Modelling.xlsx", sheet = "Data")

head(DataBank)

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











