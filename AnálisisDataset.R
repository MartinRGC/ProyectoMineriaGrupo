
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




















