
#install.packages("readxl")
library(readxl)

DataBank <- read_excel("C:/Users/51972/Documents/GitHub/ProyectoMineriaGrupo/Bank_Personal_Loan_Modelling.xlsx", sheet = "Data")

head(DataBank)

summary(DataBank)