# Estudiar la respuesta inmunologica innata ex vivo en dos poblaciones de macr?fagos (bazo y peritoneo) frente a diferentes ant?genos del tratamiento. Las muestras fueron tomadas del modelo murino que recibieron el tratamiento alternativo (A) y el de referencia (B). Para estudiar el efecto del tumor en la respuesta innata, tambi?n se le suministr? el tratamiento A y B en animales sin c?ncer. 
# - ?A que antigeno responden mas los macrofagos?
# - ?Responden los macrofagos a los mismos antigenos que se encuentran en el peritoneo y en el bazo? 
# - ?La presencia de tumor en los animales modifica la respuesta a los distintos antigenos?
#   
#   A: Tratamiento A con tumor
#   B: Tratamiento A sin tumor
#   C: Tratamiento B con tumor
#   D: Tratamiento B sin tumor
#   I: Animal control con tumor
#   J: Animal control sin tumor

## Ejercicio 1

folderPath <- c(getwd(),"/","DataBase_PEC3.csv") ## Leer ruta para cargar base de datos
csvFile <- paste0(folderPath,collapse = "")
dataMurine <- read.csv(csvFile,header = TRUE, sep = ",") ## Introducir datos

## Ejercicio 2

head(dataMurine) # Mostrar datos

summary(dataMurine) # Significado de cada variable

str(dataMurine) # Tipo de valores de cada variable

## Dar nombre a los grupos para facilitar la comprension
groupNames <- factor(dataMurine$Grupo, levels = c("A","B","C","D","I","J"),labels = c("Tratamiento A con tumor","Tratamiento A sin tumor","Tratamiento B con tumor","Tratamiento B sin tumor","Animal control con tumor","Animal control sin tumor") )  
unique(groupNames) ## Comprobar que los labels estan introducidos correctamente

## Ejercicio 3

# ?Cuantos valores N/A se encuentran en la Base de Datos? 
# Creo que se piden cuestiones como estas
cat("Existen un total de",table(!is.na(dataMurine))["FALSE"],"valores N/A en la base de datos") # Contar valores N/A

# Calcular la media de la reestimulacion de cada grupo
# Boxplot con los valores de reestimulacion por grupos



