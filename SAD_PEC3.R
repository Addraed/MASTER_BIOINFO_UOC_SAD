### INSTALACIÓN DE PAQUETES REQUERIDA: 'corrplot', 'GGally', 'PerformanceAnalytics'


# Estudiar la respuesta inmunologica innata ex vivo en dos poblaciones de macr?fagos (bazo y peritoneo) frente a diferentes antigenos del tratamiento. Las muestras fueron tomadas del modelo murino que recibieron el tratamiento alternativo (A) y el de referencia (B). Para estudiar el efecto del tumor en la respuesta innata, tambi?n se le suministr? el tratamiento A y B en animales sin c?ncer. 
# - ?A que antigeno responde mas cada grupo?
# - ?Los grupos responden a los mismos antigenos que se encuentran en el peritoneo y en el bazo? 
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

# Cuantos valores N/A se encuentran en la Base de Datos? 
cat("Existen un total de",table(!is.na(dataMurine))["FALSE"],"valores N/A en la base de datos") # Contar valores N/A

# A partir de un nuevo frame con solamente el grupo A (Tratamiento A con tumor) y de la region Peritoneo, Que media de reestimulacion es superior?
dataMurineA <- subset(dataMurine,(dataMurine$Grupo =="A" & dataMurine$Region == "Peritoneo"))
dataMurineA <- na.omit(dataMurineA) #Eliminar valores N/A
aggregate(dataMurineA$Reestimulacion, list(dataMurineA$Antigeno), FUN=mean)

# Boxplot con los valores de reestimulacion por grupo, agrupado por tipo de antigeno

library(ggplot2)

dataMurine <- na.omit(dataMurine) #Eliminar valores N/A
dataMurine$Antigeno <- as.factor(dataMurine$Antigeno)

plotMurine <- ggplot(dataMurine, aes(x=Grupo, y=Reestimulacion,fill = Antigeno)) + 
  geom_boxplot()+stat_summary(fun = median)
plotMurine + facet_wrap( ~ Grupo, scales="free")

# Crear una funcion que dado un grupo, una región, un antigeno, escriba en un csv junto con la informacion de entrada, la media, mediana, desviacion estandard y los valores maximos minimos.
functionSummary <- function(NumGrupo,NumRegion,NumAntigeno){
  resultPath <- c(getwd(),"/","Output_PEC3.csv") ## Leer ruta para cargar base de datos
  resultFile <- paste0(resultPath,collapse = "")
  resultFile
  if (file.exists(resultFile) == FALSE){
    file.create(resultFile)
    values0 <- c("Grupo","Region","Antigeno","Media","Mediana","St.Dev","Max","Min")
    
    write.table(as.list(values0), file = resultFile ,col.names = FALSE, append=TRUE,sep = ",",row.names = FALSE)
  }
  
  Num1 <- c(NumGrupo, NumRegion, NumAntigeno)
  newFrame <- subset(dataMurine,(dataMurine$Grupo == Num1[1] & dataMurine$Region == Num1[2] & dataMurine$Antigeno == Num1[3]))
  
  values1 <- c(NumGrupo,NumRegion,NumAntigeno,round(mean(newFrame$Reestimulacion),3),round(median(newFrame$Reestimulacion),3),round(sd(newFrame$Reestimulacion),3),round(max(newFrame$Reestimulacion),3),round(min(newFrame$Reestimulacion),3))
  write.table(as.list(values1), file = resultFile ,col.names = FALSE, append=TRUE,sep = ",",row.names = FALSE)
  values1
  
}

functionSummary("A","Peritoneo",1)

## Ejercicio 6
#Realizar, a partir de los conceptos trabajados en el LAB4 y la PEC2, un estudio probabilistico (a eleccion propia) de al menos 3 de las variables, que ayude a esclarecer cuestiones de relevancia que se plantean en los ambitos de accion estudiados.

# Calcular las diferentes probabilidades de obtener reestimulacion (Reestimulacion > 100) de cada grupo para el antigeno 1

# Nuevo data frame para almacenar los resultados
probData <- data.frame()

groupVector <- c("A","B","C","D","I","J") # Grupos posibles
regionVector <- c("Peritoneo","Bazo") # Regiones posibles

for (i in regionVector){
  for (j in groupVector){
    for (k in 1:9){
      Num1 <- c(j,i,k)
      
      normFrame <- subset(dataMurine,(dataMurine$Grupo == Num1[1] & dataMurine$Region == Num1[2] & dataMurine$Antigeno == Num1[3]))
      prob <- 1-pnorm(100,mean = mean(normFrame$Reestimulacion),sd = sd(normFrame$Reestimulacion),log = FALSE) # Calcular probabilidad de que exista reestimulacion
      print(prob)
      Num1 <- c(j,i,k,prob)
      print(Num1)
      probData <- rbind(probData,Num1) # Anadir resultado en un base frame junto con los datos de cada muestra
      
    }
  }
}
colnames(probData) <- c("Grupo","Region","Antigeno","Probabilidad") # Cambiar nombre de las columnas del Base Frame

# - ¿A que antigeno responde mas cada grupo?
probMax <- filter(probData, Grupo == "A" , Region == "Peritoneo")
probMax <- data.frame()

groupVector <- c("A","B","C","D","I","J") # Grupos posibles
regionVector <- c("Peritoneo","Bazo") # Regiones posibles

for (i in regionVector){
  for (j in groupVector){
      Num1 <- c(j,i)
      print(Num1)
      probabilidadMax <-  subset(probData, (probData$Grupo == j & probData$Region == i))
      colnames(probabilidadMax) <- c("Grupo","Región","Antigeno","Probabilidad_Max") # Cambiar nombre de las columnas del Base Frame
      print(probabilidadMax)
      probMax <-  rbind(probMax,subset(probabilidadMax, probabilidadMax$Probabilidad == max(probabilidadMax$Probabilidad)))
    
  }
}
colnames(probMax) <- c("Grupo","Region","Antigeno","Probabilidad_Max") # Cambiar nombre de las columnas del Base Frame

cat("La siguiente tabla muestra que antigeno tiene mas reestimulacion para cada grupo y region:")
probMax

# Los grupos responden a los mismos antigenos que se encuentran en el peritoneo y en el bazo? 

#Se verifica si existe corelacion entre los mismos antigenos que se encuentran en el peritoneo y en el bazo

# Para ello se cargan los mismos datos pero agrupados en columnas

folderPathCorrelacion <- c(getwd(),"/","DataBase_PEC3_Ejercicio6.3.csv") ## Leer ruta para cargar base de datos
csvFileCorrelacion <- paste0(folderPathCorrelacion,collapse = "")
dataMurineCorrelacion <- read.csv(csvFileCorrelacion,header = TRUE, sep = ",") ## Introducir datos
dataMurineCorrelacion <- na.omit(dataMurineCorrelacion) #Eliminar valores N/A

dataMurineCorrelacion$Grupo <- NULL # Eliminar para poder hacer la matrix de correlacion
dataMurineCorrelacion$Raton <- NULL # Eliminar para poder hacer la matrix de correlacion

#Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

corrplot(round(cor(dataMurineCorrelacion),1),method="number",type = "upper") # Gráfico con matriz de correlacion

cat("Las mayores correlaciones se encuentran en la misma region, por ejemplo entre el antigeno 4 y 5 del peritoneo")

# - La presencia de tumor en los animales modifica la respuesta a los distintos antigenos?

# Se comparan las probabilidades de tener reestimulacion entre los grupos A y B, que pertenecen al mismo tratamiento, y los grupos C y D

# Nuevo data frame para almacenar los resultados
probData <- data.frame()

regionVector <- c("Peritoneo","Bazo") # Regiones posibles

for (i in regionVector){
    for (k in 1:9){
     
      normFrame <- subset(dataMurine,(dataMurine$Grupo == "A" & dataMurine$Region == Num1[2] & dataMurine$Antigeno == Num1[3]))
      prob1 <- 1-pnorm(100,mean = mean(normFrame$Reestimulacion),sd = sd(normFrame$Reestimulacion),log = FALSE) # Calcular probabilidad de que exista reestimulacion
      normFrame <- subset(dataMurine,(dataMurine$Grupo == "B" & dataMurine$Region == Num1[2] & dataMurine$Antigeno == Num1[3]))
      prob2 <- 1-pnorm(100,mean = mean(normFrame$Reestimulacion),sd = sd(normFrame$Reestimulacion),log = FALSE) # Calcular probabilidad de que exista reestimulacion
      
      
      Num1 <- c("Tratamiento_1",i,k,prob1,prob2)
      print(Num1)
      probData <- rbind(probData,Num1) # Anadir resultado en un base frame junto con los datos de cada muestra
  }
}

for (i in regionVector){
    for (k in 1:9){
      normFrame <- subset(dataMurine,(dataMurine$Grupo == "C" & dataMurine$Region == Num1[2] & dataMurine$Antigeno == Num1[3]))
      prob3 <- 1-pnorm(100,mean = mean(normFrame$Reestimulacion),sd = sd(normFrame$Reestimulacion),log = FALSE) # Calcular probabilidad de que exista reestimulacion
      normFrame <- subset(dataMurine,(dataMurine$Grupo == "D" & dataMurine$Region == Num1[2] & dataMurine$Antigeno == Num1[3]))
      prob4 <- 1-pnorm(100,mean = mean(normFrame$Reestimulacion),sd = sd(normFrame$Reestimulacion),log = FALSE) # Calcular probabilidad de que exista reestimulacion
      Num1 <- c("Tratamiento_2",i,k,prob3,prob4)
      print(Num1)
      probData <- rbind(probData,Num1) # Anadir resultado en un base frame junto con los datos de cada muestra
      
  }
}
difference <- c()
for (i in 1:nrow(probData)){
  difference[i] <- as.numeric(probData[i,4])-as.numeric(probData[i,5]) # Calcular la diferencia de cada Antigeno/Region
}
probData$Diferencia <- difference  # Añadir las diferencias entre Reestimulacion del grupo con tumor y sin tumor al Frame

colnames(probData) <- c("Grupo","Region","Antigeno","Probabilidad_Con_Tumor","Probabilidad_Sin_Tumor","Diferencia") # Cambiar nombre de las columnas del Base Frame

cat("Las diferencias entre reestimulaciones de cada grupo se muestran en la siguiente tabla, para aquellos grupos donde la diferencia sea positiva, existe mayor reestimulacion en los grupos con tumor")
probData
