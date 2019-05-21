
## Realizamos la carga de los datos
alumnos <- read.csv("Admission_Predict_Ver1.1.csv", header=TRUE) 

## Comprobamos los datos cargados y los tipos de variables asignados
str(alumnos)

##Eliminamos la primera columna
alumnos_estudio <- alumnos[,-1]

sapply(alumnos_estudio, function(x) class(x))


 
