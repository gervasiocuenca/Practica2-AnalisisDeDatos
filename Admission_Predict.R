
## Realizamos la carga de los datos
alumnos <- read.csv("Admission_Predict_Ver1.1.csv", header=TRUE) 

## Comprobamos los datos cargados y los tipos de variables asignados
str(alumnos)

##Eliminamos la primera columna
alumnos_estudio <- alumnos[,-1]

sapply(alumnos_estudio, function(x) class(x))


 
summary(alumnos_estudio)
#sapply(alumnos_estudio, function(x) sum(is.na(x)))
colSums(is.na(alumnos_estudio))


shapiro.test(alumnos_estudio$GRE.Score)

alpha <- 0.05
col.names = colnames(alumnos_estudio)
var.no.normales <- c()
for (i in 1:ncol(alumnos_estudio)) {
  p_val = shapiro.test(alumnos_estudio[,i])$p.value
  if (p_val <= alpha) {
    var.no.normales <- c(var.no.normales, col.names[i]) 
  }
}
cat("Variables que no siguen una distribución normal: ")
cat(var.no.normales, sep=", ") 


alumnos.investigadores <- alumnos_estudio[alumnos_estudio$Research == 1,]

fligner.test(Chance.of.Admit ~ Research, data = alumnos_estudio)


participacion <- matrix(
  c("Búsqueda previa", "Gervasio Cuenca, Sabela de la Torre",
    "Redacción de las respuestas", "Gervasio Cuenca, Sabela de la Torre",
    "Desarrollo código", "Gervasio Cuenca, Sabela de la Torre"),
                        ncol = 2,
                        byrow = TRUE
)
colnames(participacion) <- c("Contribuciones", "Firma")
rownames(participacion) <- c("Búsqueda previa", 
                             "Redacción de las respuestas", 
                             "Desarrollo código")
participacion
