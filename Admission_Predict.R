
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


alumnos.universidades.calif.alta <- alumnos_estudio[alumnos_estudio$University.Rating >= 3,]
alumnos.universidades.calif.baja <- alumnos_estudio[alumnos_estudio$University.Rating < 3,]
alumnos.universidades.calif.baja

fligner.test(Chance.of.Admit ~ University.Rating >= 3, data = alumnos_estudio)

#t.test(alumnos.universidades.calif.baja, alumnos.universidades.calif.alta, alternative = "greater", conf.level = 0.05)
wilcox.test(alumnos.universidades.calif.baja$Chance.of.Admit, 
            alumnos.universidades.calif.alta$Chance.of.Admit, alternative = "greater", conf.level=0.05)
wilcox.test(alumnos.universidades.calif.baja$Chance.of.Admit, 
            alumnos.universidades.calif.alta$Chance.of.Admit, alternative = "less", conf.level=0.05)
wilcox.test(alumnos.universidades.calif.baja$Chance.of.Admit, 
            alumnos.universidades.calif.alta$Chance.of.Admit, alternative = "two.sided", conf.level=0.05)

mean(alumnos.universidades.calif.baja$Chance.of.Admit)
mean(alumnos.universidades.calif.alta$Chance.of.Admit)


# histograma y qq-plot
hist(alumnos_estudio$GRE.Score)
qqnorm(alumnos_estudio$GRE.Score)
qqline(alumnos_estudio$GRE.Score)


install.packages("corrplot")
library(corrplot)

# correlación gráfica
corr.res <- cor(alumnos_estudio)
corrplot(corr.res, method="circle")

install.packages("caret")
library(caret)

# Leave-one-out
train_control <- trainControl(method="LOOCV")
modelo1 <- train(Chance.of.Admit ~ CGPA + GRE.Score + TOEFL.Score, 
               data=alumnos_estudio, trControl=train_control, method="lm")
modelo2 <- train(Chance.of.Admit ~ CGPA + GRE.Score + University.Rating, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
modelo3 <- train(Chance.of.Admit ~ CGPA + GRE.Score + SOP, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
modelo4 <- train(Chance.of.Admit ~ CGPA + TOEFL.Score + University.Rating, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
modelo5 <- train(Chance.of.Admit ~ CGPA + TOEFL.Score + SOP, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
modelo6 <- train(Chance.of.Admit ~ CGPA + University.Rating + SOP, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
modelo7 <- train(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
modelo8 <- train(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
modelo9 <- train(Chance.of.Admit ~ TOEFL.Score + University.Rating + SOP, 
                 data=alumnos_estudio, trControl=train_control, method="lm")
print(modelo1)
summary(modelo1)

regresion <- matrix(c(1 , summary(modelo1)$r.squared,
                      2 , summary(modelo2)$r.squared,
                      3 , summary(modelo3)$r.squared,
                      4 , summary(modelo4)$r.squared,
                      5 , summary(modelo5)$r.squared,
                      6 , summary(modelo6)$r.squared,
                      7 , summary(modelo7)$r.squared,
                      8 , summary(modelo8)$r.squared,
                      9 , summary(modelo9)$r.squared),
                    ncol = 2, byrow = TRUE)
colnames(regresion) <- c("Modelo", "Bondad")
regresion

names(getModelInfo())

summary(modelo1)
summary(modelo1$pred)
predict(modelo1, train_control, type="response")
if (Modelo == 2) Prediccion<-predict(modelo2, train_control, type="response")
if (Modelo == 3) Prediccion<-predict(modelo3, alumnos_test, type="response")
if (Modelo == 4) Prediccion<-predict(modelo4, alumnos_test, type="response")
if (Modelo == 5) Prediccion<-predict(modelo5, alumnos_test, type="response")
if (Modelo == 6) Prediccion<-predict(modelo6, alumnos_test, type="response")
if (Modelo == 7) Prediccion<-predict(modelo7, alumnos_test, type="response")
if (Modelo == 8) Prediccion<-predict(modelo8, alumnos_test, type="response")
if (Modelo == 9) Prediccion<-predict(modelo9, alumnos_test, type="response")


summary(alumnos_m1)
Prediccion<-predict(alumnos_m2, alumnos_test, type="response")
Prediccion
summary(Prediccion)








# participación

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
