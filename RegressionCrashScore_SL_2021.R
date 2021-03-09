#Clase de agrupamiento de categorías dentro de factores.
#Modelo de regresión con predictores categóricos (ANOVA)
#Este ejercicio ilustra el agrupamiento de categorías dentro de un factor
#auxiliándose de las funciones anova() y contrasts().

#Fuente de la base de datos:
#Exam PA (Predictive Analytics) June 13, 2019 Project
#Report Template de la Society of Actuaries (SOA):
#https://www.soa.org/globalassets/assets/files/edu/2019/exam-pa-06-14-model-solution.pdf

#Obsérevese que el objetivo de este ejercicio es diferente al 
#del ejercicio de la fuente original.

getwd()
ls()
rm(list=ls())
options(digits=2, width = 140)

crash = read.csv("C:\\Users\\Yanely\\Documents\\2021-1\\Aprendizaje Estadístico Automatizado - Seminario de Estadística\\June_13_data.csv")
str(crash)

library(MASS)

# Variable respuesta: Y=Crash_Score (continua)
par(mfrow=c(1,1))
hist(crash$Crash_Score, breaks = 70,probability = T)

#Trabajaremos con una selección de cuatro variables explicativas,
# tipo factor. 
crashB=crash[,c(1,4,5,8,13)]
str(crashB)
for(i in 2:5) {crashB[,i]=as.factor(crashB[,i])}
str(crashB)

# Crash_Score (Y)

# Time_of_Day: 
# 1: 0:00 - 4:00
# 2: 4:00 - 8:00
# 3: 8:00 - 12:00
# 4: 12:00 - 16:00 
# 5: 16:00 - 20:00
# 6: 20:00 - 0:00

#Rd_Feature:  "NONE", "INTERSECTION", "RAMP", "DRIVEWAY", "OTHER".

#Rd_Configuration: 
# "ONE-WAY"
# "TWO-WAY-NO-MEDIAN" 
# "TWO-WAY-PROTECTED-MEDIAN"
# "TWO-WAY-UNPROTECTED-MEDIAN"
# "UNKNOWN".

#Traffic_Control: "NONE", "OTHER", "SIGNAL", "STOP SIGNAL", "YIELD".

#--------Distribución de las observaciones por grupos---------

#Veamos la distribución de  las observaciones por categoría de cada factor.
aux=list() #objeto tipo lista
for(i in 2:5) {aux[[i-1]]=table(crashB[,i])}
print(aux[[1]])
print(lapply(1:4, function(i){aux[[i]]}))

# Número total de grupos, tablas cruzadas o crosstabs:

table(crashB[,3])   #Table 1x5 cells
#Crosstab 6x5x5x5=750 cells, se imprimen 25 tablas bidimensionales de 6x5
(Frecs=table(crashB[,2:5])) 
#(xt=xtabs(~crashB[,2]+crashB[,3]+crashB[,4]+crashB[,5])) #ídem

#Veamos las medias, varianzas de la variable respuesta dentro de cada celda.
mean(crashB[,1]) #6.6 Media global de la variable respuesta Y
tapply(crashB[,1], crashB[,2], mean) #Medias Y  por grupo (var 2)
Sum=tapply(crashB[,1], crashB[,2:5],sum) #Suma de Y por celda en la tabla de 6x5x5x5=750 celdas 
Means=tapply(crashB[,1], crashB[,2:5],mean) #Media de Y por celda en la tabla de 6x5x5x5=750 celdas 
#Means=tapply(crashB[,1], list(crashB[,2],crashB[,3],crashB[,4],crashB[,5]),mean) ídem
Vars=tapply(crashB[,1], crashB[,2:5],var)
str(Means)

#Por ejemplo, cuando cada factor toma la primer categoría:
Frecs[1,1,1,1]; Sum[1,1,1,1]; Means[1,1,1,1]; Vars[1,1,1,1]
Frecs[1,2,2,4]; Sum[1,2,2,4]; Means[1,2,2,4]; Vars[1,2,2,4] #otras categorías
Frecs[6,5,5,5]; Sum[6,5,5,5]; Means[6,5,5,5]; Vars[6,5,5,5] #última categoría

SD=sqrt(Vars)
dev.off()

#Veamos la distribución de la media y varianza por celda.
par(mfrow=c(2,2)) 
plot(Means,Vars, xlab="Cell means", ylab="Cell variances", pch=19, cex=.2)
plot(Means,SD, xlab="Cell means", ylab="Cell Std Devn.", pch=19, cex=.2)
plot(Means[Means <14],Vars[Means <14], xlab="Cell means <14", ylab="Cell variances", pch=19, cex=.2)
plot(Means[Means <14],SD[Means <14], xlab="Cell means <14", ylab="Cell Std Devn.", pch=19, cex=.2)

(auxMeans=as.vector(Means)) #6x5x5x5x5=750 celdas
(sort(auxMeans)) #421/750= 0.56 proporción de celdas no vacías.
sum(is.na(Means)) #329 celdas vacías
sum(is.na(Vars))  #422 celdas vacías.

#-------Categorías o niveles de referencia--------

#Inspeccionemos las categorías de referencia de cada factor.
#Por default será la primer categoría en orden alfabético.
contrasts(crashB[,2]) # la uno: "1"
contrasts(crashB[,3]) # DRIVEWAY
contrasts(crashB[,4]) # ONE-WAY:
contrasts(crashB[,5]) # NONE

#------Selección del modelo-------

#El histograma de Y muestra una distribución asimétrica.
#La gráfica de dispersión, medias vs varianza de Y, 
#muestra una varianza no constante respecto a la maedia.
# Sugieren una transformación en Y, probemos boxcox() o Log.

par(mfrow=c(1,1))
bc=boxcox(Crash_Score~., data=crashB)
(lambda=bc$x[which.max(bc$y)]) # lambda=.26

#Exploremos la transformación sugerida por boxcox() y la log(y).
m=list()
m[[1]]= lm(Crash_Score~., data=crashB)
m[[2]]= lm((Crash_Score^lambda-1)/lambda ~., data=crashB)
m[[3]]= lm(Crash_Score^lambda ~., data=crashB)
m[[4]]= lm(log(Crash_Score+.1)~., data=crashB)
#for(i in 1:4) print(drop1(m[[i]], test="F"))
for(i in 1:4) print(summary((m[[i]]))$r.squared)
for(i in 1:4) print(AIC(m[[i]]))
for(i in 1:4) print(BIC(m[[i]]))
#Podríamos ver el error de entrenamiento de cada  modelo,
#pero Y no están en la misma escala.
for(i in 1:4) print(mean(residuals(m[[i]])^2))
#Para compararlos habremos de  calcularlos en la misma escala.

summary(crashB$Crash_Score)

#Veamos la gráfica q-q de de cada modelo:
dev.off()
par(mfrow=c(2,2))
plot(m[[1]], which=2, pch=19, cex=.25)
plot(m[[2]], which=2, pch=19, cex=.25)
plot(m[[3]], which=2, pch=19, cex=.25)
plot(m[[4]], which=2, pch=19, cex=.25)

#Seleciono el modelo m[[3]]
print(summary(m[[3]]), digits=2)

#------Agrupación de categorías dentro de un factor-----

#Veamos las etiquetas y contrastes de los factores

levels(crashB[,3])    #categorías o niveles
contrasts(crashB[,3]) #DRVEWAY es la categoría de referencia
table(crashB[,3])     #Distribución de las observaciones de acuerdo a un factor 

#Podemos modificar el nivel de referencia de forma temporal
#o de forma permanente. E.g.:
#Temporal:
contrasts(crashB[,3])
contr.treatment(levels(crashB[,3]), base=5)
contrasts(crashB[,3])

#De forma no reversible:
crashB[,3]=relevel(crashB[,3], ref=5) #cambio permanente
contrasts(crashB[,3])

#Recuperemos la variable original
#crashB=crash[,c(1,4,5,8,13)]
crashB[,3]=as.factor(crash[,5])
contrasts(crashB[,3])

#Inspeccionemos los coeficientes del modelo original
#en summary(m[[3]]). Ver tabla auxiliar.
#Agregaremos las categorías o niveles (levels) cuyos coeficientes 
#son estadísticamente iguales entre sí, o estadísticamente 
#iguales al coeficiente de la categoría de referencia (a cero). 


#Agregaremos algunas categorías o niveles de cada factor, uno por uno
#y compararemos la RSS de los modelos, el del reducido vs  el del original,
#y el del reducido vs el del que le antecede: 

table(crashB[,2])
levels(crashB[,2])[3:5]="Time_of_Day345" #agregamos tres categorías.
table(crashB[,2])

#Ajustamos el modelo
m[[5]]= lm(Crash_Score^lambda ~., data=crashB)
anova(m[[3]], m[[5]]) #No se rechaza la hipótesis
#H_o:los parámetros adicionales son cero.
#Nos quedamos con el modelo reducido,
#el que tiene las categorías agregadas,
#tiene menos variables dummy que el no reducido.

print(summary(m[[5]]), digits=2)
drop1(m[[3]], test="F")
drop1(m[[5]], test="F")

#Continuamos agrupando categorías del siguiente factor:
contrasts(crashB[,3])
table(crashB[,3])
levels(crashB[,3])[c(1,3,4)]="Rd_FeatDriveNoneOther"
levels(crashB[,3])
m[[6]]= lm(Crash_Score^lambda ~., data=crashB)
anova(m[[5]], m[[6]])
anova(m[[3]], m[[6]])

summary(m[[6]])

#Siguiente factor

table(crashB[,4])
contrasts(crashB[,4])
levels(crashB[,4])[c(1,2,4,5)]="Rd_Conf1245"
m[[7]]= lm(Crash_Score^lambda ~., data=crashB)
anova(m[[7]], m[[6]])
anova(m[[3]], m[[7]])
summary(m[[7]])

#último factor
table(crashB[,5])
contrasts(crashB[,5])
levels(crashB[,5])
levels(crashB[,5])[c(1,2,5)]="Trf_ContNoneOtherYield"
table(crashB[,5]) 
contrasts(crashB[,5]) #Observe que las categorías se renumerarón
levels(crashB[,5])[c(2,3)]="Trf_ContSigStopSig"
table(crashB[,5]) 
contrasts(crashB[,5]) #Observe que las categorías se renumerarón

m[[8]]= lm(Crash_Score^lambda ~., data=crashB)
anova(m[[7]], m[[8]]) #la reducción en RSS no es estadísticamente significativa
#nos quedamos con el modelo reducido.
anova(m[[3]], m[[8]])

#---Modelo final recategorizado-----

#El modelo m[[8]] corresponde al modelo m[[3]] 
#con categorías agrupadas en sus factores:

summary(m[[3]])$r.square;summary(m[[8]])$r.square
summary(m[[3]])$adj.r.square;summary(m[[8]])$adj.r.square
mean(residuals(m[[3]])^2); mean(residuals(m[[5]])^2)
#summary(m[[3]]);summary(m[[3]])
par(mfrow=c(1,2))
plot(m[[3]], which=2, pch=19, cex=.25)
plot(m[[8]], which=2, pch=19, cex=.25)

#El modelo m[[3]] y el m[[8]] tienen un poder predictivo similar,
# sólo difieren en el número de categorías en cada factor.

#El agrupamiento de categorías se realizó atendiendo únicamente
#aspectos metodológicos -la igualdad estadística de coeficientes.
#En la práctica también deben ser atendidos aspectos contextuales
# (subject matter).

#La estrategia seguida para el agrupamiento de las categorías
#desde luego que no es única.

#Nota técnica: Puede optar por trabajar con una copia de la base de datos,
#al agrupar categorías ya no es posible desagruparlas.

#Referencia auxiliar: "Statistics. An introduction using R", M.J. Crawley. Second ed. Wiley,2015.
#Chapter 11 Contrasts.

##Qué padre tu proyecto y todo eh
#------Fin-----------


