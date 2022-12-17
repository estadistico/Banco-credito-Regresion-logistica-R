#Datos_credito <- read.csv("C:/Users/hhida/Desktop/Minería de Datos/Base de Datos/credito.csv")
Datos_credito <- read.csv("C:/Users/Lenovo/OneDrive/R_proyectos/hector jose chile/credito.csv")

head(Datos_credito)
#vamos a ver la estructura de los datos
str(Datos_credito)
#Categorizar las variables de cumplimiento 
#1=no 
#2=si
Datos_credito$incumplimiento<-as.factor(Datos_credito$incumplimiento) #rescribimos en la misma variable, guardamos
str(Datos_credito) #vemos si se actualizó, efectivamente se actualzo, factor.
#vamos a renombrar los niveles 1 y 2
levels(Datos_credito$incumplimiento)<-c("no","si")#se pasó a no y si.
#balance_control: niveles "< 0 DM","1 - 200 DM", "> 200 DM",  "deconocido"
#Vamos a categorizar variable balance control, la convertimos en factor
Datos_credito$balance_control<-as.factor(Datos_credito$balance_control)
str(Datos_credito)
levels(Datos_credito$balance_control)<-c("< 0 DM","1 - 200 DM", "> 200 DM",  "deconocido")
#Vamos a realizar exploratorio de los datos para conseguir algo
#calcular el resumen estadistico con summary
summary(Datos_credito)
#las 3 variables de interes: incumplimiento, monto y duración del prestamo
#variable dependiente: incumplimiento armar un modelo si un nuevo cliente va a cumplir o no con el prestamo
#Supervizado: arboles, redes neuronales, regresión logistico

#Regresión logistica: la variable dependiente Y debe ser categorica dicotomica.
#Árboles: la variable dependiente puede ser numerica o categorica
  #a) Arbol de regresión: si la variable dependiente es numerica
  #b) Arbol de clasificación si la variable es categorica


#Redes neuronales: la variable dependiente puede ser numerica o categorica
#Regresión lineal simple o multiple: variable dependiente es numerica

#Comenzamos con la regresión  logistica, ya que es categorica.
#Dividimos los datos entrentrenamiento y test.

#necesitamos saber cuantas observaciones tenemos
Total<- nrow(Datos_credito)  #cuantas filas tengo
n_entrenamiento<-Total*0.8  #calculamos el 80% de los datos
n_entrenamiento  # de las 1000 obsrvaciones vamos a tomar 800 de manera aleatoria
id_muestras<-sample(1:1000,n_entrenamiento)  #indicamos rango de donde voy a tomar las muestras, y el indicoi cuantas observaciones voy a tomar de las 1000, las cuales tan
#guardadas en la variable n_entrenamiento (nuemro de filas que voya  seleccionar)
# se guarda en id_muestras
#ahora se va a filtrar los datos
Total
#Los datos se encuentran en mi data frame Datos_credito
Datos_entrenamiento<-Datos_credito[id_muestras,]#filtrar por filas y los guardamos en una variable datos_entrenaimeinto
Datos_entrenamiento
Datos_test<-Datos_credito[-id_muestras,] #calculamos datos test
Datos_test
#Ahora entrenamiento del modelo con la función glm
modelo_logistico_1<-glm(incumplimiento~.,data = Datos_entrenamiento,family = binomial) #.,para abarcar el resto de las variables
#las cuales se obtienen de datos_entrenamiento
#se guarda esto en una variable llamada modelo_logistico_1
#Voy a hacer un summary a modelo_logistico_1 para vewr el resumen de todos los indicadores que tengo
summary(modelo_logistico_1)
#Variables independientes del segundo modelo, las variables del primer modelo son todas, balance_control+duracion_prestamo+monto+saldo_ahorro+tasa_instalacion+estado_personal+edad+alojamiento
modelo_logistico_2<-glm(incumplimiento~balance_control+duracion_prestamo+monto+saldo_ahorro+tasa_instalacion+estado_personal+edad+alojamiento,data = Datos_entrenamiento,family = binomial)
#hacemos la predicción 
nrow(Datos_test)
incumplimiento_prediccion<-predict(modelo_logistico_2,data=Datos_test,type = "response") #ya que el primer modelo tiene muchas, y se seleccionan las mas significativas.

length(incumplimiento_prediccion)#las predicciones del modelo se hacen con datos test, responde es porque es variable dicotomica
#guardamos la prediccion en una variable. Si es mayor que 0,5 es "no", si es menor o iguaal que 0,5 es "si"
ifelse(incumplimiento_prediccion>0.5,"no","si")
