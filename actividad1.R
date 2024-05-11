# - ¿De qué tipo de variables se compone mi base de datos?
# Realice un análisis exploratorio de las variables numéricas 
# (media, varianza, diagrama de cajas, etc)
# ¿Qué conclusiones se pueden extraer de la muestra de datos?   

library(readxl)
data_set <- read_excel("C:/Users/David/Desktop/folders/UNIR/Maestría/clases/Analisis de datos masivos/actividades/actividad 1/data_set.xlsx")
View(data_set)
summary(data_set)

str(data_set)
copia <- data_set
str(copia)
# Seleccionar las variables de interés
variables_interes <- copia[, c("average_price", "total_volume", "4046", "4225", "4770", "total_bags", "small_bags", "large_bags", "xlarge_bags", "year")]
variables_interes
# Histogramas

variables_interes <- copia[, c("average_price", "total_volume", "4046", "4225", "4770","total_bags", "small_bags", "large_bags", "xlarge_bags", "year")]

par(mfrow=c(3, 4)) 

par(mar = c(2, 2, 1, 1))  # Reducir los márgenes # si las pantallas no son grandes, se ajustan las márgenes


#histogramas sin ajuste de los ejes

for (col in names(variables_interes)) {
  hist(variables_interes[[col]], 
       main = paste("Histograma de", col),
       xlab = col,
       col = "pink", 
       border = "white")
}



#Acá se ajustan los ejes de modo que se puedan ver mejor los datos
#para el caso de los codigos del producto, total_bus, etc.
for (col in names(variables_interes)) {
  limite_superior <- max(variables_interes[col])
  hist(variables_interes[[col]], 
       main = paste("", col),
       xlim = c(0,limite_superior),
       ylim = c(0, 100),
       xlab = col,
       col = "skyblue", 
       border = "white")
}
#- Extraiga de la base de datos el precio de venta (la variable) 
#de los aguacates orgánicos vendidos en Albany y de Boston.
venta_albany <- copia[copia$geography=='Albany' & copia$type == 'organic',
                      "total_volume"]

venta_boston <- copia[copia$geography=='Boston' & copia$type == 'organic',"total_volume"]
summary(venta_albany)
summary(venta_boston)

#- Como paso previo al modelado, calcule la covarianza y 
# la matriz de correlación del precio de los aguacates orgánicos, 
# convencionales y su volumen de ventas. ¿Qué conclusiones se pueden extraer?


organicos_price <- copia[copia$type=='organic',"average_price"]
organicos_volume <- copia[copia$type=='organic',"total_volume"]
conventional_price <- copia[copia$type=='conventional', "average_price"]
conventional_volume<-copia[copia$type=='conventional','total_volume']
print(conventional_price)
print(copia)
#calculo covarianzas
cov_organicos <- cov(organicos_price,organicos_volume)
cov_conventional <- cov(conventional_price,conventional_volume)
# Imprimir mensaje descriptivo junto con el dataframe cov_organicos
print(cov_organicos)

# Imprimir mensaje descriptivo junto con el dataframe cov_conventional
print(cov_conventional)



#calculo de correlaciones
correlacion_organico <- cor(organicos_price,organicos_volume)
correlacion_conventional <- cor(conventional_price,conventional_volume)
print(correlacion_organico)
print(correlacion_conventional)


#Determine la posible relación existente entre dichos precios y su volumen de ventas. 
#Si tomáramos logaritmos, ¿Cómo sería dicha relación?
lm(organicos_volume$total_volume~organicos_price$average_price)

# La relacion entre los precios de los aguacates orgánicos y las ventas de estos
# se daría por : 106136-26299*organicos_price 
#Segun esto, por cada que se sube el precio del aguacate orgánico , se bajan
# las ventas en una proporción de 26299.
#Ahora para verlo en porcentajes lo haremos con logaritmos

lm(log(organicos_volume$total_volume) ~ log(organicos_price$average_price))
# El volumen total de venta viene dado
# Total_volumen= 10.1407 - 0.7665*(average_price)
# Si aumentamos 10% el precio del aguacate orgánico , se bajan 7.6% las 
# ventas

analisis_organicos<-lm(log(organicos_volume$total_volume) ~ log(organicos_price$average_price))
summary(analisis_organicos)


lm(conventional_volume$total_volume ~ conventional_price$average_price)

# La relacion entre los precios de los aguacates convencionales y las ventas de estos
# se daría por : -2000275*conventional_price + 4162762
#Segun esto, por cada que se sube el precio del aguacate orgánico , se bajan
# las ventas en una proporción de 2000275.
#Ahora para verlo en porcentajes lo haremos con logaritmos



lm(log(conventional_volume$total_volume) ~ log(conventional_price$average_price))
#El volumen total de venta viene dado, total_volume=13.42-1.32(average_price)
# Si aumentamos 10% el precio del aguacate convencional , disminute un 13,2% las 
# ventas

analisis_convencional<-lm(log(conventional_volume$total_volume) ~ log(conventional_price$average_price))
summary(analisis_convencional)
############################################################################
venta_boston <- copia[copia$geography=='Boston' & copia$type == 'organic',"total_volume"]






########################################
library(zoo)
#Realice una predicción de precio de venta de los aguacates orgánicos vendidos en Albany a  3 meses. 
precio_venta_albany<-copia[copia$geography =="Albany" & copia$type =="organic", c('date', 'average_price')]
# utilizamos la función Zoo, la cual sirve también para series temporales, en lugar de la función TS, dado que es más flexible en el orden temporal o frecuencia
# Para este caso, como las frecuencia de fecha del data_set es diaria irregular(no todos los días del mes tienen datos )  y la predicción nos la solicitan mensual, debiamos promediar las tarifas promedio ( average_price) para cada mes
# Creamos la serie tmeporal usando zoo con el vector de datos de tarifa promedio, asignandole un indice de tiempo de fechas que corresponde a cada punto en el tiempo de los datos 
precio_ventas_ts<-zoo(precio_venta_albany$average_price,precio_venta_albany$date)
# como nos piden la proyección mensual, llevamos los datos de frecuencia diaria irregular a mensual, empleando la función aggregate, la cual agrupa los datos por mes, en este caso promediandolo, ya que estamos hablando de tarifa promedio
precio_ventas_albany_st <- aggregate(precio_ventas_ts, as.yearmon, mean)
precio_ventas_albany_st

#graficamos la serie de tiempo
plot(precio_ventas_albany_st)
##################################



#Descompones la estacionalidad de la seria tempral, en sus componentes: Ciclo, tendencia, estacionalidad y aletaoriedad

# Convertir la serie temporal zoo a ts para que sea compatible con la función decompose
precio_ventas_albany_st_ts <- as.ts(precio_ventas_albany_st)

decompose(precio_ventas_albany_st_ts)
#representemoslo 
plot(decompose(precio_ventas_albany_st_ts))

#almacenamos la descomposición de la serie

descomp_pre_albany<-decompose(precio_ventas_albany_st_ts)
descomp_pre_albany$trend
install.packages("forecast")
library(forecast)

# 1º paso: creamos el modelo de predicción con el comando auto.arima (que pertenece a forecast)


modeloprecio_venta_albany<-auto.arima(precio_ventas_albany_st_ts)
#2º paso:  realizamos la predicción con el modelo del 1º paso
# hacemos una predicción para los próximos 3 meses
prediccion_precio_venta_albany <- forecast(modeloprecio_venta_albany,3)
# ahora lo graficamos

plot(prediccion_precio_venta_albany)
valor_prediccion<-prediccion_precio_venta_albany$mean
View(valor_prediccion)
print(valor_prediccion)
install.packages("openxlsx")
library(openxlsx)

# guardamos los resultados de la predicción de tres meses
valor_prediccion_albany <- as.data.frame(valor_prediccion)
write.xlsx(valor_prediccion_albany, "C:/Users/Lenovo/OneDrive - UNIR/Datos/Mestría/Análisis de datos para el Negocio/Actividad 1/Requisitos/Entrega Actividad 1/valor_prediccio_precio_albany.xlsx")