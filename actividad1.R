# - ¿De qué tipo de variables se compone mi base de datos?
# Realice un análisis exploratorio de las variables numéricas 
# (media, varianza, diagrama de cajas, etc)
# ¿Qué conclusiones se pueden extraer de la muestra de datos?	

summary(data_set)
str(data_set)
copia <- data_set
str(copia)
# Seleccionar las variables de interés
variables_interes <- copia[, c("average_price", "total_volume", "4046", "4225", "4770", "total_bags", "small_bags", "large_bags", "xlarge_bags", "year")]
variables_interes
# Calcular la matriz de correlación

variables_interes <- copia[, c("average_price", "total_volume", "4046", "4225", "4770","total_bags", "small_bags", "large_bags", "xlarge_bags", "year")]

par(mfrow=c(3, 4)) 

par(mar = c(2, 2, 1, 1))  # Reducir los márgenes # si las pantallas no son grandes, se ajustan las márgenes

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
venta_boston <- copia[copia$geography=='Boston' & copia$type == 'organic',
                      "total_volume"]
View(venta_albany)
View(venta_boston)

#- Como paso previo al modelado, calcule la covarianza y 
# la matriz de correlación del precio de los aguacates orgánicos, 
# convencionales y su volumen de ventas. ¿Qué conclusiones se pueden extraer?

View(copia)
organicos_price <- copia[copia$type=='organic',"average_price"]
organicos_volume <- copia[copia$type=='organic',"total_volume"]
conventional_price <- copia[copia$type=='conventional', "average_price"]
conventional_volume <- copia[copia$type=='conventional', "total_volume"]
View(conventional_price)
View(copia)
#calculo covarianzas
cov_organicos <- cov(organicos_price,organicos_volume)
cov_conventional <- cov(conventional_price,conventional_volume)
View(cov_organicos)
View(cov_conventional)

#calculo de correlaciones
correlacion_organico <- cor(organicos_price,organicos_volume)
correlacion_conventional <- cor(conventional_price,conventional_volume)
View(correlacion_organico)
View(correlacion_conventional)

#calculo de correlaciones de precio y ventas en general
correlacion_ventas_precio <- cor(copia$average_price,copia$total_volume)
View(correlacion_ventas_precio)
#El resultado de correlacion_ventas_precio es -0.1 lo que implica
# una dependencia muy baja entre ambas variables

#Determine la posible relación existente entre dichos precios y su volumen de ventas. 
#Si tomáramos logaritmos, ¿Cómo sería dicha relación?
lm(organicos_volume ~ organicos_price)
# La relacion entre los precios de los aguacates orgánicos y las ventas de estos
# se daría por : -26299*organicos_price + 106136
#Segun esto, por cada que se sube el precio del aguacate orgánico , se bajan
# las ventas en una proporción de 26299.
#Ahora para verlo en porcentajes lo haremos con logaritmos

lm(log(organicos_volume) ~ log(organicos_price))
# Nos da un intercepto de 10.1407 y log(organicos_price): -0.7665
# Si aumentamos 10% el precio del aguacate orgánico , se bajan un 76% las 
# ventas

lm(conventional_volume ~ conventional_price)
# La relacion entre los precios de los aguacates convencionales y las ventas de estos
# se daría por : -2000275*organicos_price + 4162762
#Segun esto, por cada que se sube el precio del aguacate orgánico , se bajan
# las ventas en una proporción de 2000275.
#Ahora para verlo en porcentajes lo haremos con logaritmos

lm(log(conventional_volume) ~ log(conventional_price))
# Nos da un intercepto de 13.42 y log(conventional_price): -1.32
# Si aumentamos 10% el precio del aguacate convencional , se bajan un 132% las 
# ventas

############################################################################
#Otros análisis

# Gráfica dispersión para aguacates conencionales
plot(conventional_price, conventional_volume, 
     main = "Diagrama de dispersión", 
     xlab = "conventional_price", 
     ylab = "conventional_volume")
# Gráfica dispersión para aguacates de los dos tipos
plot(copia$average_price, copia$total_volume, 
     main = "Diagrama de dispersión", 
     xlab = "Price", 
     ylab = "Volume")

#########################
#matriz correlación
matriz_corr <- cor(variables_interes)
print(matriz_corr)
