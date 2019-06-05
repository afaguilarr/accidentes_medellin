require(gamlss)
satisfaccion.I <- 10 - MS$satisfaccion
histDist(satisfaccion.I,k=2,family = ZISICHEL)
MG1 <- gamlss(satisfaccion.I~edad+regimen+estado_de_salud+
                nivel_educativo+actividad_semana_pasada+arriendo+
                vivienda_propia+seguridad+bienes+estrato+
                ingreso_percapita+personas_hogar,
              data = MS, family = ZISICHEL, k=2)
GAIC(MG1)
wp(MG1)
plot(MG1)

MS_row <- MS[1,2:13] 
satisfaccion2 <- data.frame(satisfaccion2=(10-predict(MG1, newdata = MS, type="response")))

saMS2 = MS
MS2 <- MS2[,1]
MS2 <- cbind(MS2,satisfaccion2)

MS_row_2016 <- MS_2016[1,2:13] 
satisfaccion_2016 <- data.frame(satisfaccion=(10-predict(MG1, newdata = MS, type="response")))

satisfaccion_2016 <- round(satisfaccion_2016)

# boxplot(satisfaccion~estrato,data=MS)
# boxplot(satisfaccion~edad,data=MS)
# boxplot(satisfaccion~regimen,data=MS)
# boxplot(satisfaccion~estado_de_salud,data=MS)
# boxplot(satisfaccion~nivel_educativo,data=MS)
# boxplot(satisfaccion~actividad_semana_pasada,data=MS)
# plot(satisfaccion~log(arriendo),data=MS)
# boxplot(satisfaccion~vivienda_propia,data=MS)
# boxplot(satisfaccion~personas_hogar,data=MS)
# boxplot(satisfaccion~seguridad,data=MS)
# boxplot(satisfaccion~bienes,data=MS)
# plot(satisfaccion~log(ingreso_percapita),data=MS)
# boxplot(MS$satisfaccion)