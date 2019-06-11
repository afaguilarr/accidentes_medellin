#NOTA: hacer el formulario desplegable con los nombres para barrios/comunes usando algo como unique(bar_mes_mod1$Barrio)

files_names= list.files(pattern="*.RData", path = getwd())
#Carga los nombres de los archivos del directorio /Descriptiva y depuracion/Modelos (debe ser el wd)

#Carga los datos con los nombres
for(name in files_names){
  load(name)
}
bar_mes_mod2 <- bar_mes_mod2[bar_mes_mod2$Barrio!= "Inst",]
bar_sem_mod2 <- bar_sem_mod2[bar_sem_mod2$Barrio!= "Inst",]

mypredictions <- function(tipo_temp, tipo_esp, df_predict, nombre){
  #tipo_temp: T = semana, F = mes ; tipo_esp: T = barrio, F = comuna
  #df1 sera el modelo entrenado con 2014-2016 y df2 con 2014-2017
  #df_predict = base de datos  a predecir con todas las fechas (se sumarian los totales)
  #nombre = nombre de barrio  o columna según corresponda.
  
  ##RETORNA:
  #Base de datos con las unidades espaciales (barrio/comuna) y las predicciones
  #para el modelo1 (entrenamiento 2014-2016 y para el modelo 2 (entrenamiento 2014-2017)
  
  if(tipo_temp & tipo_esp){
    df1 <- bar_sem_mod1
    df2 <- bar_sem_mod2
  }
  else if (!tipo_temp & tipo_esp){
    df1 <- bar_mes_mod1
    df2 <- bar_mes_mod2
  }
  else if (!tipo_temp & !tipo_esp){
    df1 <- com_mes_mod1
    df2 <- com_mes_mod2
  }
  else{
    df1 <- com_sem_mod1
    df2 <- com_sem_mod2
  }

  
  mod1 <- df1[df1[,1] == nombre ,4]
  mod2 <- df2[df2[,1] == nombre ,4]
  

  
  predicciones1 <- predict(mod1, df_predict)
  predicciones2 <- predict(mod2, df_predict)
  
  df_predict$predicciones_mod1 <- as.vector(unlist(predicciones1))
  df_predict$predicciones_mod2 <- as.vector(unlist(predicciones2))

  return(df_predict)
  
}

## EJEMPLO PREDICCIONES:

#SEMANAS 5 a la 10 del 2019 para barrios 

df_test <- data.frame(SEMANA = 5:10)
df_test <- mypredictions(tipo_temp = T, tipo_esp= T, df_predict = df_test, "Altavista")


# MESES 4 Y 5 del 2019 para Comunas

df_test <- data.frame(MES = 4:5)
df_test <- mypredictions(tipo_temp = F, tipo_esp= F, df_predict = df_test, "La Candelaria")
