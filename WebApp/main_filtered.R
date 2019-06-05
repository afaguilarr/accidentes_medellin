library(visNetwork)
library(dplyr)

## proceso para juntar todas las tablas
carga_bd <- function(){
  
  #Persona
  caracteristicas_y_composicion_hogar <- read.table(file = "txt/Características y composición del hogar.txt", header = T, sep = "\t") %>%
    rename(satisfaccion=P1895 , edad = P6040) %>%
    select(DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, satisfaccion, edad)
  
  salud <- read.table(file = "txt/Salud.txt", header = T, sep = "\t") %>%
    rename(regimen = P6100, estado_de_salud = P6127, edad_primer_hijo = P1708S1,
           pago_salud = P8551) %>%
    select(DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, regimen,
           estado_de_salud, edad_primer_hijo, pago_salud)
  
  educacion <- read.table(file = "txt/Educación.txt", header = T, sep = "\t") %>%
    rename( nivel_educativo = P8587 ) %>%
    select(DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, nivel_educativo)
  
  fuerza_de_trabajo <- read.table(file = "txt/Fuerza de trabajo.txt", header = T, sep = "\t") %>%
    rename(actividad_semana_pasada = P6240, cargo_trabajo = P6435 ) %>%
    select(DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, actividad_semana_pasada, cargo_trabajo)
  
  #Hogar
  servicios_del_hogar <- read.table(file = "txt/Servicios del hogar.txt", header = T, sep = "\t") %>%
    rename(origen_agua = P8530 , ingreso_percapita = PERCAPITA, personas_hogar=CANT_PERSONAS_HOGAR) %>%
    select(DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, origen_agua, ingreso_percapita, personas_hogar)
  
  tenencia_y_financiacion_vivienda <- read.table(file = "txt/Tenencia y financiación de la vivienda que ocupa el hogar.txt", header = T, sep = "\t")
  tenencia_y_financiacion_vivienda$arriendo <- tenencia_y_financiacion_vivienda %>%
    select(P5130, P5140) %>%
    (function (x) {
      x[is.na(x)] <- 0
      return(x)
    }) %>%
    rowSums
  
  tenencia_y_financiacion_vivienda <- tenencia_y_financiacion_vivienda %>%
    rename(valor_vivienda = P5110, vivienda_propia=P5095) %>%
    select(DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, valor_vivienda, arriendo, vivienda_propia)
  
  condiciones_de_vida <- read.table(file = "txt/Condiciones de vida del hogar y tenencia de bienes.txt", header = T, sep = "\t") 
  condiciones_de_vida$bienes <- condiciones_de_vida %>%
    select(P1077S1, P1077S2, P1077S3, P1077S4, P1077S5, P1077S6, P1077S7, P1077S8, P1077S9, P1077S10, P1077S11, P1077S12, P1077S13, P1077S14, P1077S15, P1077S16, P1077S17, P1077S18, P1077S19, P1077S20,P1077S21, P1077S22, P1077S23) %>%
    (function (x) {
      x[x==2] <- 0
      return(x)
    }) %>%
    rowSums
  
  condiciones_de_vida <- condiciones_de_vida %>%
    rename(seguridad = P9010) %>% 
    select(DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, seguridad, bienes)
  
  #vivienda
  viviendas <- read.table(file = "txt/Datos de la vivienda.txt", header = T, sep = "\t") %>%
    rename(SECUENCIA_ENCUESTA_V = SECUENCIA_ENCUESTA ,estrato = P8520S1A1 ) %>% #no se encontro ingreso per capita ni cantidad de personas
    select(DIRECTORIO,SECUENCIA_ENCUESTA_V , estrato)
  
  hogares <- merge(servicios_del_hogar, tenencia_y_financiacion_vivienda)
  hogares <- merge(hogares, condiciones_de_vida) %>%
    rename(SECUENCIA_ENCUESTA_H = SECUENCIA_ENCUESTA )
  
  personas <- merge(caracteristicas_y_composicion_hogar,salud)
  personas <- merge(personas,educacion)
  personas <- merge(personas,fuerza_de_trabajo)
  
  hogares_vivienda <- merge(hogares,viviendas, by.x = c("DIRECTORIO", "SECUENCIA_P"), by.y = c("DIRECTORIO", "SECUENCIA_ENCUESTA_V")) %>%
    select(-SECUENCIA_P)
  
  personas_hogares <- merge(personas, hogares_vivienda, by.x = c("DIRECTORIO", "SECUENCIA_P"), by.y = c("DIRECTORIO", "SECUENCIA_ENCUESTA_H"))
  
  personas_hogares$es_madre_soltera <- logical(nrow(personas_hogares)) #creación de la columna de solteras
  
  
  return(personas_hogares)
}

carga_personas <- function(){
  
bd_familias <- read.table(file = "txt/Características y composición del hogar.txt", header = T, sep = "\t")


#Inicio adecuación del dataframe
bd_familias <- bd_familias %>% #El operador %>% inserta el resultado de la izquierda en la función de la derecha
  rename(sexo = P6020, estado_civil = P5502, relacion_con_jefe = P6051,
         madre = P6083S1, padre = P6081S1, conyuge = P6071S1) %>%
  select(ORDEN, DIRECTORIO,SECUENCIA_ENCUESTA ,SECUENCIA_P, sexo, estado_civil, relacion_con_jefe,
          madre, padre, conyuge)

bd_familias$sexo <- factor(bd_familias$sexo, 
                       levels = c(1,2),
                       labels = c("hombre","mujer"),
                       ordered = FALSE)

bd_familias$estado_civil <- factor(bd_familias$estado_civil,
                               levels = 1:6,
                               labels = c("unionlibre<2","unionlibre>2","viudo","separado","soltero","casado"),
                               ordered = FALSE)

bd_familias$relacion_con_jefe <- factor(bd_familias$relacion_con_jefe,
                                    levels = 1:14,
                                    labels = c("jefe","pareja","hijo(a)","nieto(a)","padre","suegro","hermano(a)","yerno(a)","familiar",
                                               "empleado sd","familiar sd","trabajador","pensionista","otro"),
                                    ordered = FALSE)

bd_familias$soltera <- logical(nrow(bd_familias)) #creación de la columna de solteras

#Finadecuación del dataframe
return(bd_familias)
}

## Funciones
es.madre <- function(p,edges){
  if("hijo" %in% edges[edges$to == p, "label"] ){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
es.soltera <- function(p,edges){
  if("pareja" %in% edges[edges$to == p | edges$from == p, "label"] ){ #si no existe relacion de pareja con otra persona
    return(FALSE)
  }else{
    return(TRUE)
  }
}

#esta funcion recibe un hogar como un dataframe y regresa una lista de dos dataframes:
#nodes: Son las personas y su clasificacion (hombre, mujer o madre soltera)
#edges: Son las relaciones entre las personas
procesar_hogar <- function(hogar){ 


## inicializacion del grafo
from <- numeric(0)
to <- numeric(0)
relacion <- numeric()
level <- numeric(0)

# Crea el grafo, se puede vectorizar
for (p in 1:nrow(hogar)) { # para cada persona en el hogar
  persona <- hogar[p,]
  
  #relacion con el jefe
  relacion <- c(relacion, persona$relacion_con_jefe)
  from <- c(from, p)
  to <- c(to,1)
  if(persona$relacion_con_jefe %in% c("jefe","pareja","hermano(a)") ){
    level <- c(level, 0)
  }else if( persona$relacion_con_jefe %in% c("hijo(a)","yerno(a)")){
    level <- c(level,1)
  }else if( persona$relacion_con_jefe == "nieto(a)"){
    level <- c(level,2)
  }else if( persona$relacion_con_jefe %in% c("padre","suegro") ){
    level <- c(level,-1)
  }else{
    level <- c(level,3)
  }
  
  #otras relaciones
  if(!is.na(persona$madre)){
    if(persona$madre != 1){ #si su madre es el jefe de hogar, esta relacion ya existe y no se repite
      relacion <- c(relacion, 3)
      from <- c(from, p)
      to <- c(to,persona$madre)
    }
  }
  
  if(!is.na(persona$padre)){
    if(persona$padre != 1){
      relacion <- c(relacion, 3)
      from <- c(from, p)
      to <- c(to,persona$padre)
    }
  }
  
  if(!is.na(persona$conyuge)){
    if(persona$conyuge != 1 && !any(relacion[from == p | to == p] != 2) ){ #si ya hay relacion de pareja, no la repite
      relacion <- c(relacion, 2)
      from <- c(from, p)
      to <- c(to,persona$conyuge)
    }
  }
  
}


relacion <- factor(relacion,
                  levels = 1:14,
                  labels = c("jefe","pareja","hijo","nieto","padre","suegro","hermano","yerno","familiar",
                             "empleado sd","familiar sd","trabajador","pensionista","otro"),
                  ordered = FALSE)


#se arman los nodes y los edges con la informacion obtenida
nodes <- data.frame(id = hogar$ORDEN,
                    group = hogar$sexo,
                    label = hogar$ORDEN,
                    level = level
                    )

edges <- data.frame(from = from, to = to,
                    label = relacion %>% as.character,
                    arrows = "to"
                    )

# Esto encuentra el id las madres solteras segun la definición elegida
id_solteras_hogar <- numeric()
seq_solteras_hogar <- numeric()

for (p in 1:nrow(hogar)) {
  persona <- hogar[p,]
  
  if(persona$sexo == "mujer" && #es mujer
     es.madre(p,edges) &&       #es madre
     es.soltera(p,edges) &&     #es soltera
     persona$ORDEN == 1){       #es jefe de hogar
      id_solteras_hogar <- c(id_solteras_hogar, persona$ORDEN)
      seq_solteras_hogar <- c(seq_solteras_hogar, persona$SECUENCIA_ENCUESTA)
  }
}

#Creacion de un grupo de madres solteras para que se coloreen distino en el grafico
levels(nodes$group) <- c("hombre","mujer","madre soltera")
nodes$group[id_solteras_hogar] <- "madre soltera"

#agrega las solteras encontradas en la bd principal
bd_estudio$es_madre_soltera[bd_estudio$DIRECTORIO == hogar$DIRECTORIO[1] &
                              bd_estudio$SECUENCIA_P == hogar$SECUENCIA_P[1] &
                              bd_estudio$SECUENCIA_ENCUESTA %in% seq_solteras_hogar] <<- TRUE

bd_familias$soltera[bd_familias$DIRECTORIO == hogar$DIRECTORIO[1] &
                      bd_familias$SECUENCIA_P == hogar$SECUENCIA_P[1] &
                      bd_familias$SECUENCIA_ENCUESTA %in% seq_solteras_hogar] <<- TRUE


return(list(nodes = nodes,edges = edges))

} #Fin función procesar_hogar


procesar_todos_hogares <- function(){

  hogares <- split(bd_familias, #split parte la bd en una lista de hogares a partir de su identificador unico
                   list(bd_familias$DIRECTORIO, bd_familias$SECUENCIA_P), #identificador unico del hogar segun el DANE
                   drop = TRUE) #no incluye hogares sin personas  

  # Encuentra las madres solteras en todos los hogares
  i<-0
  for(hogar in hogares){
    if(i%%1000==0){
      print(paste( round(i/length(hogares)*100),"%") ) #imprime el porcentaje de avance
    }
    
    i<-i+1
    res <- procesar_hogar(hogar)
  }
  return(hogares)
}

# Grafica de el un hogar, puede ser por el numero de orden o por la clave unica (directorio.secuencia_p)
grafica_hogar_de_persona <- function(idhogar){
  
  res <- procesar_hogar(hogares[[idhogar]])
  
  # visNetwork(res$nodes, res$edges, width = "100%") %>% #crea el grafico
  #   visPhysics(solver = "repulsion", repulsion = list(nodeDistance = 200)) %>% #fisicas para que no queden muy juntos
  #   visLegend() #muestra la leyenda para los grupos coloreados
  visNetwork(res$nodes, res$edges, width = "100%") %>% 
    visHierarchicalLayout() %>%
    visLegend()
}

## Inicio programa

bd_estudio <- carga_bd()

bd_familias <- carga_personas()

hogares <- procesar_todos_hogares()

madres_solteras = bd_estudio[bd_estudio$es_madre_soltera,] #saca el subconjunto de la bd principal pero con madres solteras

#Filtrado adicional de la BD final:
madres_solteras_f1 = madres_solteras[madres_solteras$estrato!=9,]
madres_solteras_f1 = madres_solteras_f1[madres_solteras_f1$estrato!=8,]
madres_solteras_f1 = madres_solteras_f1[madres_solteras_f1$estrato!=0,]
madres_solteras_f2 = madres_solteras_f1[madres_solteras_f1$regimen!=9,]
madres_solteras_f3 = madres_solteras_f2[madres_solteras_f2$actividad_semana_pasada!=6,]
madres_solteras_f4 = madres_solteras_f3[madres_solteras_f3$ingreso_percapita!=0,]
madres_solteras_f5 = madres_solteras_f4[madres_solteras_f4$arriendo!=99,]

MS_filter = madres_solteras_f5

#Conversion de variables discretas a categoricas:
MS_filter$actividad_semana_pasada <- factor(MS_filter$actividad_semana_pasada,
                   levels = 1:5,
                   labels = c("Trabajando","Buscando trabajo","Estudiando"
                              ,"Oficios del hogar","Incapacitado"),
                   ordered = FALSE)


MS_filter$vivienda_propia <- factor(MS_filter$vivienda_propia,
                                    levels = 1:5,
                                    labels = c("Propia","Propia"
                                               ,"Arriendo","Usufructuario",
                                               "Sin titulo/colectiva"),
                                    ordered = FALSE)

MS_filter$seguridad <- factor(MS_filter$seguridad,
                              levels = 1:2,
                              labels = c("Segura","Insegura"),
                              ordered = FALSE)

MS_filter$regimen <- factor(MS_filter$regimen,
                            levels = 1:3,
                            labels = c("Contributivo","Especial","Subsidiado"),
                            ordered = FALSE)

grafica_hogar_de_persona("6000013.1")

# SELECCIONANDO VARIABLES IMPORTANTES
MS <- MS_filter %>% select(satisfaccion, edad, regimen, estado_de_salud,
                           nivel_educativo, actividad_semana_pasada,
                           arriendo, vivienda_propia, seguridad,
                           bienes, estrato, ingreso_percapita, personas_hogar)
MS <- na.omit(MS)

#Renombramiento de variables para mostrar en version WEB
MS_WEB = MS

MS_WEB$estado_de_salud <- factor(MS_WEB$estado_de_salud,
                            levels = 1:4,
                            labels = c("Muy bueno","Bueno","Regular","Malo"),
                            ordered = TRUE)

MS_WEB$nivel_educativo <- factor(MS_WEB$nivel_educativo,
                         levels = 1:13,
                         labels = c("Ninguno","Preescolar","Primaria(1-5)","Secundaria(6-9)",
                                    "Media(10-13)","Tecnico sin titulo", "Tecnico con titulo",
                                    "Teconologico sin titulo", "Tecnologico con titulo",
                                    "Universitario sin titulo", "Universitario con titulo",
                                    "Postgrado sin titulo", "Postgrado con titulo"),
                         ordered = TRUE)

MS_WEB[, "arriendo"] <- as.integer(MS_WEB[, "arriendo"])
MS_WEB[, "arriendo"] <- prettyNum(MS_WEB[, "arriendo"], scientific=FALSE, big.mark=".")
MS_WEB[, "arriendo"] <- paste("$", MS_WEB[, "arriendo"])

MS_WEB[, "ingreso_percapita"] <- as.integer(MS_WEB[, "ingreso_percapita"])
MS_WEB[, "ingreso_percapita"] <- prettyNum(MS_WEB[, "ingreso_percapita"], scientific=FALSE, big.mark=".")
MS_WEB[, "ingreso_percapita"] <- paste("$", MS_WEB[, "ingreso_percapita"])

write.table(bd_estudio, "txt/bd_estudio.txt", sep="\t")
write.table(bd_familias, "txt/bd_familias.txt", sep="\t")
write.table(MS_WEB, "txt/madres_solteras.txt", sep="\t")
save(hogares, file="txt/hogares.RData")


