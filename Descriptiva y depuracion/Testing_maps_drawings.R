setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("maps_scripts.R")

medellin_map_location <- "./Datasets/Barrio_Vereda/Barrio_Vereda.shp"
df_coloring <- read.csv("./Datasets/df_coloring.csv")

barrios_med <- shapefile(medellin_map_location, encoding="UTF-8",use_iconv=TRUE)
nombres_barrios=iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")

b <- barrios_med@data$NOMBRE

matched <- intersect(b, df_coloring$BARRIO)
all <-  union(b, df_coloring$BARRIO)
non.matched <- as.matrix(all[!all %in% matched])

v <- df_coloring[!df_coloring$BARRIO %in% intersect(b, df_coloring$BARRIO),]$BARRIO
v <- as.matrix(v)

v2 <- b[!b %in% intersect(b, df_coloring$BARRIO)]
v2 <- as.matrix(v2)

common <- intersect(v, v2) 

l_colors <- c("purple", "green", "yellow", "red", "blue", "gray", "black")

barrios_med <- barrios_med[barrios_med@data$NOMBRE %in% df_coloring$BARRIO,]
barrios_med <- barrios_med[barrios_med@data$SUBTIPO_BA == "1",]

df_coloring <- df_coloring[order(barrios_med@data$NOMBRE),]
colnames(df_coloring)[1] <- "NOMBRE"

o_db <- barrios_med
nombres_filas <- rownames(o_db@data)

barrios_med@data <- merge(barrios_med@data, df_coloring[, -2:-8], by = 'NOMBRE')
barrios_med@data <- barrios_med@data[order(barrios_med@data$OBJECTID),] 

for(i in 1:max(barrios_med@data$C3G))
{
  barrios_med@data$C3G <- ifelse(barrios_med@data$C3G == i, 
                                 l_colors[i], barrios_med@data$C3G)
}

for(i in 1:max(barrios_med@data$C4G))
{
  barrios_med@data$C4G <- ifelse(barrios_med@data$C4G == i, 
                                 l_colors[i], barrios_med@data$C4G)
}

for(i in 1:max(barrios_med@data$C5G))
{
  barrios_med@data$C5G <- ifelse(barrios_med@data$C5G == i, 
                                 l_colors[i], barrios_med@data$C5G)
}


for(i in 1:max(barrios_med@data$C6G))
{
  barrios_med@data$C6G <- ifelse(barrios_med@data$C6G == i, 
                                 l_colors[i], barrios_med@data$C6G)
}

for(i in 1:max(barrios_med@data$C7G))
{
  barrios_med@data$C7G <- ifelse(barrios_med@data$C7G == i, 
                                 l_colors[i], barrios_med@data$C7G)
}

m<-leaflet(barrios_med)
m<-addProviderTiles(m,provider="OpenStreetMap.Mapnik")
m<-addPolygons(m,popup=o_db@data$NOMBRE,weight = 1)

colores <- barrios_med@data$C4G
m<-addPolygons(m,popup=o_db@data$NOMBRE, color = "black", fillColor=colores,weight = 2, fillOpacity = 0.4)
m

save(m, file="./exports/m.RData")













