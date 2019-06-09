setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pkg_test_and_load <- function(pkg)
{
  if(pkg %in% rownames(installed.packages()))
  {
    library(pkg, character.only = TRUE)
  }
  else
  {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

init_pkgs <- function()
{
  pkgs <- c("mvtnorm", "ggplot2", "MBESS", "Matrix", "ks", 
            "caret", "rgdal", "leaflet", "knitr","rmarkdown", 
            "PKI", "packrat", "raster", "tidyverse")
  lapply(pkgs, pkg_test_and_load)
}

init_pkgs()

init_map <- function(data_location, colores=NA) 
{
  barrios_med <- shapefile(data_location, encoding="UTF-8",use_iconv=TRUE)
  nombres_barrios=iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")
  
  m<-leaflet(barrios_med)
  m<-addProviderTiles(m,provider="OpenStreetMap.Mapnik")
  m<-addPolygons(m,popup=barrios_med@data$NOMBRE,weight = 1)
  
  if(is.na(colores))
  {
    #colores<-sample(x=c("orange","green","yellow"),
    #                size=length(barrios_med@data$NOMBRE),replace=TRUE)
    m<-addPolygons(m,popup=barrios_med@data$NOMBRE,color=colores,weight = 1)
    print(m)
  }

}


