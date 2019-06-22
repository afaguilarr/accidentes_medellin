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
            "PKI", "packrat", "raster", "tidyverse", "fastDummies", "dummies")
  lapply(pkgs, pkg_test_and_load)
}

init_pkgs()


dias_especiales <- read.csv("./dias_espe.csv")
comunas_dias <- read.csv("./comunas_dia.csv")
barrios_dia <- read.csv("./barrios_dia.csv")


dias_especiales$pasted <- paste(dias_especiales$DIA, dias_especiales$MES, 
                                dias_especiales$PERIODO, sep = "/")

comunas_dias$pasted <- paste(comunas_dias$DIA, comunas_dias$MES, 
                                comunas_dias$PERIODO, sep = "/")

barrios_dia$pasted <- paste(barrios_dia$DIA, barrios_dia$MES, 
                                barrios_dia$PERIODO, sep = "/")

comunas_dias$especial <- ifelse(comunas_dias$pasted %in% dias_especiales$pasted, 1, 0)
barrios_dia$especial <- ifelse(barrios_dia$pasted %in% dias_especiales$pasted, 1, 0)


mean(barrios_dia[barrios_dia$especial==1,]$total)
mean(barrios_dia[barrios_dia$especial==0,]$total)
mean(comunas_dias[comunas_dias$especial==1,]$total)
mean(comunas_dias[comunas_dias$especial==0,]$total)

write.csv(x = barrios_dia, file = "barrios_dia_esp.csv")
write.csv(x = comunas_dias, file = "comunas_dias_esp.csv")
