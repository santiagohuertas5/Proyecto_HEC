rm(list=ls())
require(pacman)
p_load(tidyverse, rio, skimr, janitor, ggplot2, haven, dplyr, stargazer,broom,xlsx,readxl, tidyr) 

#Educación

educacion <- read_dta("PANEL_DE_EDUCACION(2021).dta")

educacion <- subset(educacion, select = c(codmpio, ano, s11_total))
anos_faltantes <- expand.grid(codmpio = unique(educacion$codmpio),
                              ano = 1986:2018)

educacion <- merge(anos_faltantes, educacion, by = c("codmpio","ano"), all.x = TRUE)
educacion <- subset(educacion, ano==1986 | ano==1990 | ano==1994 | ano ==1998| ano==2002 |
                      ano==2006|ano==2010|ano==2014|ano==2018 )


educacion$s11_total[is.na(educacion$s11_total)] <- ave(educacion$s11_total, educacion$codmpio,
                                                       FUN = function(x) mean(x, na.rm = TRUE))[is.na(educacion$s11_total)]


rm(anos_faltantes)

#Características geográficas
caracteristicas_geograficas <- read_dta("PANEL_CARACTERISTICAS_GENERALES(2022).dta")




caracteristicas_geograficas<- subset(caracteristicas_geograficas, select = c(codmpio, ano,
                                                                             discapital, disbogota, 
                                                                             areaoficialkm2, altura))
anos_faltantes <- expand.grid(codmpio = unique(caracteristicas_geograficas$codmpio),
                              ano = 1986:2018)
caracteristicas_geograficas <- merge(anos_faltantes, caracteristicas_geograficas,
                                     by = c("codmpio","ano"), all.x = TRUE)



caracteristicas_geograficas <- subset(caracteristicas_geograficas, ano==1986 | ano==1990 | ano==1994 
                                      | ano ==1998| ano==2002 |ano==2006|ano==2010|ano==2014|ano==2018)




caracteristicas_geograficas$discapital[is.na(caracteristicas_geograficas$discapital)] <- ave(caracteristicas_geograficas$discapital,
                                                                                            caracteristicas_geograficas$codmpio,
                                                       FUN = function(x) mean(x, na.rm = TRUE))[is.na(caracteristicas_geograficas$discapital)]



caracteristicas_geograficas$disbogota[is.na(caracteristicas_geograficas$disbogota)] <- ave(caracteristicas_geograficas$disbogota,
                                                                                             caracteristicas_geograficas$codmpio,
                                                                                             FUN = function(x) mean(x, na.rm = TRUE))[is.na(caracteristicas_geograficas$disbogota)]


caracteristicas_geograficas$areaoficialkm2[is.na(caracteristicas_geograficas$areaoficialkm2)] <- ave(caracteristicas_geograficas$areaoficialkm2,
                                                                                           caracteristicas_geograficas$codmpio,
                                                                                           FUN = function(x) mean(x, na.rm = TRUE))[is.na(caracteristicas_geograficas$areaoficialkm2)]



caracteristicas_geograficas$altura[is.na(caracteristicas_geograficas$altura)] <- ave(caracteristicas_geograficas$altura,
                                                                                           caracteristicas_geograficas$codmpio,
                                                                                           FUN = function(x) mean(x, na.rm = TRUE))[is.na(caracteristicas_geograficas$altura)]


controles <- left_join(x=educacion, y=caracteristicas_geograficas, by=c("codmpio","ano"))

rm(anos_faltantes, caracteristicas_geograficas, educacion)

