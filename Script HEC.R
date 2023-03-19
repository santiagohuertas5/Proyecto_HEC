rm(list=ls())
require(pacman)
p_load(tidyverse, rio, skimr, janitor, ggplot2, haven, dplyr, stargazer,broom,xlsx) 


#Bases de datos de elecciones a presidente y alcalde 
elec_pres_1986 <- read_dta("1986_presidencia.dta")
elec_pres_1990 <- read_dta("1990_presidencia.dta")
elec_pres_1994 <- read_dta("1994_Presidencia_Segunda_Vuelta.dta")
elec_pres_1998 <- read_dta("1998_Presidencia_Segunda_Vuelta.dta")
elec_pres_2002 <- read_dta("2002_Presidencia.dta")
elec_pres_2006 <- read_dta("2002_Presidencia.dta")
elec_pres_2010 <- read_dta("2010_Presidencia_Primera_Vuelta.dta")
elec_pres_2014 <- read_dta("2014_Presidencia_Segunda_Vuelta.dta")
elec_pres_2018 <- read_dta("2018_Presidencia_Primera_Vuelta.dta")
elec_alca_1988 <- read_dta("1988_alcaldia.dta")
elec_alca_1992 <- read_dta("1992_alcaldia.dta")
elec_alca_1997 <- read_dta("1997_alcaldia.dta")
elec_alca_2000 <- read_dta("2000_alcaldia.dta")
elec_alca_2003 <- read_dta("2003_alcaldia.dta")
elec_alca_2007 <- read_dta("2007_alcaldia.dta")
elec_alca_2011 <- read_dta("2011_alcaldia.dta")
elec_alca_2015 <- read_dta("2015_alcaldia.dta")
elec_alca_2019 <- read_dta("2019_alcaldia.dta")


#Bases de datos de población 
poblacion_1985_1994 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1985-1994.xlsx")
poblacion_1995_2004 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1995-2004.xlsx")
poblacion_2005_2017 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_2005-2017.xlsx")
poblacion_2018_2026 <- read_excel("anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")


#Bases de datos violencia
acciones_belicas <- read_excel("CasosAB_202206.xlsx")
asesinatos_selectivos <- read_excel("CasosAS_202206.xlsx")
danos_bienes_civiles <- read_excel("CasosDB_202206.xlsx")
desaparicion_forzada <- read_excel("CasosDF_202206.xlsx")
masacres <- read_excel("CasosMA_202206.xlsx")
minas <- read_excel("CasosMI_202206.xlsx")
reclutamiento_menores <- read_excel("CasosRU_202206.xlsx")
secuestros <- read_excel("CasosSE_202206.xlsx")
violencia_sexual <- read_excel("CasosVS_202206.xlsx")











